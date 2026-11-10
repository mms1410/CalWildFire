# note: ecoz and mld are from differnt sources and small precision errors
# especially at coast region can lead to slightly different geometry
# therefore filtered for mld and ecoz_mld
library(esri2sf)
library(fs)
library(sf)
library(dplyr)
library(here)
library(readr)
library(spdep)
library(USAboundaries)
library(rnaturalearth)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils", "utils_io.R"))
source(path(here(), "R", "utils", "utils_geocomp.R"))
#-------------------------------------------------------------------------------
set.seed(123)
config_mesh <- readYaml("mesh")
config_data <- readYaml("data")
ecoz_url <- config_data$ecoz$url
coastline_url <- config_data$coastline$url
ca_crs <- st_crs(config_data$crs)
destination_dir <- dir_create(path(here(), "data", "auxiliary"))
#-------------------------------------------------------------------------------
getState <- function(state_name) {
  return(USAboundaries::us_states(states = state_name, resolution = "high") |>
    select(geometry) |>
    st_transform(ca_crs) |>
    st_as_sf())
}

ca <- getState("California")
ca_mld <- ca |>
  cropToLargestPoly()
st_write(ca_mld, path(destination_dir, "ca_mld.gpkg"), append = FALSE)

neigh <- rbind(cropToLargestPoly(getState("Nevada")),
               cropToLargestPoly(getState("Arizona")),
               cropToLargestPoly(getState("Oregon")),
               ne_countries(country = "Mexico", returnclass = "sf") |>
                 select(geometry) |>
                 st_transform(ca_crs))
st_write(neigh, path(destination_dir, "ca_ne.gpkg"), append = FALSE)


ecoz_mld <- readGeoZip(ecoz_url, ca_crs) |>
  mutate(region = US_L3NAME,
         region_code = US_L3CODE) |>
  select(region, region_code, geometry) |>
  st_intersection(ca_mld)

st_write(ecoz_mld, path(destination_dir, "ecoz_mld.gpkg"), append = FALSE)

spdep::nb2INLA(path(destination_dir, "ecoz_mld.adj"), spdep::poly2nb(ecoz_mld, queen = TRUE))

coastline <- esri2sf(coastline_url) |>
  pull(geoms) |>
  st_transform(ca_crs)
st_write(coastline, path(destination_dir, "coastline.gpkg"), append = FALSE)


stations <- readr::read_fwf("https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt",
                     col_positions = fwf_positions(start = c(1,  13, 22, 32, 39, 42, 73, 77, 81),
                                                   end =   c(11, 20, 30, 37, 40, 71, 75, 79, 85),
                                                   col_names = c("id", "latitude", "longitude", "elevation", "state", "name", "gsn_flag", "hcn_crn_flag", "wmo_id")))
stations <- stations |>
  dplyr::filter(state == "CA") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(st_crs(ca_mld)) |>
  st_filter(ca_mld, .predicate = st_intersects) |>
  st_filter(ecoz_mld, .predicate = st_intersects)

st_write(stations, path(destination_dir, "ca_stations.gpkg"), append = FALSE)

grid <- st_make_grid(stations, n = c(40, 40), what = "polygons") |>
  st_sf() |>
  mutate(grid_id = row_number())

stations_in_grid <- st_join(stations, grid, join = st_intersects)

stations_small <- stations_in_grid |>
  filter(!is.na(grid_id)) |>
  group_by(grid_id) |>
  slice_sample(n = 1) |>
  ungroup() |>
  select(-grid_id)
plot(stations_small)
st_write(stations_small, path(destination_dir, "ca_stations_small.gpkg"), append = FALSE)