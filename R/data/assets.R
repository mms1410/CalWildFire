library(esri2sf)
library(fs)
library(sf)
library(dplry)
library(USAboundaries)
#-------------------------------------------------------------------------------
source("R/const.R")
source("R/utils.R")
config_mesh <- readYaml("mesh")
config_data <- readYaml("data")
config_assets <- readYaml("assets")
path_assets_data <- dir_create(path(getwd(), "data", "assets"))
#-------------------------------------------------------------------------------
california <- USAboundaries::us_states(states = "California", resolution = "high") |> 
  select(geometry) |>
  st_transform(config_data$crs) |>
  st_as_sf()
st_write(california, path(path_assets_data, "cal.gpkg"),
         append = FALSE)

ca_mld <- california |>
  st_cast("POLYGON", warn = FALSE) |>
  mutate(area = st_area(geometry)) |>
  slice_max(area, n = 1) |>
  select(geometry)
st_write(california_mainland, path(path_assets_data, "cal_mld.gpkg"),
         append = FALSE)


mesh_boundary_ca <- st_simplify(st_union(ca_mld), dTolerance = config_mesh$dtolerance)
st_write(mesh_boundary_ca, path(path_assets_data, "mesh_boundary_ca.gpkg")) 

coastline <- esri2sf(config_assets$url_coastline) |>
  pull(geoms)
st_write(coastline, path(path_assets_data, "coastline.gpkg"))  