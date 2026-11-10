library(fs)
library(lubridate)
library(here)
library(readr)
library(dplyr)
library(sf)
library(data.table)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils", "utils_io.R"))
source(path(here(), "R", "utils", "utils_geocomp.R"))
#-------------------------------------------------------------------------------
general <- readYaml("general")
paths <- readYaml("paths")
config <- readYaml("data")

ecoz <- readFile("ecoz_mld.gpkg")
ca_mld <- readFile("ca_mld.gpkg")
  
destination_dir <- reduceYamlPath(paths$dir_prep)

stations <- readFile("stations.gpkg")
stations <- stations |>
  st_intersection(ecoz) |>
  st_intersection(st_buffer(ca_mld, dist = -5000))
variables <- c("ppt", "vpdmax", "vs", "tmax")
#-------------------------------------------------------------------------------
message("=============== Query raster data at station locations ==============")

for (variable in variables) {
  
  message(sprintf("Query variable %s...", variable))
  
  raster <- readFile(paste0(variable, ".tif"))
  checkmate::assert(st_crs(raster) == st_crs(general$crs))
  
  values_wide <- as.data.table(terra::extract(raster, stations, ID = FALSE, xy = TRUE))
  values_long <- melt(values_wide,
                      id.vars = c("x","y"),
                      variable.name = "date",
                      value.name = "value")
  values_long[, date := as.Date(date)]
  fwrite(values_long, path(destination_dir, paste0(variable, "_stations.csv")))
}

#library(ggplot2)
# dtbl <- readFile("tmax_stations.csv")
# dtbl <- unique(dtbl, by = c("x", "y"))
# sf_frame <- st_as_sf(dtbl, coords = c("x", "y"), crs = st_crs(3310))
# stWithinLogical(sf_frame, ecoz) |> sum()
# library(ggplot2)
# ggplot() +
#   geom_sf(data = readFile("ca_mld.gpkg"), fill = NA, color = "red") +
#   geom_sf(data = ecoz, aes(fill = region), alpha = 0.3) +
#   geom_sf(data = filter(sf_frame, !stWithinLogical(geometry, ecoz)), size  = 0.4)

          