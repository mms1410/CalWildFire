library(sf)
library(dplyr)
#-------------------------------------------------------------------------------
source("R/utils.R")
config <- readYaml("data")
cal_crs <- st_crs(config$crs)
ecoz_url <- config$ecoz$url
destination_dir <- dir_create(path("data", "assets"))
tmp <- tempfile(fileext = ".zip")
#-------------------------------------------------------------------------------
ecoz <- getGeoZip(ecoz_url, cal_crs) |>
  mutate(ecoz = US_L3NAME,
         ecoz_code = US_L3CODE) |>
  select(ecoz, ecoz_code, geometry)

st_write(ecoz, path(destination_dir, "ecoz.gpkg"))