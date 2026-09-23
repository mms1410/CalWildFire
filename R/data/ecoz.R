library(sf)
library(dplyr)
#-------------------------------------------------------------------------------
source("R/utils.R")
config <- readYaml("data")
cal_crs <- st_crs(config$crs)
ecoz_url <- config$ecoz$url
destination_dir <- dir_create(path(getwd(), "data"))
tmp <- tempfile(fileext = ".zip")
#-------------------------------------------------------------------------------
download.file(ecoz_url, tmp, mode = "wb")
ecoz <- st_read(paste0("/vsizip/", tmp)) |>
  transmute(region = US_L3NAME) |>
  st_transform(cal_crs)
st_write(ecoz, path(destination_dir, "ecoz.gpkg"))
