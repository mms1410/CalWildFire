library(sf)
library(dplyr)
#-------------------------------------------------------------------------------
source("R/const.R")
destination_dir <- path(here(), "data")
dir_create(destination_dir)
url <- CONF$ecoz$url
tmp <- tempfile(fileext = ".zip")
#-------------------------------------------------------------------------------
download.file(url, tmp, mode = "wb")
ecoz <- st_read(paste0("/vsizip/", tmp)) |>
  transmute(region = US_L3NAME) |>
  st_transform(CRS)
st_write(ecoz,path(destination_dir, "ecoz.gpkg"))
