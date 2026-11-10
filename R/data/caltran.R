library(fs)
library(here)
library(sf)
library(terra)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils.R"))
config <- readYaml("data")
ca_crs <- st_crs(config$crs)
ca_mld <- readFile("cal_mld.gpkg")
url_ca_road <- config$caltrans$url
destination_dir <- dir_create(path(here(),"raw",  "preprocessed"))
#-------------------------------------------------------------------------------
roads <- st_read(url_ca_road)
roads <- st_transform(roads, cal_crs)
roads <- st_intersection(roads, ca)

write_sf(roads, path(destination_dir, "roads.gpkg"), overwrite = TRUE)
