library(fs)
library(here)
library(sf)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils", "data_queries.R"))
destination_dir <- path(here(), "data", "raw")
dir_create(destination_dir)
config <- read_yaml()
crs <- st_crs(config[["crs"]])
url_ca_road <- config[["caltrans"]][["url"]]
#-------------------------------------------------------------------------------
roads <- st_read(url_ca_road)
ca <- read_geodata(keyword = "ca_state", sf_crs = crs)
ca_roads <- st_intersection(roads, ca)
st_write(roads, path(destination_dir, "caltrans.gpkg"))