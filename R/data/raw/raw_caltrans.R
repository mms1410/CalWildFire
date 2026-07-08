library(fs)
library(here)
library(sf)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils", "data_queries.R"))
destination_dir <- path(here(), "data", "raw")
dir_create(destination_dir)
conf <- get_conf()
crs <- st_crs(read_conf(conf, "crs"))
#-------------------------------------------------------------------------------
url <- read_conf(conf, "url_ca_road")
#url <- "https://caltrans-gis.dot.ca.gov/arcgis/rest/services/CHhighway/All_Roads/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
roads <- st_read(url)
roads <- data_source <- arc_open(url)
ca <- read_geodata(keyword = "ca_state", sf_crs = crs)
ca_roads <- st_intersection(roads, ca)
st_write(roads, path(destination_dir, "caltrans.gpkg"))



