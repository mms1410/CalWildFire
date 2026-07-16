library(ggplot2)
library(tidyterra)
library(sf)
library(terra)
#-------------------------------------------------------------------------------
source("R/utils/data_queries.R")
conf <- get_conf()
crs <- st_crs(read_conf(conf, "crs"))
ppt <- read_geodata(keyword = "tif", sf_crs = crs, filename = "ppt")

ppt_months <- agg_raster(ppt, split(names(ppt),lubridate::month(names(ppt), label = TRUE)))

ggplot() +
  geom_spatraster(data = ppt_months) +
  facet_wrap(~lyr)
