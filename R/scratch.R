library(ggplot)
library(tidyterra)
library(sf)
library(terra)
library(patchwork)
#-------------------------------------------------------------------------------
source("R/utils/data_queries.R")
conf <- get_conf()
crs <- st_crs(read_conf(conf, "crs"))
road_network <- read_geodata(keyword = "road_network", sf_crs = crs)
road_raster <- read_geodata(keyword = "tif", sf_crs = crs, filename = "road")


plt_road_network <- ggplot() +
  geom_sf(data = read_geodata(keyword = "road_network", sf_crs = crs), linewidth = 0.1, alpha = 0.5) +
  labs(x = "Longitude", y = "Latitude")

plt_road_raster <- ggplot() +
  geom_spatraster(data = read_geodata(keyword = "tif", sf_crs = crs, filename = "road")) +
  scale_fill_viridis_c(na.value = NA, name = "Road density", guide = "colorbar") +
  labs(x = "Longitude", y = "Latitude")

plt_road_raster
plt_road_network



