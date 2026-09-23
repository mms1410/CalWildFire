library(fs)
library(here)
library(sf)
library(terra)
#-------------------------------------------------------------------------------
source("R/const.R")
config <- readYaml("data")
ca_crs <- st_crs(config$crs)
url_ca_road <- config$caltrans$url
destination_dir <- dir_create(path(here(),"raw",  "data"))
#-------------------------------------------------------------------------------
roads <- st_read(url_ca_road)
roads <- st_transform(roads, CRS)
roads <- st_intersection(roads, CA)

roads <- st_cast(roads, "MULTILINESTRING") # cast linestring to multilinestring to have only one geom
roads_vect <- vect(roads)
ca_vect <- vect(CA)

grid <- rast(ca_vect, res = spat_res)
rast_length <- rasterizeGeom(roads_vect, grid, fun = "length") # Length per cell (in metres)
cell_area_km2 <- prod(res(grid)) / 1e6 # unit [m] assumed
rast_road_density <- rast_length / cell_area_km2
rast_road_density <- mask(rast_road_density, ca_vect)

# grid <- rast(ext(ca_vect), res = spat_res, crs = CRS$wkt)
# rast_road_density <- rasterizeGeom(roads_vect, grid, fun = "length")
# cell_area_km2 <- prod(res(grid)) / 1e6
# rast_road_density <- (rast_road_density) / cell_area_km2
# rast_road_density <- mask(rast_road_density, ca_vect)

rast_log <- log1p(rast_road_density)
min_val <- global(rast_log, "min", na.rm = TRUE)[1,1]
max_val <- global(rast_log, "max", na.rm = TRUE)[1,1]
rast_normalized <- (rast_log - min_val) / (max_val - min_val)

write_sf(roads, path(destination_dir, "roads.gpkg"), overwrite = TRUE)
writeRaster(rast_road_density, path(destination_dir, "road_density.tif") ,overwrite = TRUE)
