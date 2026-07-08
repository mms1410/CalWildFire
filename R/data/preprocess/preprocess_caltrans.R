library(fs)
library(here)
library(sf)
library(terra)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils", "data_queries.R"))
destination_dir <- path(here(), "data", "preprocessed")
source_dir <- path(here(), "data", "raw", "caltrans.gpkg")
dir_create(destination_dir)
conf <- get_conf()
crs <- st_crs(read_conf(conf, "crs"))
#-------------------------------------------------------------------------------
roads <- st_read(source_dir)
roads <- st_transform(roads, crs)

# data contains linestrings and multilinestring but terra expects one type only
roads <- st_cast(roads, "MULTILINESTRING")
if (st_crs(roads)$IsGeographic) stop("Error: The CRS is geographic (degrees). Need a projected CRS that uses meters (e.g., California Albers or UTM).")
roads_vect <- vect(roads)
ca <- read_geodata(keyword = "ca_state", sf_crs = crs)
ca_vect <- vect(ca)
grid <- rast(ext(ca_vect), res = 2000, crs = crs$wkt)
rast_road_density <- rasterizeGeom(roads_vect, grid, fun = "length")
cell_area_km2 <- prod(res(grid)) / 1e6
rast_road_density <- (rast_road_density) / cell_area_km2
rast_road_density <- mask(rast_road_density, ca_vect)
plot(rast_road_density)
hist(rast_road_density)
rast_log <- log1p(rast_road_density)
min_val <- global(rast_log, "min", na.rm = TRUE)[1,1]
max_val <- global(rast_log, "max", na.rm = TRUE)[1,1]
rast_normalized <- (rast_log - min_val) / (max_val - min_val)
plot(rast_normalized)
hist(values(rast_normalized))

writeRaster(rast_road_density,
            filename = path(destination_dir, "road_density.tif"),
            overwrite = TRUE)
