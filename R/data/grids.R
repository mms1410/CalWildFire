library(terra)
library(fs)
library(lubridate)
library(data.table)
library(sf)
#-------------------------------------------------------------------------------
source(path("R", "utils", "utils_io.R"))
source(path("R", "utils", "utils_geocomp.R"))
#-------------------------------------------------------------------------------
conf <- readYaml("grid")
paths <- readYaml("paths")
general <- readYaml("general")

destination_dir <- dir_create(reduceYamlPath(paths$dir_prep))
aux_dir <- dir_create(reduceYamlPath(paths$dir_aux))
tif_dir <- reduceYamlPath(paths$dir_prep)

spat_res <- conf$spat_res_km
temp_res <- conf$temp_res_d
crs_ca <- st_crs(conf$crs)

ca_mld <- readFile("ca_mld.gpkg")
ecoz_mld <- readFile("ecoz_mld.gpkg")
resolution_spec <- paste0(spat_res, "km", temp_res, "d")
tif_files <- dir_ls(tif_dir, glob = "*.tif")

grid_pts <- st_make_grid(st_buffer(ca_mld, dist = conf$buffer_distance), cellsize = conf$spat_res * 1000, square = TRUE) |>
  st_centroid() |>
  st_intersection(ca_mld) |>
  st_intersection(ecoz_mld) |>
  st_as_sf()
st_write(grid_pts, path(aux_dir, paste0("ca_grid_", resolution_spec, ".gpkg")), append = FALSE)
#-------------------------------------------------------------------------------
message("======= Creating tables at %s km spatial resolution with %s-day time steps =======", spat_res, temp_res)

for (tif_file in tif_files) {
  
  variable_name <- path_ext_remove(basename(tif_file))
  raster <- terra::rast(tif_file)
  single_layer <- (nlyr(raster) == 1)
  checkmate::assert(crs(raster) == st_crs(grid_pts)$wkt)
  filepath <- path(destination_dir, paste0(variable_name, "_" , resolution_spec, ".csv"))
  
  message(sprintf("Query  grid locations of variable %s...", variable_name))

  tbl_wide <- as.data.table(terra::extract(raster, vect(grid_pts), ID = FALSE, xy = TRUE))
  if (single_layer) {
    
    colnames(tbl_wide)[1] <- "value"
    fwrite(tbl_wide, filepath)
    
  } else {
    
    tbl_long <- tbl_wide |>
      melt(id.vars = c("x","y"),
           variable.name = "date",
           value.name = "value")
    tbl_long[,date := as.Date(date)]
    fwrite(tbl_long, filepath)
    
  }
}