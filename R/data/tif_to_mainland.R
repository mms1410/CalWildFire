library(fs)
library(terra)
#-------------------------------------------------------------------------------
source("R/utils.R")
mask_mainland <- readFile(path(getwd(), "data", "assets", "cal_mld.gpkg"))
destination_dir <- path(getwd(), "data", "preprocessed")
tmp_dir <- path(here(), "tmp_terra") # /tmp too small for raster data
#-------------------------------------------------------------------------------
tif_files <- dir_ls(path(getwd(), "data", "raw"), glob = "*.tif")
for (tif_file in tif_files) {
 variable_name <- basename(tif_file)
 raster <- terra::rast(tif_file)
 cat(paste0("Process '", variable_name, "'...\n"))
 
 dir_create(tmp_dir)
 terraOptions(tempdir = tmp_dir)
 raster <- cropMainland(raster, mask_mainland)
 writeRaster(raster, path(destination_dir, variable_name), overwrite = TRUE)
 
 dir_delete(tmp_dir)
 rm(raster);gc()
 terra::tmpFiles(current = TRUE, orphan = TRUE, remove = TRUE)
}

sf_files <- dir_ls(path(here(), "data", "raw"), glob = "*.gpkg")
for (sf_file in sf_files) {
  variable_name <- basename(sf_file)
  cat(paste0("Process '", variable_name, "'...\n"))
  
  gpkg <- sf::read_sf(sf_file)
  gpkg <- cropMainland(gpkg, mask_mainland)
  st_write(gpkg, path(destination_dir, variable_name), append = FALSE)
}