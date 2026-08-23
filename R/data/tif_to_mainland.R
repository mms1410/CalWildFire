library(fs)
library(here)
library(terra)
#-------------------------------------------------------------------------------
source("R/utils.R")
source("R/const.R")
#-------------------------------------------------------------------------------
tif_files <- dir_ls(path(here(), "data"), glob = "*.tif")
for (tif_file in tif_files) {
 tif_name <- basename(tif_file)
 raster <-terra::rast(tif_file)
 raster <- cropMainland(raster)
 filename <- sub(".tif", "_mainland.tif", basename(tif_file))
 terra::writeRaster(raster, path(dirname(tif_file), filename), overwrite = TRUE)
 rm(raster);gc()
 terra::tmpFiles(current = TRUE, orphan = TRUE, remove = TRUE)
}
