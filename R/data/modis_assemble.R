library(here)
library(fs)
library(tidyverse)
library(sf)
library(terra)
#-------------------------------------------------------------------------------
source("R//utils.R")
source("R/const.R")
modis_conf <- CONF$modis$appeears
modis_items <- names(modis_conf)
modis_folders <- sapply(modis_items, function(x) path(here(), "data", "raw", x))
modis_layers <- sapply(modis_items, function(x) modis_conf[[x]][["layer"]])
destination_dir <- path(here(), "data")
dir_create(destination_dir)
#-------------------------------------------------------------------------------
for (item in modis_items) {
  folder <- modis_folders[[item]]
  if(!dir_exists(folder)) {
    warning(paste0("Expected to find folder at ", folder, " for item/variable ", item, " but does not exists."))
    next
  }
  cat(paste0("Process folder/item '", item), "'...\n")
  layers <- modis_layers[[item]]
  
  for (layer in layers) { 
    cat(paste0("  Process layer ", layer, "\n"))
    pattern <- paste0("*", layer, "*.tif$")
    files <- dir_ls(folder, glob = pattern, recurse = TRUE)
    checkmate::assertFile(files)
    dates <- str_extract(files, "(?<=doy)(\\d{4})(\\d{3})")
    dates <- lubridate::as_date(dates, format = "%Y%j")
    
    cat("    Read tif files....\n")
    raster <- terra::rast(files)
    names(raster) <- dates
    terra::time(raster) <- dates
    
    cat("    Project crs....\n")
    if(crs(raster) != CRS$wkt) 
      raster <- project(raster, CRS$wkt)
    writeRaster(raster, path(destination_dir, paste0(item, ".tif")), overwrite = TRUE)
    
    if (grepl("dem", item, ignore.case = TRUE)) {
      cat("    Compute slope and aspect...\n")
      slope <- terra::terrain(raster, v = "slope", unit = "degrees") # unit for output
      writeRaster(slope, path(destination_dir, "slope.tif"), overwrite = TRUE)
      rm(slope);gc()
      aspect <- terra::terrain(raster, v = "aspect", unit = "degrees")
      writeRaster(aspect, path(destination_dir, "aspect.tif"), overwrite = TRUE)
      rm(aspect); gc()
    }
    rm(raster);gc()
  }
}