library(here)
library(fs)
library(tidyverse)
library(sf)
library(terra)
#-------------------------------------------------------------------------------
source("R/utils/functions.R")
source("R/utils/data_queries.R")
conf <- get_conf()
crs <- st_crs(read_conf(conf, "crs"))
modis_conf <- read_conf(conf, "appeears")
modis_variables <- names(modis_conf)
modis_folders <- sapply(modis_variables, function(x) path(here(), "data", "raw", x))
modis_layers <- sapply(modis_variables, function(x) modis_conf[[x]][["layer"]])
dir_dest <- path(here(), "data", "preprocessed")
dir_create(dir_dest)
#-------------------------------------------------------------------------------
for (variable in modis_variables) {
  folder <- modis_folders[[variable]]
  if (!dir_exists(folder)) {
    message(paste0("Folder '", folder, "' does not exist (skip)"))
    next
  }
  message(paste0("Process folder/variable '", variable), "'...")
  layers <- modis_layers[[variable]]
  for (layer in layers) {
    message(paste0("    Process layer ", layer))
    pattern <- paste0("*", layer, "*.tif$")
    files <- dir_ls(folder, glob = pattern)
    checkmate::assertFile(files)
    dates <- parse_string(files, parserlist = parsers_modis)
    yrs <- unique(year(dates))
    filenames_yrs <- path(dir_dest, paste0(variable, "_", layer, "_", yrs, ".tif"))
    for (idx in seq_along(yrs)) {
      message(paste0("    Process year ", yrs[idx], "..."))
      filenames_chunk <- files[year(dates) == yrs[idx]]
      dates_chunk <- dates[year(dates) == yrs[idx]]
      raster_yr <- rast(filenames_chunk)
      terra::time(raster_yr) <- dates_chunk
      names(raster_yr) <- dates_chunk
      raster_yr <- terra::project(raster_yr, crs$wkt)
      writeRaster(raster_yr, filenames_yrs[idx], overwrite = TRUE)
      }
    message("    Merge yearly data...")
    raster_total <- rast(filenames_yrs)
    filename_total <- path(dir_dest, paste0(variable, "_", layer, ".tif"))
    writeRaster(raster_total,
                filename = filename_total,
                overwrite = TRUE)
    sapply(filenames_yrs, function(file) file_delete(file))
    
  }
}