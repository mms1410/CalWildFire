library(here)
library(fs)
library(tidyverse)
library(sf)
library(terra)
#-------------------------------------------------------------------------------
source("R/data/preprocess/utils.R")
source("R/utils/data_queries.R")
conf <- get_conf()
crs <- st_crs(read_conf(conf, "crs"))
modis_conf <- read_conf(conf, "appeears")
modis_variables <- names(modis_conf)
modis_folders <- sapply(modis_variables, function(x) path(here(), "data", "raw", x))
modis_layers <- sapply(modis_variables, function(x) modis_conf[[x]][["layer"]])
dir_dest <- path(here(), "data", "preprocessed")
dir_create(dir_dest)
categorical <- FALSE
#-------------------------------------------------------------------------------
for (variable in modis_variables) {
  folder <- modis_folders[[variable]]
  categorical <- ifelse(variable == "landcover", TRUE, FALSE)
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
      if (categorical & variable == "landcover"){
        raster_yr <- as.factor(raster_yr)
        raster_yr <- set_raster_level_lc(raster_yr, str_extract(layer, "\\d+$"))
      }
      terra::time(raster_yr) <- dates_chunk
      names(raster_yr) <- dates_chunk
      raster_yr <- terra::project(raster_yr, crs$wkt)
      writeRaster(raster_yr, filenames_yrs[idx], overwrite = TRUE)
    }
    message("    Merge yearly data...")
    raster_total <- rast(filenames_yrs)
    filename_total <- path(dir_dest, paste0(variable, "_", layer, ".tif"))
    if (categorical) {
      # categorical rasters store additional data, create extra folder
      dir_create(path(dirname(filename_total), layer))
      filename_total <- path(dirname(filename_total), layer, paste0(layer, ".tif"))
    } 
    to_delete <- dir_ls(dir_dest, regexp  = layer, type = "file")
    writeRaster(raster_total,
                filename = filename_total,
                overwrite = TRUE)
    sapply(to_delete, function(file) file_delete(file))
  }
}
dem_file <- dir_ls(dir_dest, regexp = "dem_.+\\.tif$", recurse = TRUE)
tif_dem <- rast(dem_file)
slope <- terra::terrain(tif_dem, "slope")
aspect <- terra::terrain(tif_dem, "aspect")
writeRaster(slope, sub("dem", "slope", dem_file), overwrite = TRUE)
writeRaster(aspect, sub("dem", "aspect", dem_file), overwrite = TRUE)
