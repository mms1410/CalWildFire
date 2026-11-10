library(fs)
library(here)
library(tidyverse)
library(sf)
library(terra)
library(stringr)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils", "utils_io.R"))
source(path(here(), "R", "utils", "utils_geocomp.R"))
#-------------------------------------------------------------------------------
config <- readYaml("data")
cal_crs <- st_crs(config$crs)
mask_mainland <- readFile(path(here(), "data", "auxiliary", "cal_mld.gpkg"))
modis_conf <- config$modis$appeears
modis_variables <- names(modis_conf)
modis_folders <- sapply(modis_variables, function(x) path(here(), "data", "raw", x))
modis_layers <- sapply(modis_variables, function(x) modis_conf[[x]][["layer"]]) # appeears offers multiple layers for a variable, for example evi -> 500m, 250m, resolution....
destination_dir <- path(here(), "data", "preprocessed")
tmp_dir <- path(here(), "tmp_terra")
#-------------------------------------------------------------------------------
for (variable_name in modis_variables) {
  folder <- modis_folders[[variable_name]]
  if (variable_name != "lai") next
  if (!dir_exists(folder)) {
    warning(sprintf("Expected folder at %s for %s, but does not exist.",folder, variable_name))
    next
  }
  message(sprintf("🛠 Process %s", variable_name))
  layers <- modis_layers[[variable_name]]
  #-----------------------------------------------------------------------------
  for (layer in layers) { 
    message(sprintf("  Process layer %s", layer))
    pattern <- paste0("*", layer, "*.tif$")
    files <- dir_ls(folder, glob = pattern, recurse = TRUE)
    checkmate::assertFile(files)
    dates <- str_extract(files, "(?<=doy)(\\d{4})(\\d{3})")
    dates <- lubridate::as_date(dates, format = "%Y%j")
    #---------------------------------------------------------------------------
    message("    Read tif files....")
    raster <- terra::rast(files)
    names(raster) <- dates
    terra::time(raster) <- dates
    #---------------------------------------------------------------------------
    if (!terra::same.crs(raster, cal_crs$wkt))
      message(sprintf("    Project crs from %s into %s....", crs(raster, describe = TRUE)[["name"]],  crs(cal_crs$wkt, describe = TRUE)[["name"]]))
      raster <- project(raster, cal_crs$wkt)
    #---------------------------------------------------------------------------
    message("    Crop to mainland...")
    dir_create(tmp_dir)
    terraOptions(tempdir = tmp_dir)
    raster <- cropMainland(raster, mask_mainland)
    
    if (grepl("dem", variable_name, ignore.case = TRUE)) {
      message("   Compute slope and aspect...")
      slope <- terra::terrain(raster, v = "slope", unit = "degrees")
      writeRaster(slope, path(destination_dir, "slope.tif"), overwrite = TRUE)
      aspect <- terra::terrain(raster, v = "aspect", unit = "degrees")
      writeRaster(aspect, path(destination_dir, "aspect.tif"), overwrite = TRUE)
      message("   Scale DEM...")
      raster <- terra::scale(raster)
      rm(aspect, slope); gc()
    }
    #---------------------------------------------------------------------------
    # WRITE AND CLEAN
    writeRaster(raster, path(destination_dir, paste0(variable_name, ".tif")), overwrite = TRUE)
    dir_delete(tmp_dir)
    rm(raster);gc()
    terra::tmpFiles(current = TRUE, orphan = TRUE, remove = TRUE)
  }
}
tailMessage()