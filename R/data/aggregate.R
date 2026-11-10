library(lubridate)
library(fs)
library(here)
library(fmesher)
library(terra)
library(sf)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils", "utils_io.R"))
paths <- readYaml("paths")
destination_dir <- reduceYamlPath(paths$dir_aux)
raster_files <-  c("ppt.tif", "vs.tif", "tmax.tif", "vpdmax.tif", "evi.tif")
#-------------------------------------------------------------------------------
process_raster <- function(filename, destination_dir = destination_dir) {
  
  dir_create(destination_dir)
  
  raster <- readFile(filename)
  variable_name <- path_ext_remove(filename)
  message(sprintf("Process variable %s", variable_name))
  
  config <- readYaml("data")
  years_train <- seq(config$start_train,config$end_train)
  
  dates <-  format(terra::time(raster), "%Y-%m-%d")
  years_idx <- sub("-.*", "", dates)
  months_idx <- sub(".*-", "", dates)
  
  chunks_year <- split(dates, years_idx)
  chunks_month <- split(dates, months_idx)
  
  #  aggregate yearly
  for (yr in names(chunks_year)) {
    infoMessage(sprintf("Aggregate year %s", yr))
    
    raster_names <- chunks_year[as.character(yr)]
    destination <- path(destination_dir, paste0(variable_name, "_yr_", yr, ".tif"))
    raster_agg <- terra::app(raster[[raster_names]],
                             fun = "mean",
                             na.rm = TRUE,
                             filename = destination,
                             overwrite = TRUE)
    gc();tmpFiles(remove = TRUE)
  }
  files_list <- dir_ls(destination_dir, regexp = paste0(variable_name, "_yr_"))
  agg_yr <- rast(files_list)
  names(agg_yr) <- names(chunks_year)
  writeRaster(agg_yr, path(destination_dir, paste0(variable_name, "_agg_yr.tif")), overwrite = TRUE)
  lapply(files_list, file_delete)
  
  
  # aggregate monthly
  for (mth in names(chunks_month)) {
    infoMessage(sprintf("Aggregate month %s", mth))
    
    raster_names <- chunks_month[[as.character(mth)]]
    destination <- path(destination_dir, paste0(variable_name, "_mth_", mth, ".tif"))
    raster_agg <- terra::app(raster[[raster_names]],
                             fun = "mean",
                             na.rm = TRUE,
                             filename = destination,
                             overwrite = TRUE)
    gc();tmpFiles(remove = TRUE)
  }
  files_list <- dir_ls(destination_dir, regexp = paste0(variable_name, "_mth_"))
  agg_mth <- rast(files_list)
  names(agg_mth) <- names(chunks_month)
  writeRaster(agg_mth, path(destination_dir, paste0(variable_name, "_agg_mth.tif")), overwrite = TRUE)
  lapply(files_list, file_delete)
  
  # compute sd
  infoMessage("calulcate pixelwise sd")
  dates_train_idx <- year(dates) %in% years_train
  destination <-  path(destination_dir, paste0(variable_name, "_sd_train.tif"))
  rast_sd <- terra::app(raster[[dates_train_idx]],
                        fun = "sd",
                        na.rm = TRUE,
                        filename = destination,
                        overwrite = TRUE)
}

for (filename in raster_files) {
  process_raster(filename)
}