library(fs)
library(ggplot2)
library(terra)
library(tidyterra)
#-------------------------------------------------------------------------------
source("R/utils.R")
source("R/plots/defaults_plots.R")
source("R/plots/utils_plots.R")
tif_files <- dir_ls(path("data", "preprocessed"), glob = "*.tif")
destination_dir <- dir_create(path("assets", "plots", "eda", "raster"))
gg_save <- ggSaveFactory(destination_dir)
#-------------------------------------------------------------------------------
for (tif_path in tif_files) {
  variable_name <- getFilename(tif_path)
  cat(paste0())
  raster <- rast(tif_path)
  
  chunks_year <- split(terra::time(raster), format(terra::time(raster), "%Y"))
  chunks_month <- split(terra::time(raster), format(terra::time(raster), "%m"))
  
  
  for (mth in names(chunks_month)) {
    idx <- time(raster) %in% chunks_month[[mth]]
    raster_mth <- mean(raster[[idx]], na.rm = TRUE)
    
    plt_mth <- ggplot() +
      geom_spatraster(data = raster_mth) +
      scale_fill_viridis_c(na.value = NA, name = variable_name, guide = "colorbar")
    gg_save(paste0(variable_name, "_month_", mth, ".png")) 
  }
  
  for (yr in names(chunks_year)) {
    idx <- time(raster) %in% chunks_year[[yr]]
    raster_yr <- mean(raster[[idx]], na.rm = TRUE)
    
    plt_yr <- ggplot() +
      geom_spatraster(data = raster_yr) +
      scale_fill_viridis_c(na.value = NA, name = variable_name, guide = "colorbar")
    gg_save(paste0(variable_name, "_year_", yr, ".png")) 
  }
}
