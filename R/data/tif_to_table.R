library(zoo)
library(terra)
library(sf)
library(tidyverse)
library(fs)
library(here)
library(data.table)
#-------------------------------------------------------------------------------
tif_files <- dir_ls(path("data"), regexp = "\\.tif$")
destination_dir <- path(here(), "data", "tables")
dir_create(destination_dir)
# world geodetic system projection for longitude and latitude values
crs_destination <- st_crs(4326)
categorical <- FALSE
spat_res <- 2 # 2 km spat res
temp_res <- 3 # 3 day temp res
#-------------------------------------------------------------------------------
#tif_files <- path(here(), "data", "vs.tif")
tif_files <- tif_files[1]
for (tif_file in tif_files) {
  filename <- str_extract(basename(tif_file), pattern = "(.*)(?=.tif)")
  cat(paste0("Process raster ", filename, "...\n"))
  
  raster <- rast(tif_file)
  source_spat_res <- res(raster)[1]
  source_temp_res <- unique(diff(time(raster)))
  
  categorical <- all(is.factor(raster))
  #method <- ifelse(categorical, "near", "bilinear")
  #source_crs <- crs(raster, describe = TRUE)
  # if(source_crs$authority != "EPSG" || source_crs$code != 4326) {
  #   cat(paste0("   reproject source crs " , source_crs$name, " into destination crs ", crs_destination$input, "...\n"))
  #   raster <- project(raster, "EPSG:4326", method = method)
  # }
  target_resolution <- km_res / 111.32 # km in deg
  current_resolution <- res(raster)[1]
  if (current_resolution < target_resolution) {
    
    cat(paste0("   Current resolution ", current_resolution, " is smaller than ~", km_res, "km resolution...\n"))
    template <- rast(ext(raster), resolution = target_resolution, crs = crs(raster))
    raster <- resample(raster, template, method = method)
    
    tmpFiles(current = FALSE, orphan = TRUE, remove = TRUE)
    rm(template); gc()
  }
  else {
    cat(paste0("   Current resolution ", current_resolution, " is larger than ~", km_res, "km. Do not downsample resolution.\n"))
  }
  cat(paste0("   Read data.table...\n"))
  rastertable_wide <- raster |>
    as.data.table(xy = TRUE, na.rm =TRUE)
  setnames(rastertable_wide, old = c("x", "y"), new = c("lon", "lat"))
  fwrite(rastertable_wide, file = path(destination_dir, paste0(filename, "_wide.csv")))
  
  rm(raster);gc()
  cat(paste0("   Melt ", basename(tif_file), "...\n"))
  rastertable_long <- melt(rastertable_wide, id.vars = c("lon", "lat"),
                           variable.name = "date", value.name = "value")
  fwrite(rastertable_long, file = path(destination_dir, paste0(filename, "_long.csv")))
  rm(rastertable_long, rastertable_wide); gc()
}
