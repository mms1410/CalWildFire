library(zoo)
library(terra)
library(sf)
library(tidyverse)
library(fs)
library(here)
library(data.table)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils", "data_queries.R"))
conf <- get_conf()
crs <- st_crs(read_conf(conf, "crs"))
categorical <- FALSE
tif_files <- dir_ls(path("data", "preprocessed"), regexp = "\\.tif$", recurse = TRUE)
destination_dir <- path("data", "preprocessed")
# world geodetic system projection for longitude and latitude values
crs_destination <- st_crs(4326)
km_res <- 2 # desired resolution in km
#-------------------------------------------------------------------------------
for (tif_file in tif_files) {
  filename <- str_extract(basename(tif_file), pattern = "(.*)(?=.tif)")
  message(paste0("Process raster ", filename, "..."))
  raster <- rast(tif_file)
  categorical <- all(is.factor(raster))
  method <- ifelse(categorical, "near", "bilinear")
  source_crs <- crs(raster, describe = TRUE)
  if(source_crs$authority != "EPSG" || source_crs$code != 4326) {
    message(paste0("   reproject source crs " , source_crs$name, " into destination crs ", crs_destination$input, "..."))
    raster <- project(raster, "EPSG:4326", method = method)
  }
  target_resolution <- km_res / 111.32 # km in deg
  current_resolution <- res(raster)[1]
  if (current_resolution < target_resolution) {
    message(paste0("   Current resolution ", current_resolution, " is smaller than ~", km_res, "km (deg) resolution..."))
    template <- rast(ext(raster), resolution = target_resolution, crs = crs(raster))
    raster <- resample(raster, template, method = method)
    tmpFiles(current = FALSE, orphan = TRUE, remove = TRUE)
    rm(template); gc()
  }
  message(paste0("   Read data.table..."))
  rastertable_wide <- raster |>
    as.data.table(xy = TRUE, na.rm =TRUE)
  setnames(rastertable_wide, old = c("x", "y"), new = c("lon", "lat"))
  fwrite(rastertable_wide, file = path(destination_dir, paste0(filename, "_wide.csv")))
  rm(raster);gc()
  message(paste0("   Melt ", basename(tif_file), "..."))
  rastertable_long <- melt(rastertable_wide, id.vars = c("lon", "lat"),
                           variable.name = "date", value.name = "value")
  fwrite(rastertable_long, file = path(destination_dir, paste0(filename, "_long.csv")))
  rm(rastertable_long, rastertable_wide); gc()
}
