library(fs)
library(here)
library(sf)
library(terra)
#-------------------------------------------------------------------------------
source("R/utils/functions.R")
source("R/utils/data_queries.R")

conf <- get_conf()
crs <- st_crs(read_conf(conf, "crs"))
ca <- read_sf_frame(keyword = "ca_state", sf_crs = crs)
#-------------------------------------------------------------------------------
prism_folders <- dir_ls(path(here(),"data", "raw", "prism"))
destination_folder <- path(here(), "data", "preprocessed")
for(folder in prism_folders) {
  
  message(paste0("Preprocess variable ", basename(folder), "..."))
  pattern <- "\\d{6}\\.zip$"
  zip_files <- dir_ls(folder, recurse = TRUE, regexp = pattern)
  dates <- parse_string(zip_files, parserlist = parsers_prism)
  
  filename <- path(destination_folder, paste0(basename(folder), ".tif"))
  rasters <- lapply(zip_files, function(file){
    read_zip_raster(file) |>
      project(crs$wkt) |>
      crop(ca) |>
      mask(ca)
  })
  
  raster_total <- rast(rasters)
  names(raster_total) <- dates
  time(raster_total) <- dates
  
  rm(rasters)
  writeRaster(raster_total, filename = filename, overwrite = TRUE)
  message(paste0("    Wrote ", filename))
}
