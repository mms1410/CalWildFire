library(zoo)
library(terra)
library(sf)
library(tidyverse)
library(fs)
library(here)
library(data.table)
#-------------------------------------------------------------------------------
source("R/utils.R")
tif_files <- dir_ls(path("data"), regexp = "\\.tif$")
destination_dir <- path(here(), "data", "tables")
dir_create(destination_dir)
spat_res <- 4 # x km spatial res
tmp_res <-  3 # x day temporal res
#-------------------------------------------------------------------------------
#tif_file <- tif_files[4]
for (tif_file in tif_files) {
  filename <- str_extract(basename(tif_file), pattern = "(.*)(?=.tif)")
  cat(paste0("Process raster ", filename, "...\n"))
  
  raster <- rast(tif_file)
  raster_agg <- aggSpatTemp(raster, tmp_res = tmp_res, spat_res = spat_res)
  
  dtbl_long <- as.data.table(raster_agg, xy = TRUE)
  setnames(dtbl_long, 1:2, c("lon", "lat"))
  fwrite(dtbl_long, path(destination_dir, paste0(filename, ".csv")))
}
