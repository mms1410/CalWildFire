library(zoo)
library(terra)
library(tidyverse)
library(fs)
library(data.table)
#-------------------------------------------------------------------------------
fact <- 3
#-------------------------------------------------------------------------------
message(paste0("Aggregate rasters with factor ", fact))
tif_files <- dir_ls(path("data", "preprocessed"), regexp = ".tif")
destination_dir <- path("data", "preprocessed")
for (tif_file in tif_files) {
  filename <- str_extract(basename(tif_file), pattern = "(.*)(?=.tif)")
  message(paste0("Load raster ", filename, "..."))
  raster <- rast(tif_file)
  raster <- project(raster, "EPSG:4326")
  message(paste0("Read data.table ", filename, "..."))
  rastertable_wide <- raster |>
    aggregate(fact = fact) |> # otw too large /memory issues
    as.data.table(xy = TRUE, na.rm =TRUE)
  setnames(rastertable_wide, old = c("x", "y"), new = c("lon", "lat"))
  fwrite(rastertable_wide, file = path(destination_dir, paste0(filename, "_wide.csv")))
  rm(raster)
  message(paste0("Melt ", basename(tif_file), "..."))
  rastertable_long <- melt(rastertable_wide, id.vars = c("lon", "lat"),
                           variable.name = "date", value.name = "value")
  fwrite(rastertable_long, file = path(destination_dir, paste0(filename, "_long.csv")))
  rm(rastertable_long)
}