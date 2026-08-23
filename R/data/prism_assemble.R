library(terra)
library(checkmate)
library(fs)
library(here)
#-------------------------------------------------------------------------------
source("R/constants.R")
prism_variables <- CONF[["prism"]][["variables"]]
target_folders <- dir_ls(path(here(), "data", "raw", "prism"),
                         regexp = paste0(prism_variables, collapse = "|"))
destination_dir <- path(here(), "data")
dir_create(destination_dir)
#-------------------------------------------------------------------------------
checkmate::assert(length(target_folders) == length(prism_variables))
for (target_folder in target_folders) {
  prism_variable <- basename(target_folder)
  cat(paste0("Aggregate .tif files for variable '", prism_variable, "'\n"))
  target_tifs <- dir_ls(target_folder, recurse = TRUE, glob = "*.tif") |>
    sort()
  target_total <- terra::rast(target_tifs)
  # naming convention: "prism_<variable>_us_30s_<date>.tif"
  target_dates <- stringr::str_extract(target_tifs, "\\d{8}")
  target_dates <- lubridate::ymd(target_dates)
  names(target_total) <- target_dates
  terra::time(target_total) <- target_dates
  terra::writeRaster(target_total, path(destination_dir, paste0(prism_variable,".tif")), overwrite = TRUE)
}
