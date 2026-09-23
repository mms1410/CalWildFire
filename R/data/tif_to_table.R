library(terra)
library(fs)
library(lubridate)
library(data.table)
#-------------------------------------------------------------------------------
source("R/utils.R")
config <- readYaml("data")
spat_res <- config$table_spat_res
temp_res <- config$table_temp_res
tif_files <- dir_ls(path(getwd(), "data", "preprocessed"), glob = "*.tif")
destination_dir <- dir_create(path(getwd(), "data", "tables"))
#-------------------------------------------------------------------------------
cat(paste0("Create tables with ", spat_res, "km spatial resolution and ", temp_res, " day-steps\n"))
for (tif_file in tif_files) {
  # filename = variablename
  variable_name <- path_ext_remove(basename(tif_file))
  cat(paste0("Process ", variable_name, "...\n"))
  raster <- terra::rast(tif_file)
  
  # layer = time
  if ((nlyr(raster)) > 1) {
    raster_agg <- aggSpatTemp(raster, tmp_res = temp_res, spat_res = spat_res)
  } else {
    raster_agg <- aggSpatTemp(raster, spat_res = spat_res)
  }
  
  dtbl_wide <- as.data.table(raster_agg, xy = TRUE)
  setnames(dtbl_wide, 1:2, c("lon", "lat"))
  
  dtbl_long <- melt(dtbl_wide,
                    id.vars = c("lon", "lat"),
                    measure.vars = setdiff(names(dt), c("long", "lat")),
                    variable.name = "date",
                    value.name = "value")
  dtbl_long[, date:= lubridate::ymd(date)]
  
  
  fwrite(dtbl_long, path(destination_dir, paste0(variable_name, "_long.csv")))
  fwrite(dtbl_wide, path(destination_dir, paste0(variable_name, "_wide.csv")))
}
