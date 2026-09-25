library(fs)
library(data.table)
library(gstat)
#-------------------------------------------------------------------------------
source("R/utils.R")
config <- readYaml("data")
destination_dir <- dir_create("assets", "vario")
ecoz <- readFile(path("data", "assets", "ecoz.gpkg"))
vario_cutoff <- 500000
vario_width <- 15000
#-------------------------------------------------------------------------------
datasets <- dir_ls(path("data", "tables"), glob = "*_long.csv")
for (data_path in datasets) {
  variable_name <- gsub("_long", "", get_filename(data_path))
  dataset <- fread(data_path) |>
    filter(!is.na(value)) # lon, lat combo outside cal
  destination_dir_var <- dir_create(path(destination_dir, variable_name))
  cat(paste0("  Fit variogram for '", variable_name, "'...\n"))
 for (yr in unique(year(dataset$date))) {
   dataset_year <- dataset[year(date) == yr]
   for (mth in unique(month(dataset_year$date))) {
     data_sf <- st_as_sf(dataset[year(date) == yr & month(date) == mth],
                         coords = c("lon", "lat"), crs = config$crs)
     vario <- gstat::variogram(value ~ date, data = data_sf, cutoff = vario_cutoff, width = vario_width)
     saveRDS(vario, path(destination_dir_var, paste0(yr, "_", mth, ".rds")))
   }
   data_sf <- st_as_sf(dataset_year, coords = c("lon", "lat"), crs = config$crs)
   vario <- gstat::variogram(value ~ date, data = data_sf, cutoff = vario_cutoff, width = vario_width)
   saveRDS(vario, path(destination_dir_var, paste0(yr, ".rds")))
 } 
}
