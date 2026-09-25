library(fs)
library(data.table)
library(checkmate)
library(recipes)
#-------------------------------------------------------------------------------
source("R/utils.R")
config <- readYaml("data")
variables <- c("vpdmax", "tmax", "vs")
destination_dir <- dir_create(path("assets", "scalers"))
checkmate::assertDirectoryExists(path("data", "tables"))
#-------------------------------------------------------------------------------
for (variable_name in variables) {
  dataset <- fread(path("data", "tables", paste0(variable_name, "_long.csv")))
  dataset <- dataset[year(date) <= config$end_train]
  scaler <- recipe(~value, data = dataset) |>
    step_scale(value) |>
    step_center(value) |>
    prep(training = dataset)
  saveRDS(scaler, path(destination_dir, paste0("scaler_", variable_name, ".rds")))
}