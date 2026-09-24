library(inlabru)
library(INLA)
#-------------------------------------------------------------------------------
source("R/utils.R")
source("R/analysis/utils.R")
source("R/analysis/defaults.R")

destination_dir <- dir_create(path("assets", "fields", "ppt"))
save_rds <- saveFactory(destination_dir)
do.call(bru_options_set, bru_options)
#-------------------------------------------------------------------------------
ppt <- readFile(path("data", "preprocessed", "samples", "ppt.gpkg")) |>
  subset_train_test() |>
  dplyr::slice_sample(n = 50000)

ppt <- ppt |>
  mutate(month = lubridate::month(date),
         time = as.numeric(date),
         geometry = geom,
         value = value,
         month = lubridate::month(date),
         occurance = as.numeric(value > 0),
         amount = ifelse(value > 0, value, NA))

ppt_occured <- ppt |> 
  filter(!is.na(amount))

mesh_2d <- readRDS(path("assets", "mesh", "mesh2d", "mesh2d_default.rds"))
mesh_1d <- readRDS(path("assets", "mesh", "mesh1d", "mesh2_irreg.rds"))
mapper_space <- bru_mapper(mesh_2d)
mapper_time <- bru_mapper(mesh_1d)

spde2d <- get_spde2d()
field_spat <- get_field_spat()
field_ar1 <- get_field_ar1()
field_spattemp1 <- get_field_spatiotemp1()

do_fit1("tweedie", ppt)