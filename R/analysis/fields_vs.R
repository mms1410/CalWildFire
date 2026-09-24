library(inlabru)
library(INLA)
#-------------------------------------------------------------------------------
source("R/utils.R")
source("R/analysis/utils.R")
source("R/analysis/defaults.R")

destination_dir <- dir_create(path("assets", "fields", "vs"))
save_rds <- saveFactory(destination_dir)
do.call(bru_options_set, bru_options)
#-------------------------------------------------------------------------------
vs <- readFile(path("data", "preprocessed", "samples", "vs.gpkg")) |>
  subset_train_test() |>
  dplyr::slice_sample(n = 50000)

vs <- vs |>
  mutate(month = lubridate::month(date),
         time = as.numeric(date),
         geometry = geom,
         value = value,
         month = lubridate::month(date))

mesh_2d <- readRDS(path("assets", "mesh", "mesh2d", "mesh2d_default.rds"))
mesh_1d <- readRDS(path("assets", "mesh", "mesh1d", "mesh2_irreg.rds"))
mapper_space <- bru_mapper(mesh_2d)
mapper_time <- bru_mapper(mesh_1d)

spde2d <- get_spde2d()
field_spat <- get_field_spat()
field_ar1 <- get_field_ar1()

do_fit1("gaussian", vs)
do_fit1("sn", vs)
do_fit1("t", vs)
do_fit1("skewt", vs)
do_fit1("weibull", vs)