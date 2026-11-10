library(ggplot2)
library(patchwork)
library(here)
library(fs)
library(lubridate)
library(sf)
library(dplyr)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils", "utils_io.R"))
source(path(here(), "R", "utils", "utils_geocomp.R"))
source(path(here(), "R", "plots", "defaults.R"))
#-------------------------------------------------------------------------------
ca_mld <- readFile("ca_mld.gpkg") |>
  st_transform(calAlbersKm())

eval_grid <- getEvalGrid(make_date(2011, 1, 1), make_date(2011, 12, 31))
eval_grid |>
  filter(date == make_date(2011, 1,1)) |>
  plot()

fit_cDiff <- readFile("fit_tmax_temp_mesh_k30d1_2016_spat_mesh_2_cDiff.rds")
fit_kron <- readFile("fit_tmax_temp_mesh_k30d1_2016_spat_mesh_2_kron.rds")
fit_iDiff <- readFile("fit_tmax_temp_mesh_k30d1_2016_spat_mesh_2_iDiff.rds")


fits <- list(fit_kron, fit_cDiff, fit_iDiff)
names(fits) <- c("Kronecker", "critical Diffusion", "iterative Diffusion")