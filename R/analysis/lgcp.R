#' SN CA diam 1220 Km
#' WE CA diam 400km
#' area CA 423km^2
#' 3300 stations
#' prior range appprox 3-5 edge length
#'
library(here)
library(fs)
library(sf)
library(fmesher)
library(data.table)
library(dplyr)
library(inlabru)
library(INLA)
library(ggplot2)
library(dplyr)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils", "utils_io.R"))
source(path(here(), "R", "utils", "utils_mesh.R"))
source(path(here(), "R", "utils", "utils_analysis.R"))
source(path(here(), "R", "utils", "utils_geocomp.R"))
source(path(here(), "R", "analysis", "components.R"))
#-------------------------------------------------------------------------------
set.seed(123)
bru_options_set(verbose = TRUE,
                bru_verbose = 3,
                bru_max_iter = 5,
                num.threads = "2:2",
                control.inla = list(strategy = "gaussian", int.strategy = "eb", diagonal = 1e-4),
                control.compute = list(dic = TRUE, waic = TRUE, config = TRUE),
                bru_int_args = list(method = "stable", nsub1 = 5, nsub2 = 3))


crs_km <- calAlbersKm()
ca_mld <- readFile("ca_mld.gpkg") |>
  st_transform(crs_km)
ecoz <- readFile("ecoz_mld.gpkg")

paths <- readYaml("paths")

destination_dir <- dir_create(reduceYamlPath(paths$dir_fits))
cache_dir <- dir_create(reduceYamlPath(paths$dir_cache))
mesh_dir <- dir_create(reduceYamlPath(paths$dir_mesh))
#-------------------------------------------------------------------------------
config <- readYaml("lgcp")

int_spec <- paste0(path_ext_remove(config$integration$mesh_temp),
                   "_",
                   path_ext_remove(config$integration$mesh_spat))

mesh_temp_int <- readFile(path(mesh_dir, config$integration$mesh_temp))
mesh_spat_int <- readFile(path(mesh_dir, config$integration$mesh_spat))

mesh_spat_tmax <- readFile(path(mesh_dir, config$fields$tmax$mesh_spat))
mesh_temp_tmax <- readFile(path(mesh_dir, config$fields$tmax$mesh_temp))

mesh_spat_vpdmax <- readFile(path(mesh_dir, config$fields$tmax$mesh_spat))
mesh_temp_vpdmax <- readFile(path(mesh_dir, config$fields$vpdmax$mesh_temp))

mesh_spat_vs <- readFile(path(mesh_dir, config$fields$vs$mesh_spat))
mesh_temp_vs <- readFile(path(mesh_dir, config$fields$vs$mesh_temp))

mesh_spat_ppt <- readFile(path(mesh_dir, config$fields$ppt$mesh_spat))
mesh_temp_ppt <- readFile(path(mesh_dir, config$fields$ppt$mesh_temp))

saveModel <- saveModelFactory(destination_dir, paste0("lgcp_", int_spec))

cache_name_ips <- paste0("ips_", int_spec, ".rds")
cache_name_fit <- paste0("lgcp_", int_spec, ".rds")
cache_name_fires <- "fires_2015.gpkg"
cache_name_tmax <- paste0("tmax_", config$fields$tmax$mesh_temp, "_", config$fields$tmax$mesh_temp, ".gpkg")
cache_name_vpdmax <- paste0("vpdmax_", config$fields$vpdmax$mesh_temp, "_", config$fields$vpdmax$mesh_temp, ".gpkg")
cache_name_vs <- paste0("vpdmax_", config$fields$vs$mesh_temp, "_", config$fields$vs$mesh_temp, ".gpkg")
cache_name_ppt <- paste0("vpdmax_", config$fields$ppt$mesh_temp, "_", config$fields$ppt$mesh_temp, ".gpkg")
#===============================================================================
#                              Data
#===============================================================================
# Fire Locations
if (file_exists(path(cache_dir, cache_name_fires))) {
  fires <- readFile(cache_name_fires)
} else {
  fires <- readFile("calfire.gpkg") |>
    filter(year(date) <= 2015) |>
    mutate(time = as.numeric(date)) |>
    addRoadDistance() |>
    addRastToSfPoint("evi.tif", "evi") |>
    addRastToSfPoint("lai.tif", "lai") |>
    mutate(time = as.numeric(date)) |>
    st_transform(crs_km)
  st_write(fires, path(cache_dir, cache_name_fires), append = FALSE)
}

# Latent Field vpdmax
if (file_exists(path(cache_dir, cache_name_vpdmax))) {
  vpdmax <- readFile(cache_name_vpdmax)
} else{
  vpdmax <- readFile(config$fields$vpdmax$data)[year(date) <= 2015][sample(.N, config$fields$vpdmax$n_sample)]
  vpdmax <- vpdmax |>
    st_as_sf(coords = c("x", "y"), crs = st_crs(ecoz)) |>
    mutate(time = as.numeric(date)) |>
    addEcozToSf(ecoz) |>
    addCoastDistance() |>
    addRastToSfPoint("dem.tif", "elevation") |>
    st_transform(crs_km)
  st_write(vpdmax, path(cache_dir, cache_name_vpdmax), append = FALSE)
}
vpdmax <- vpdmax |>
  mutate(value = (value - mean(value)) / sd(value))


# Latent Field tmax
if (file_exists(path(cache_dir, cache_name_tmax))) {
  tmax <- readFile(cache_name_tmax)
} else{
  tmax <- readFile(config$fields$tmax$data)[year(date) <= 2015][sample(.N, config$fields$tmax$n_sample)]
  tmax <- tmax |>
    st_as_sf(coords = c("x", "y"), crs = st_crs(ecoz)) |>
    mutate(time = as.numeric(date)) |>
    addEcozToSf(ecoz) |>
    addCoastDistance() |>
    addRastToSfPoint("dem.tif", "elevation") |>
    st_transform(crs_km)
  st_write(tmax, path(cache_dir, cache_name_tmax), append = FALSE)
}
tmax <- tmax |>
  mutate(value = (value - mean(value)) / sd(value))

# Latent Field vs
if (file_exists(path(cache_dir, cache_name_vs))) {
  vs <- readFile(cache_name_vs)
} else{
  vs <- readFile(config$fields$vs$data)[year(date) <= 2015][sample(.N, config$fields$vs$n_sample)]
  vs <- vs |>
    st_as_sf(coords = c("x", "y"), crs = st_crs(ecoz)) |>
    mutate(time = as.numeric(date)) |>
    addEcozToSf(ecoz) |>
    addCoastDistance() |>
    addRastToSfPoint("dem.tif", "elevation") |>
    st_transform(crs_km)
  st_write(vs, path(cache_dir, cache_name_vs), append = FALSE)
}
vs <- vs |>
  mutate(value = sqrt(value) + 1)


# Latent Field ppt
if (file_exists(path(cache_dir, cache_name_ppt))) {
  ppt <- readFile(cache_name_ppt)
} else{
  ppt <- readFile(config$fields$ppt$data)[year(date) <= 2015][sample(.N, config$fields$ppt$n_sample)]
  ppt <- ppt |>
    st_as_sf(coords = c("x", "y"), crs = st_crs(ecoz)) |>
    mutate(time = as.numeric(date)) |>
    addEcozToSf(ecoz) |>
    addCoastDistance() |>
    addRastToSfPoint("dem.tif", "elevation") |>
    st_transform(crs_km)
  st_write(ppt, path(cache_dir, cache_name_ppt), append = FALSE)
}
#===============================================================================
#                              Integration Points
#===============================================================================
if (file_exists(path(cache_dir, cache_name_ips))) {
  ips <- readFile(cache_name_ips)
} else {
  checkmate::assert(fm_crs(mesh_spat_int) == st_crs(ca_mld))
  integration_mesh <- fmesher::fm_int(domain  = list(geometry = mesh_spat_vpdmax, time  = mesh_temp_int),
                              samplers = ca_mld)
  # integration points assumed to have covariate values
  ips <- integration_mesh |>
    mutate(date = as.Date(time)) |>
    filter(year(date) <= 2016) |>
    addRastToSfPoint("evi.tif", "evi") |> # coastline na
    addRastToSfPoint("lai.tif", "lai") |> # no na
    addRoadDistance()
  
  saveRDS(ips, path(cache_dir, cache_name_ips))
}
#-------------------------------------------------------------------------------
fires$distance_road <- log1p(fires$distance_road)
ips$distance_road <- log1p(ips$distance_road)
hist(c(fires$distance_road, ips$distance_road))
sum(c(is.na(fires$distance_road), is.na(ips$distance_road)))

fires$lai <- log1p(fires$lai)
ips$lai <- log1p(ips$lai)
hist(c(fires$lai, ips$lai))

fires$evi <- log1p(fires$evi)
ips$evi <- log1p(ips$evi)
hist(c(fires$evi, ips$evi))

n_region <- length(unique(tmax$region))
graph_ecoz <- readFile("ecoz_mld.adj")
#===============================================================================
#                              SPDE Specification
#===============================================================================
getPriors <- function(name) {
  prs <- unlist(config$fields[[name]]$prior_range)
  prt <- unlist(config$fields[[name]]$prior_temp)
  psigma <- unlist(config$fields[[name]]$prior_sigma)
  return(list(prs = prs, prt = prt, psigma = psigma))
}

priors_spacetime <- list(prs = c(700, 0.05), prt = c(1, 0.5), psigma = c(1, 0.05))

spde_102_tmax <- initSeperableSPDE(mesh_spat_tmax, mesh_temp_tmax, getPriors("tmax"))
spde_102_vpdmax <- initSeperableSPDE(mesh_spat_vpdmax, mesh_temp_vpdmax, getPriors("vpdmax"))
spde_102_ppt <- initSeperableSPDE(mesh_spat_ppt, mesh_temp_ppt, getPriors("ppt"))
spde_102_vs <- initSeperableSPDE(mesh_spat_vs, mesh_temp_vs, getPriors("vs"))

cmp_field_102_tmax <- bru_comp_list( ~ -1 + field_tmax(main = list(space = geometry, time = time), model = spde_102_tmax))
cmp_field_102_vpdmax <- bru_comp_list( ~ -1 + field_vpdmax(main = list(space = geometry, time = time), model = spde_102_vpdmax))
cmp_field_102_vs <- bru_comp_list(~ -1 + field_vs(main = list(space = geometry, time = time), model = spde_102_vs))
cmp_field_102_ppt <- bru_comp_list(~ -1 + field_ppt(main = list(space = geometry, time = time), model = spde_102_ppt))
#===============================================================================
#                              Components Latent Fields
#===============================================================================
cmp_intercept_tmax <- initCmpIntercept("tmax")
cmp_elevation_tmax <- initCmpElevation("tmax")
cmp_coast_distance_tmax <- initCmpCoastDistance("tmax")

cmp_intercept_vpdmax <- initCmpIntercept("vpdmax")
cmp_elevation_vpdmax <- initCmpElevation("vpdmax")
cmp_coast_distance_vpdmax <- initCmpCoastDistance("vpdmax")

cmp_intercept_ppt <- initCmpIntercept("ppt")
cmp_elevation_ppt <- initCmpElevation("ppt")
cmp_coast_distance_ppt <- initCmpCoastDistance("ppt")

cmp_intercept_vs <- initCmpIntercept("vs")
cmp_elevation_vs <- initCmpElevation("vs")
cmp_coast_distance_vs <- initCmpCoastDistance("vs")

cmp_tmax <- c(cmp_intercept_tmax,
              cmp_elevation_tmax,
              cmp_coast_distance_tmax,
              cmp_field_102_tmax)

cmp_vpdmax <- c(cmp_intercept_vpdmax,
                cmp_elevation_vpdmax,
                cmp_coast_distance_vpdmax,
                cmp_field_102_vpdmax)

cmp_ppt <- c(cmp_intercept_ppt,
             cmp_elevation_ppt,
             cmp_coast_distance_ppt,
             cmp_field_102_ppt)

cmp_vs <- c(cmp_intercept_vs,
            cmp_elevation_vs,
            cmp_coast_distance_vs,
            cmp_field_102_vs)

obs_model_tmax <- bru_obs(value ~ Intercept_tmax + effect_elevation_tmax + effect_coast_distance_tmax + field_tmax,
                          family = "gaussian",
                          data = tmax)

obs_model_vpdmax <- bru_obs(value ~ Intercept_vpdmax + effect_elevation_vpdmax + effect_coast_distance_vpdmax + field_vpdmax,
                            family = "gaussian",
                            data = vpdmax)

obs_model_ppt <- bru_obs(value ~ Intercept_ppt + effect_elevation_ppt + effect_coast_distance_ppt + field_ppt,
                         family = "tweedie",
                         data = ppt)

obs_model_vs <- bru_obs(value ~ Intercept_vs + effect_elevation_vs + effect_coast_distance_vs + field_vs,
                         family = "gamma",
                         data = vs)
#===============================================================================
#                           Components Intensity
#===============================================================================
cmp_intercept <- initCmpIntercept()
cmp_lai <- bru_comp_list(~ -1 + effect_lai(lai, model = "linear"))
cmp_evi <- bru_comp_list(~ -1 + effect_evi(evi, model = "linear"))

cmp_lgcp <- c(cmp_ppt, cmp_vs, cmp_intercept, cmp_lai, cmp_evi)

obs_model_lgcp <-  bru_obs(formula = geometry + time ~  Intercept + effect_evi + effect_lai + field_ppt + field_vs + field_tmax,
                           family = "cp",
                           data = fires,
                           ips = ips)

fit_lgcp <- bru(cmp_lgcp,
                obs_model_lgcp,
                obs_model_tmax,
                obs_model_vs,
                obs_model_ppt)
           
saveModel(fit_lgcp, "lgcp_")