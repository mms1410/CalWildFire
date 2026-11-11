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
                num.threads = "1:1",
                control.inla = list(strategy = "gaussian", int.strategy = "eb", diagonal = 1e-4),
                control.compute = list(dic = FALSE, waic = FALSE, config = FALSE),
                bru_int_args = list(method = "stable", nsub1 = 5, nsub2 = 3))


crs_km <- calAlbersKm()
ca_mld <- readFile("ca_mld.gpkg") |>
  st_transform(crs_km)
ecoz <- readFile("ecoz_mld.gpkg")

destination_dir <- dir_create(path(here(), "assets", "fits"))
cache_dir <- dir_create(path(here(), "data", "cache"))
#-------------------------------------------------------------------------------
config <- readYaml("lgcp")

mesh_name_temp_int <- config$mesh_temp$integration
mesh_name_spat_int <- config$mesh_spat$integration

mesh_temp_int <- readFile(path("assets","mesh", paste0(mesh_name_temp_int, ".rds")))
mesh_spat_int <- readFile(path("assets","mesh", paste0(mesh_name_spat_int, ".rds")))

mesh_spat_tmax <- readFile(path("assets","mesh", config$mesh_spatial$tmax))
mesh_temporal_tmax <- readFile(path("assets", "mesh", config$mesh_temp$tmax))

mesh_spat_vpdmax <- readFile(path("assets", "mesh", config$mesh_spatial$vpdmax))
mesh_temporal_vpdmax <- readFile(path("assets", "mesh", config$mesh_temp$vpdmax))

saveModel <- saveModelFactory(destination_dir, paste0("lgcp_", mesh_name_temp_int, "_", mesh_name_spat_int))

cache_name_ips <- paste0("ips_", mesh_name_temp_int, "_", "mesh_name_spat_int", "_2011.rds" ) # sf not possible here
cache_name_fit <- paste0("lgcp_", mesh_name_temp_int, "_", mesh_name_spat_int, "_2011.rds")
cache_name_fires <- "fires_2015.gpkg"
cache_name_tmax <- paste0("tmax_", config$mesh_temporal$tmax, "_", config$mesh_spatial$tmax, ".gpkg")
cache_name_vpdmax <- paste0("vpdmax_", config$mesh_temporal$vpdmax, "_", config$mesh_spatial$vpdmax, ".gpkg")
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
  vpdmax <- readFile(config$data$vpdmax)[year(date) <= 2015][sample(.N, config$n_sample)]
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
  tmax <- readFile(config$data$tmax)[year(date) <= 2015][sample(.N, config$n_sample)]
  tmax <- tmax |>
    st_as_sf(coords = c("x", "y"), crs = st_crs(ecoz)) |>
    mutate(time = as.numeric(date)) |>
    addEcozToSf(ecoz) |>
    addCoastDistance() |>
    addRastToSfPoint("dem.tif", "elevation") |>
    st_transform(crs_km)
  st_write(vpdmax, path(cache_dir, cache_name_tmax), append = FALSE)
}
tmax <- tmax |>
  mutate(value = (value - mean(value)) / sd(value))
#===============================================================================
#                              Integration Points
#===============================================================================
if (file_exists(path(cache_dir, cache_name_ips))) {
  ips <- readFile(cache_name_ips)
} else {
  checkmate::assert(fm_crs(mesh_spat_int) == st_crs(ca_mld))
  integration_mesh <- fmesher::fm_int(domain  = list(geometry = mesh_spat_int, time  = mesh_temp_int),
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

ips_outsider <- ips |>
  filter(!stWithinLogical(geometry, ca_mld) & time == 12418) |> # first date in numeric
  select(geometry)

colSums(is.na(fires))
colSums(is.na(ips))

evi_nas <- ips |>
  filter(is.na(evi))

lai_nas <- ips |>
  filter(is.na(lai))

# int_pts <- int_pts |>
#   mutate(lai = tidyr::replace_na(lai, 0),
#          evi = tidyr::replace_na(evi, 0))

ggplot() +
  geom_sf(data = st_union(fm_as_sfc(mesh_spat_int)), color = "blue", fill = NA) +
  geom_sf(data = ca_mld, fill = NA) +
  geom_sf(data = ips_outsider, color = "black", shape = 13) +
  geom_sf(data = evi_nas, color = "orange", shape = 2) +
  geom_sf(data = lai_nas, color  = "red", shape = 14)
#-------------------------------------------------------------------------------
fires$distance_road <- log1p(fires$distance_road)
ips$distance_road <- log1p(ips$distance_road)

fires$lai <- log1p(fires$lai)
ips$distance_road <- log1p(ips$distance_road)

n_region <- length(unique(tmax$region))
graph_ecoz <- readFile("ecoz_mld.adj")
#===============================================================================
#                              SPDE Specification
#===============================================================================
plot(mesh_spat_tmax)
meshInfoPriorRange(mesh_spat_tmax)
spde_kron_tmax <- inla.spde2.pcmatern(mesh_spat_tmax,
                                 prior.sigma = c(1, 0.05),
                                 prior.range = c(700, 0.05))
plot(mesh_spat_vpdmax)
meshInfoPriorRange(mesh_spat_vpdmax)
spde_kron_vpdmax <- inla.spde2.pcmatern(mesh_spat_vpdmax,
                                   prior.sigma = c(1, 0.05),
                                   prior.range = c(700, 0.05))
#===============================================================================
cmp_intercept_tmax <- initCmpIntercept("tmax")
# TODO: adj.matrix <-> region char
#cmp_region_tmax <- initCmpBesag("tmax")
cmp_elevation_tmax <- initCmpElevation("tmax")
cmp_coast_distance_tmax <- initCmpCoastDistance("tmax")
cmp_field_kron_tmax <- initCmpKroneckerField("tmax", mesh_temporal_tmax, spde_kron_tmax)

cmp_tmax <- c(cmp_intercept_tmax,
              cmp_elevation_tmax,
              cmp_coast_distance_tmax,
              cmp_field_kron_tmax)

obs_model_tmax <- bru_obs(value ~ -1 +  Intercept_tmax  + effect_elevation_tmax +
                            effect_coast_distance_tmax + field_kron_tmax,
                          family = "gaussian",
                          data = tmax,
                          control.family = list(hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))))

# "sanity check"
# fit_tmax <- bru(components = cmp_tmax, obs_model_tmax)
#-------------------------------------------------------------------------------
cmp_intercept <- initCmpIntercept()
cmp_lai <- bru_comp_list(~ -1 + effect_lai(lai, model = "linear"))
cmp_evi <- bru_comp_list(~ -1 + effect_evi(evi, model = "linear"))
cmp_tmax_copy <- bru_comp_list( ~ -1 + field_kron_tmax_copy(main = geometry,
                                                       copy = "field_kron_tmax",
                                                       group = time,
                                                       group_mapper = bru_mapper(mesh_temporal_tmax),
                                                       hyper = list(beta = list(prior = "normal",
                                                                                param = c(0, 0.1), fixed = FALSE))))

obs_model_lgcp <- bru_obs(formula = geometry + time ~  Intercept + effect_evi + effect_lai + field_kron_tmax_copy,
                    family = "cp",
                    data = fires,
                    ips = ips)

cmp_lgcp <- c(cmp_intercept, cmp_evi, cmp_lai, cmp_tmax_copy)
#-------------------------------------------------------------------------------
fit_tmax <- readFile("fit_tmax_temp_mesh_k30d1_2016_spat_mesh_2_kron.rds")
tmax_mode_hyper <- fit_tmax$mode$theta
tmax_mode_latent <- fit_tmax$mode$x
rm(fit_tmax);gc() # size of fit object can be large

fit <- bru(components = c(cmp_lgcp, cmp_tmax),
           obs_model_lgcp,
           obs_model_tmax,
           options = list(control.mode = list(theta = tmax_mode_hyper,
                                              x = tmax_mode_latent,
                                              restart = TRUE)))

saveRDS(fit, path(destination_dir, paste0(basename(int_pts_name), "_1.rds")))
message("Saved model")