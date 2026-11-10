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

destination_dir <- dir_create(path(here(), "assets", "fits"))
cache_dir <- dir_create(path(here(), "data", "cache"))
#-------------------------------------------------------------------------------
config <- readYaml("lgcp")

mesh_name_temp_int <- config$mesh_temp$integration
mesh_name_spat_int <- config$mesh_spat$integration

mesh_temp_int <- readFile(path("assets","mesh", paste0(mesh_name_temp_int, ".rds")))
mesh_spat_int <- readFile(path("assets","mesh", paste0(mesh_name_spat_int, ".rds")))

mesh_spat_tmax <- readFile(path("assets","mesh", config$mesh_spatial$tmax))
mesh_temp_tmax <- readFile(path("assets", "mesh", config$mesh_temp$tmax))

mesh_spat_vpdmax <- readFile(path("assets", "mesh", config$mesh_spatial$vpdmax))
mesh_temp_vpdmax <- readFile(path("assets", "mesh", config$mesh_temp$vpdmax))

saveModel <- saveModelFactory(destination_dir, paste0("lgcp_", mesh_name_temp_int, "_", mesh_name_spat_int))
cache_name <- paste0("lgcp_", mesh_name_temp_int, "_", mesh_name_spat_int, "_2011")
#===============================================================================
#                              Data
#===============================================================================
#' fires <- readFile("calfire.gpkg") |>
#'   filter(year(date) <= 2015) |>
#'   mutate(time = as.numeric(date)) |>
#'   addRoadDistance() |>
#'   addRastToSfPoint("evi.tif", "evi") |>
#'   addRastToSfPoint("lai.tif", "lai") |>
#'   mutate(time = as.numeric(date)) |>
#'   st_transform(crs_km)
#' st_write(fires, path(here(), "data", "cache", "fires_2015.gpkg"), append = FALSE)
fires <- readFile("fires_2015.gpkg")


# tmax <- readFile("tmax_5km1d.csv")[year(date) <= 2010][sample(.N, config$tmax$n_sample)] |>
#   st_as_sf(coords = c("lon", "lat"), crs = st_crs(4326)) |>
#   mutate(time = as.numeric(date)) |>
#   addEcoz() |>
#   addCoastDistance() |>
#   addRastToSfPoint("dem.tif", "elevation") |>
#   st_transform(crs_km)
# st_write(tmax, path(cache_dir, "tmax_2010.gpkg"), append = FALSE)

tmax <- readFile("tmax_2016.gpkg")
tmax <- tmax |>
  mutate(value = (value - mean(value) / sd(value)))

# vpdmax <- readFile("tmax_5km1d.csv")[year(date) <= 2010][sample(.N, config$tmax$n_sample)] |>
#   st_as_sf(coords = c("lon", "lat"), crs = st_crs(4326)) |>
#   mutate(time = as.numeric(date)) |>
#   addEcoz() |>
#   addCoastDistance() |>
#   addRastToSfPoint("dem.tif", "elevation") |>
#   st_transform(crs_km)
# st_write(vpdmax, path(cache_dir, "vpdmax_2010.gpkg"), append = FALSE)

# vpdmax <- readFile("vpdmax_2010.gpkg")
# vpdmax <- vpdmax |>
#   mutate(value = (value - mean(value) / sd(value)))
#===============================================================================
#                              SPDE Specification
#===============================================================================
plot(mesh_spat_tmax)
meshInfoPriorRange(mesh_spat_tmax)
spde_tmax <- inla.spde2.pcmatern(mesh_spat_tmax,
                              prior.sigma = c(1, 0.05),
                              prior.range = c(700, 0.05))
plot(mesh_spat_vpdmax)
meshInfoPriorRange(mesh_spat_vpdmax)
spde_vpdmax <- inla.spde2.pcmatern(mesh_spat_vpdmax,
                                   prior.sigma = c(1, 0.05),
                                   prior.range = c(700, 0.05))
#===============================================================================
#                              Integration Points
#===============================================================================
plot(mesh_spat_int)
int_pts_name <- paste0("int_", mesh_name_temp_int, "_", mesh_name_spat_int, ".rds")

checkmate::assert(fm_crs(mesh_spat_int) == st_crs(ca_mld))
int_mesh <- fmesher::fm_int(domain  = list(geometry = mesh_spat_int, time  = mesh_temp_int),
                            samplers = ca_mld)
int_pts <- int_mesh |>
  mutate(date = as.Date(time)) |>
  filter(year(date) <= 2010) |>
  addRastToSfPoint("evi.tif", "evi") |> # coastline na
  addRastToSfPoint("lai.tif", "lai") |> # no na
  addRoadDistance()
saveRDS(int_pts, path(cache_dir, paste0(int_pts_name, ".rds"))) # .block col not compatible with sf
# int_pts <- readRDS(path(cache_dir, paste0(int_pts_name, ".rds")))

int_pts_outsiders <- int_pts |>
  filter(!stWithinLogical(geometry, ca_mld) & time == 12418) |>
  select(geometry)

colSums(is.na(fires))
colSums(is.na(int_pts))

evi_nas <- int_pts |>
  filter(is.na(evi))

lai_nas <- int_pts |>
  filter(is.na(lai))

# int_pts <- int_pts |>
#   mutate(lai = tidyr::replace_na(lai, 0),
#          evi = tidyr::replace_na(evi, 0))

ggplot() +
  geom_sf(data = st_union(fm_as_sfc(mesh_spat_int)), color = "blue", fill = NA) +
  geom_sf(data = ca_mld, fill = NA) +
  geom_sf(data = int_pts_outsiders, color = "black", shape = 13) +
  geom_sf(data = evi_nas, color = "orange", shape = 2) +
  geom_sf(data = lai_nas, color  = "red", shape = 14)
#-------------------------------------------------------------------------------
fires$distance_road <- log1p(fires$distance_road)
int_pts$distance_road <- log1p(int_pts$distance_road)

fires$lai <- log1p(fires$lai)
int_pts$distance_road <- log1p(int_pts$distance_road)

n_region <- length(unique(tmax$region))
graph_ecoz <- getEcozGraph()
#-------------------------------------------------------------------------------
cmp_intercept_tmax <- initCmpIntercept("tmax")
cmp_region_tmax <- initCmpRegionBYM("tmax")
cmp_elevation_tmax <- initCmpElevation("tmax")
cmp_coast_distance_tmax <- initCmpCoastDistance("tmax")

cmp_field_tmax <- bru_comp_list(~ -1 + field_sep_tmax(main = geometry,
                                                  model = spde_tmax,
                                                  group = time,
                                                  group_mapper = bru_mapper(mesh_temp_tmax)))

cmp_tmax <- c(cmp_intercept_tmax,
              cmp_region_tmax,
              cmp_elevation_tmax,
              cmp_coast_distance_tmax,
              cmp_field_tmax)

obs_model_tmax <- bru_obs(value ~ -1 +  Intercept_tmax + effect_region_tmax + effect_elevation_tmax +
                            effect_coast_distance_tmax + field_sep_tmax,
                          family = "gaussian",
                          data = tmax,
                          control.family = list(hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))))

# "sanity check"
# fit_tmax <- bru(components = cmp_tmax, obs_model_tmax)
#-------------------------------------------------------------------------------
cmp_intercept <- initCmpIntercept()
cmp_lai <- bru_comp_list(~ -1 + effect_lai(lai, model = "linear"))
cmp_evi <- bru_comp_list(~ -1 + effect_evi(evi, model = "linear"))
cmp_tmax_copy <- bru_comp_list( ~ -1 + field_sep_tmax_copy(main = geometry,
                                                       copy = "field_sep_tmax",
                                                       group = time,
                                                       group_mapper = bru_mapper(mesh_temp_tmax),
                                                       hyper = list(beta = list(prior = "normal",
                                                                                param = c(0, 0.1), fixed = FALSE))))

obs_model_lgcp <- bru_obs(formula = geometry + time ~  Intercept + effect_evi + effect_lai + field_sep_tmax_copy,
                    family = "cp",
                    data = fires,
                     ips = int_pts)

cmp_lgcp <- c(cmp_intercept, cmp_evi, cmp_lai, cmp_tmax_copy)


fit_tmax <- readFile("fit_tmax_temp_mesh_k30d1_2016_spat_mesh_2_kron.rds")
tmax_mode_hyper <- fit_tmax$mode$theta
tmax_mode_latent <- fit_tmax$mode$x
rm(fit_tmax);gc()

fit <- bru(components = c(cmp_lgcp, cmp_tmax),
           obs_model_lgcp,
           obs_model_tmax,
           options = list(control.mode = list(theta = tmax_mode_hyper,
                                              x = tmax_mode_latent,
                                              restart = TRUE)))

saveRDS(fit, path(destination_dir, paste0(basename(int_pts_name), "_1.rds")))
message("Saved model")