library(here)
library(fs)
library(sf)
library(fmesher)
library(data.table)
library(dplyr)
library(inlabru)
library(INLA)
library(dplyr)
library(INLAspacetime)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils", "utils_io.R"))
source(path(here(), "R", "utils", "utils_mesh.R"))
source(path(here(), "R", "utils", "utils_analysis.R"))
source(path(here(), "R", "utils", "utils_geocomp.R"))
source(path(here(), "R", "analysis", "components.R"))
#-------------------------------------------------------------------------------
set.seed(123)

paths <- readYaml("paths")
general <- readYaml("general")
config <- readYaml("fields")[["vpdmax"]]

train_years <- seq(general$start_train, general$end_train)

crs_km <- calAlbersKm()
ca_mld <- readFile("ca_mld.gpkg") |>
  st_transform(crs_km)
graph_ecoz <- readFile("ecoz_mld.adj")

destination_dir <- reduceYamlPath(paths$dir_fits)
cache_dir <- reduceYamlPath(paths$dir_cache)
data_dir <- reduceYamlPath(paths$dir_grid)
#-------------------------------------------------------------------------------
bru_options_set(verbose = TRUE,
                bru_verbose = 3,
                num.threads = "1:1",
                control.inla = list(strategy = "simplified.laplace", int.strategy = "ccd", diagonal = 1e-4),
                control.compute = list(dic = TRUE, waic = TRUE, mlik = TRUE, config = TRUE))

checkmate::assertList(config)
checkmate::assert("n_sample" %in% names(config))
checkmate::assert("mesh_temp" %in% names(config))
checkmate::assert("mesh_spat" %in% names(config))
checkmate::assert("data" %in% names(config))

mesh_name_temp <- config$mesh_temp
mesh_name_spat <- config$mesh_spat
config_spec <- paste0("vpdmax", "_", mesh_name_temp, "_", mesh_name_spat)
saveModel <- saveModelFactory(destination_dir, config_spec)
#-------------------------------------------------------------------------------
mesh_temporal <- readFile(path("mesh", paste0(mesh_name_temp, ".rds")))
mesh_spatial <- readFile(path("mesh", paste0(mesh_name_spat, ".rds")))

if (file_exists(path(cache_dir, config_spec))) {
  vpdmax <- readFile(path(cache_dir,  config_spec))
} else {
  vpdmax <- readFile(path(data_dir, data)) |>
    filter(year(date) %in% train_years)
  vpdmax <- vpdmax[sample(.N, config$n_sample)]
  
  vpdmax <- st_as_sf(vpdmax, coords = c("lon", "lat"), crs = 4326) |>
    addCoastDistance() |>
    addEcoz() |>
    addRastToSfPoint("dem.tif", "elevation") |>
    st_transform(crs_km) |>
    mutate(time = as.numeric(date))
  st_write(vpdmax, path(cache_dir, paste0(config_spec, ".gpkg")))
}
#-------------------------------------------------------------------------------
vpdmax <- vpdmax |>
  mutate(value = (value - mean(value)) / sd(value),
         distance_coast = distance_coast)
n_region <- length(unique(vpdmax$region))

cmp_intercept <- initCmpIntercept(variable_name)
cmp_elevation <- initCmpElevation(variable_name)
cmp_coast_distance <- initCmpCoastDistance(variable_name)
cmp_region <- initCmpRegionBYM(variable_name)

cmp_base <- c(cmp_intercept, cmp_elevation, cmp_coast_distance, cmp_region)
form_base <- as.formula(value ~ Intercept_vpdmax + effect_elevation + effect_coast_distance_vpdmax + effect_region_vpdmax)
#-------------------------------------------------------------------------------
# separable kronecker model
spde_sep <- inla.spde2.pcmatern(mesh_spatial,
                                prior.sigma = unlist(config$prior_sigma),
                                prior.range = unlist(config$prior_range))


cmp_field_sep <- bru_comp_list(~ -1 + field_sep_vpdmax(main = geometry,
                                                       model = spde_sep,
                                                       group = time,
                                                       group_mapper = bru_mapper(mesh_temporal)))

fit_sep <- bru(components = c(cmp_base, cmp_field_sep),
               bru_obs(formula = update(form_base, . ~ . + field_sep_vpdmax),
                       family = "gaussian",
                       data = vpdmax))

message("save model kron...")
saveModel(fit_sep, "kron")
#-------------------------------------------------------------------------------
priors_spacetime <- list(prs = unlist(config$prior_range), prt = unlist(config$prior_temp), psigma = unlist(config$prior_sigma))

# separable diffusion
spde_102 <- initSeperableSPDE(mesh_spatial, mesh_temporal, priors_spacetime)

# critical diffusion
spde_121 <- initCriticalDiffusionSPDE(mesh_spatial, mesh_temporal, priors_spacetime)

# separable order2
spde_202 <- initSperable2SPDE(mesh_spatial, mesh_temporal, priors_spacetime)

# iterated diffusion
spde_220 <- initInteratedDiffusionSPDE(mesh_spatial, mesh_temporal, priors_spacetime)


cmp_field_102 <- bru_comp_list(~ -1 + field_102_vpdmax(main = list(space = geometry, time = time),
                                                     model = spde_102))
cmp_field_121 <- bru_comp_list(~ -1 + field_121_vpdmax(main = list(space = geometry, time = time),
                                                     model = spde_121))
cmp_field_202 <- bru_comp_list(~ -1 + field_202_vpdmax(main = list(space = geometry, time = time),
                                                     model = spde_202))
cmp_field_220 <- bru_comp_list(~ -1 + field_220_vpdmax(main = list(space = geometry, time = time),
                                                     model = spde_220))
#-------------------------------------------------------------------------------
fit_102 <- bru(components = c(cmp_base, cmp_field_102),
               bru_obs(formula = update(form_base, . ~ . + field_102_vpdmax),
                       family = "gaussian",
                       data = vpdmax))

message("save model sDiff...")
saveModel(fit_102, "sDiff")

fit_121 <- bru(components = c(cmp_base, cmp_field_121),
               bru_obs(formula = update(form_base, . ~ . + field_121_vpdmax),
                       family = "gaussian",
                       data = vpdmax))

message("save model cDiff...")
saveModel(fit_121, "cDiff")

fit_202 <- bru(components = c(cmp_base, cmp_field_202),
               bru_obs(formula = update(form_base, . ~ . + field_202_vpdmax),
                       family = "gaussian",
                       data = vpdmax))

message("save model s2Diff")
saveModel(fit_202, "s2Diff")

fit_220 <- bru(components = c(cmp_base, cmp_field_220),
               bru_obs(formula = update(form_base, . ~ . + field_220_vpdmax),
                       family = "gaussian",
                       data = vpdmax))

message("save model iDiff")
saveModel(fit_220, "iDiff")
