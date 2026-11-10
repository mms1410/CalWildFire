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

args_commandline <- commandArgs(trailingOnly = TRUE)
variable_name <- args_commandline[1]

checkmate::assertChoice(variable_name, c("ppt", "vpdmax", "tmax", "vs"))

paths <- readYaml("paths")
general <- readYaml("general")
conf <- readYaml("fields")
conf <- conf[[variable_name]]

train_years <- seq(general$start_train, general$end_train)

crs_km <- calAlbersKm()
ecoz <- readFile("ecoz_mld.gpkg")
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

checkmate::assertList(conf)
checkmate::assert("n_sample" %in% names(conf))
checkmate::assert("mesh_temp" %in% names(conf))
checkmate::assert("mesh_spat" %in% names(conf))
checkmate::assert("data" %in% names(conf))

mesh_name_temp <- conf$mesh_temp
mesh_name_spat <- conf$mesh_spat

mesh_temporal <- readFile(path("mesh", paste0(mesh_name_temp, ".rds")))
mesh_spatial <- readFile(path("mesh", paste0(mesh_name_spat, ".rds")))

conf_spec <- paste0(variable_name, "_", mesh_name_temp, "_", mesh_name_spat)
saveModel <- saveModelFactory(destination_dir, conf_spec)
#-------------------------------------------------------------------------------
# cache data
if (file_exists(path(cache_dir, paste0(conf_spec, ".gpkg")))) {
  geodata <- readFile(path(cache_dir, paste0(conf_spec, ".gpkg")))
} else {
  geodata <- readFile(conf$data) |>
    filter(year(date) %in% train_years)
  
  geodata <- geodata[sample(.N, conf$n_sample)] |>
    addEcozTbl(st_crs(ecoz), ecoz = ecoz) |>
    addCoastDistanceTbl(st_crs(ecoz)) |>
    addRastToTbl(st_crs(ecoz), "dem.tif", "elevation") |>
    st_as_sf(coords = c("x", "y"), crs = st_crs(ecoz)) |>
    st_transform(crs_km) |>
    mutate(time = as.numeric(date))

  st_write(geodata, path(cache_dir, paste0(conf_spec, ".gpkg")), append = FALSE)
}
#-------------------------------------------------------------------------------
geodata <- geodata |>
  mutate(value = (value - mean(value)) / sd(value),
         distance_coast = distance_coast)
n_region <- length(unique(geodata$region))

cmp_intercept <- initCmpIntercept(variable_name)
cmp_elevation <- initCmpElevation(variable_name)
cmp_coast_distance <- initCmpCoastDistance(variable_name)
# TODO: int graph <-> char region
# cmp_region <- initCmpBesag(variable_name)

cmp_base <- c(cmp_intercept, cmp_elevation, cmp_coast_distance)
form_base <- reformulate(paste0(c("Intercept", "effect_elevation", "effect_coast_distance"), "_", variable_name),
                         response = "value")
#-------------------------------------------------------------------------------
# separable kronecker model
spde_kron <- inla.spde2.pcmatern(mesh_spatial,
                                prior.sigma = unlist(conf$prior_sigma),
                                prior.range = unlist(conf$prior_range))

cmp_field_kron <- initKroneckerField(variable_name)

cmp_field_kron <- bru_comp_list(~ -1 + field_kron_vpdmax(main = geometry, 
                                                  model = spde_kron,
                                                  group = time,
                                                  group_mapper = bru_mapper(mesh_temporal, indexed = TRUE),
                                                  control.group = list(model = "ar1")))

form <-  update(form_base, as.formula(sprintf("~. + field_kron_%s", variable_name)))
fit_sep <- bru(components = c(cmp_base, cmp_field_kron),
               bru_obs(formula = form_base,
                       family = conf$family,
                       data = geodata))

message("save model kron...")
saveModel(fit_sep, "kron")
#-------------------------------------------------------------------------------
priors_spacetime <- list(prs = unlist(conf$prior_range),
                         prt = unlist(conf$prior_temp),
                         psigma = unlist(conf$prior_sigma))
#-------------------------------------------------------------------------------
# separable diffusion

spde_102 <- initSeperableSPDE(mesh_spatial, mesh_temporal, priors_spacetime)
cmp_field_102 <- initCmpDiffusionField(variable_name, "102")

form <- update(form_base, as.formula(sprintf("~. + field_102_%s", variable_name)))
fit_102 <- bru(components = c(cmp_base, cmp_field_102),
               bru_obs(formula = form,
                       family = conf$family,
                       data = geodata))

message("save model sDiff...")
saveModel(fit_102, "sDiff")
#-------------------------------------------------------------------------------
# critical diffusion

spde_121 <- initCriticalDiffusionSPDE(mesh_spatial, mesh_temporal, priors_spacetime)
cmp_field_121 <- initCmpDiffusionField(variable_name, "121")

fit_121 <- bru(components = c(cmp_base, cmp_field_121),
               bru_obs(formula = update(form_base, as.formula(sprintf("~. + field_121_%s", variable_name))),
                       family = conf$family,
                       data = geodata))

message("save model cDiff...")
saveModel(fit_121, "cDiff")
#-------------------------------------------------------------------------------
# separable diffusion order2

spde_202 <- initSperable2SPDE(mesh_spatial, mesh_temporal, priors_spacetime)
cmp_field_202 <- initCmpDiffusionField(variable_name, "202")

fit_202 <- bru(components = c(cmp_base, cmp_field_202),
               bru_obs(formula = update(form_base, as.formula(sprintf("~. + field_202_%s", variable_name))),
                       family = conf$family,
                       data = geodata))

message("save model s2Diff")
saveModel(fit_202, "s2Diff")
#-------------------------------------------------------------------------------
# iterated diffusion

spde_220 <- initInteratedDiffusionSPDE(mesh_spatial, mesh_temporal, priors_spacetime)
cmp_field_220 <- initCmpDiffusionField(variable_name, "220")

fit_220 <- bru(components = c(cmp_base, cmp_field_220),
               bru_obs(formula = update(form_base, as.formula(sprintf("~. + field_220_%s", variable_name))),
                       family = conf$family,
                       data = geodata))

message("save model iDiff")
saveModel(fit_220, "iDiff")
#-------------------------------------------------------------------------------




