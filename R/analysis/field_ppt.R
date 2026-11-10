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
library(INLAspacetime)
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
                control.inla = list(strategy = "simplified.laplace", int.strategy = "ccd", diagonal = 1e-4),
                control.compute = list(dic = TRUE, waic = TRUE, mlik = TRUE, config = TRUE))

crs_km <- calAlbersKm()
ca_mld <- readFile("ca_mld.gpkg") |>
  st_transform(crs_km)

destination_dir <- dir_create(path(here(), "assets", "fits"))
cache_dir <- dir_create(path(here(), "data", "cache"))
config <- readYaml("fields")[["ppt"]]

mesh_name_temp <- config$mesh_temp
mesh_name_spat <- config$mesh_spat

saveModel <- saveModelFactory(destination_dir, paste0("ppt_", mesh_name_temp, "_", mesh_name_spat))
cache_name <- paste0("ppt_2025_cache", ".gpkg")
#-------------------------------------------------------------------------------
mesh_temporal <- readFile(path("mesh", paste0(mesh_name_temp, ".rds")))
mesh_spatial <- readFile(path("mesh", paste0(mesh_name_spat, ".rds")))

plot(mesh_spatial)
meshInfoPriorRange(mesh_spatial)

if (file_exists(path(cache_dir, cache_name))) {
  ppt <- readFile(path(cache_dir,  cache_name))
} else {
  ppt <- readFile(path("tables", paste0(config$table, ".csv")))
  ppt <- ppt[year(date) <= 2015][sample(.N, config$n_sample)]
  ppt <- st_as_sf(ppt, coords = c("lon", "lat"), crs = 4326) |>
    addCoastDistance() |>
    addEcoz() |>
    addRastToSfPoint("dem.tif", "elevation") |>
    st_transform(crs_km) |>
    mutate(time = as.numeric(date))
  
  st_write(ppt, path(cache_dir, cache_name))
}

ppt <- ppt |>
  mutate(distance_coast = distance_coast / 100)
#-------------------------------------------------------------------------------
n_region <- length(unique(ppt$region))
graph_ecoz <- getEcozGraph()

cmp_intercept <- initCmpIntercept("ppt")
cmp_elevation <- initCmpElevation("ppt")
cmp_coast_distance <- initCmpCoastDistance("ppt")
cmp_region <- initCmpRegionBYM("ppt")

cmp_base <- c(cmp_intercept, cmp_elevation, cmp_coast_distance, cmp_region)

# seperable kronecker model
spde_sep <- inla.spde2.pcmatern(mesh_spatial,
                                prior.sigma = c(1, 0.05),
                                prior.range = c(250, 0.05))


cmp_field_sep <- bru_comp_list(~ -1 + field_sep_ppt(main = geometry,
                                                     model = spde_sep,
                                                     group = time,
                                                     group_mapper = bru_mapper(mesh_temporal)))

fit_sep <- bru(components = c(cmp_base, cmp_field_sep),
               bru_obs(formula = value ~ Intercept_ppt + effect_elevation_ppt + effect_coast_distance_ppt + field_sep_ppt,
                       family = "tweedie",
                       data = ppt))

message("save model kron...")
saveModel(fit_sep, "kron")
#-------------------------------------------------------------------------------
priors_spacetime <- list(prs = c(350, 0.05), prt = c(5, 0.5), psigma = c(1, 0.1))

# seperable diffusion
spde_102 <- initSeperableSPDE(mesh_spatial, mesh_temporal, priors_spacetime)

# critical diffusion
spde_121 <- initCriticalDiffusionSPDE(mesh_spatial, mesh_temporal, priors_spacetime)

# seperable order2
spde_202 <- initSperable2SPDE(mesh_spatial, mesh_temporal, priors_spacetime)

# iterated diffusion
spde_220 <- initInteratedDiffusionSPDE(mesh_spatial, mesh_temporal, priors_spacetime)


cmp_field_102 <- bru_comp_list(~ -1 + field_102_ppt(main = list(space = geometry, time = time),
                                                     model = spde_102))
cmp_field_121 <- bru_comp_list(~ -1 + field_121_ppt(main = list(space = geometry, time = time),
                                                     model = spde_121))
cmp_field_202 <- bru_comp_list(~ -1 + field_202_ppt(main = list(space = geometry, time = time),
                                                     model = spde_202))
cmp_field_220 <- bru_comp_list(~ -1 + field_220_ppt(main = list(space = geometry, time = time),
                                                     model = spde_220))
#-------------------------------------------------------------------------------
fit_102 <- bru(components = c(cmp_base, cmp_field_102),
               bru_obs(formula = value ~ Intercept_ppt + effect_elevation_ppt + effect_coast_distance_ppt + field_102_ppt,
                       family = "gaussian",
                       data = ppt))

message("save model sDiff...")
saveModel(fit_102, "sDiff")

fit_121 <- bru(components = c(cmp_base, cmp_field_121),
               bru_obs(formula = value ~ Intercept_ppt + effect_elevation_ppt + effect_coast_distance_ppt + field_121_ppt,
                       family = "gaussian",
                       data = ppt))

message("save model cDiff...")
saveModel(fit_121, "cDiff")

fit_202 <- bru(components = c(cmp_base, cmp_field_202),
               bru_obs(formula = value ~ Intercept_ppt + effect_elevation_ppt + effect_coast_distance_ppt + field_202_ppt,
                       family = "gaussian",
                       data = ppt))

message("save model s2Diff")
saveModel(fit_202, "s2Diff")

fit_220 <- bru(components = c(cmp_base, cmp_field_220),
               bru_obs(formula = value ~ Intercept_ppt + effect_elevation_ppt + effect_coast_distance_ppt + field_220_ppt,
                       family = "gaussian",
                       data = ppt))

message("save model iDiff")
saveModel(fit_220, "iDiff")
