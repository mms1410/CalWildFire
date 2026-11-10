library(here)
library(sf)
library(lubridate)
library(fmesher)
library(dplyr)
library(ggplot2)
library(fs)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils", "utils_io.R"))
source(path(here(), "R", "utils", "utils_mesh.R"))
source(path(here(), "R", "utils", "utils_geocomp.R"))
source(path(here(), "R", "plots", "defaults.R"))
#-------------------------------------------------------------------------------
set.seed(123)
config_data <- data <- readYaml("data")
train_years <- seq(config_data[["start_train"]], config_data[["end_train"]])

config <- readYaml("mesh")
crs_km <- calAlbersKm()
ca_mld <- readFile("ca_mld.gpkg") |>
  st_transform(crs_km)

destination_dir <- dir_create(Reduce(path, config$destination_dir, init = here()))
saveMesh <- function(mesh, name)  saveRDS(mesh, path(destination_dir, paste0(name, ".rds")))
#-------------------------------------------------------------------------------
filterCroppedMld <- function(sf_points, dist = -20) {
  
  ca_mld_crop <- st_buffer(ca_mld, dist = dist)
  
  sf_points |>
    filter(stWithinLogical(geometry, ca_mld_crop))
}



.debugPlotMesh <- function(mesh) {
  ggplot() +
    geom_fm(data = mesh, defs = list(bnd = list(linewidth = 0.2)), fill = NA) +
    geom_sf(data = ca_mld, color = "red", fill = NA) +
    labs(subtitle = getMeshInfo(mesh))
}


.debugPlotInt <- function(mesh, fires_outside) {
  
  
  outside <- getMeshNodes(mesh) |>
    filter(!stWithinLogical(geometry, ca_mld))

  plt <-  ggplot() +
    geom_sf(data = ca_mld, fill = NA, aes(color = "ca mld"), linewidth = 0.3) +
    geom_fm(data = mesh,  fill = NA, color = "grey", alpha = 0.5, linewidth = 0.1,
            defs = list(bnd = list(linewidth = 0.3, color = "forestgreen"))) +
    geom_sf(data = fires, aes(color = "fire"), alpha = 0.3, size = 0.1) +
    geom_sf(data = outside, aes(color = "outside"), size = 0.7) +
    labs(subtitle = paste0(getMeshInfo(mesh), " outside: ", nrow(outside))) +
    scale_color_manual(name   = NULL,
                       values = c("ca mld" = "green","outside" = "blue", "fire" = "black"),
                       breaks = c("ca mld", "fire", "outside"))
  plt
}
meshFactory <- function(mesh_config, bnd_inner, bnd_outer) {
    
    offset <- mesh_config$offset
    cutoff <- mesh_config$cutoff
    max_edge <- as.numeric(mesh_config$max.edge) # integer does not work here (degenerate exterior)
    
    spat_mesh <- fm_mesh_2d(boundary = bnd_inner,
                          ext = bnd_outer,
                          crs = crs_km,
                          max.edge = max_edge, 
                          offset = offset,
                          cutoff = cutoff,
                          max.n.strict = mesh_config$max.n.strict)
  .debugPlotMesh(spat_mesh)
  return(spat_mesh)
}
#===============================================================================
#                    Spatial Mesh for Met. Covariates
#===============================================================================
ca_simple <- ca_mld |> 
  st_simplify(dTolerance = config$boundary$tolerance,
              preserveTopology = TRUE) 

bnd_meteo <- fm_extensions(ca_simple,
                     convex = c(100, 250),
                     resolution = c(20, 45),
                     crs  = crs_km)

ggplot() +
  geom_sf(data = ca_mld , fill = NA, color = "red") +
  geom_sf(data = bnd_meteo[[1]], fill = NA, color = "blue") +
  geom_sf(data = bnd_meteo[[2]], fill = NA, color = "brown")


spat_configs_name <- names(config)[grepl("spat_mesh*", names(config))]
for  (config_name in spat_configs_name) {
  mesh_config <- config[[config_name]]
  spat_mesh <- meshFactory(mesh_config, bnd_meteo[[1]], bnd_meteo[[2]])
  saveMesh(spat_mesh, config_name)
}
#===============================================================================
#                    Spatial Mesh Integration Points
#===============================================================================
fires <- readFile("calfire.gpkg") |>
  filter(year(date) %in% train_years) |>
  st_transform(crs_km)


int_mesh_1 <- fm_mesh_2d(boundary = st_buffer(ca_mld, dist = -0.2),
                         crs = crs_km,
                         loc = slice_sample(fires, n = 100),
                         max.edge = 80,
                         cutoff = 40)

ggplot() +
  geom_sf(data = ca_mld, color = "red", fill = NA) +
  geom_sf(data = fires, color = "brown", size = 0.1, alpha  = 0.3) +
  geom_fm(data = int_mesh_1, fill = NA, defs = list(bnd = list(linewidth = 0.1)))

saveMesh(int_mesh_1, "int_mesh_1")
#-------------------------------------------------------------------------------
int_mesh_2 <- fm_mesh_2d(boundary = st_buffer(ca_mld, dist = -0.2),
                         crs = crs_km,
                         loc = fires,
                         max.edge = 50,
                         cutoff = 50)

ggplot() +
  geom_sf(data = ca_mld, color = "red", fill = NA) +
  geom_sf(data = fires, color = "brown", size = 0.1, alpha  = 0.3) +
  geom_fm(data = int_mesh_2, fill = NA, defs = list(bnd = list(linewidth = 0.1)))

saveMesh(int_mesh_2, "int_mesh_2")
#-------------------------------------------------------------------------------
int_mesh_3 <- fm_mesh_2d(boundary = st_buffer(ca_mld, dist = -0.2),
                         crs = crs_km,
                         loc = fires,
                         cutoff = 50)

ggplot() +
  geom_sf(data = ca_mld, color = "red", fill = NA) +
  geom_sf(data = fires, color = "brown", size = 0.1, alpha  = 0.3) +
  geom_fm(data = int_mesh_3, fill = NA, defs = list(bnd = list(linewidth = 0.1)))

saveMesh(int_mesh_2, "int_mesh_3")
#===============================================================================
#                               Temporal Meshes
#===============================================================================
getDateSeq <- function(year_start, year_end, knots){
  # TODO: add x.12.31 always
  
  start_date <- lubridate::make_date(year_start, 1, 1)
  end_date <- lubridate::make_date(year_end, 12, 31)
  
  if (inherits(knots, "character")) {
    return(seq.Date(start_date, end_date, by = knots))
  }
  
  days_range <- as.numeric(end_date - start_date)
  steps <- as.integer(max(1, round(days_range / (knots - 1))))
  nominal_steps <- days_range / max(1, (knots - 1))
  
  date_seq <- seq.Date(start_date, end_date, by = paste(steps, "days"))
  checkmate::assertIntegerish(diff(as.numeric(date_seq)))
  return(date_seq)
}

temp_mesh_k10d1_2011 <- fm_mesh_1d(as.numeric(getDateSeq(2004, 2011, 10)), degree = 1)
temp_mesh_k10d2_2011 <- fm_mesh_1d(as.numeric(getDateSeq(2004, 2011, 10)), degree = 2)

temp_mesh_k10d1_2016 <- fm_mesh_1d(as.numeric(getDateSeq(2004, 2016, 10)), degree = 1)
temp_mesh_k10d2_2016 <- fm_mesh_1d(as.numeric(getDateSeq(2004, 2016, 10)), degree = 2)

temp_mesh_k30d1_2016 <- fm_mesh_1d(as.numeric(getDateSeq(2004, 2016, 30)), degree = 1)
temp_mesh_k30d2_2016 <- fm_mesh_1d(as.numeric(getDateSeq(2004, 2016, 30)), degree = 2)

temp_mesh_k50d1_2016 <- fm_mesh_1d(as.numeric(getDateSeq(2004, 2016, 50)), degree = 1)
temp_mesh_kd0d2_2016 <- fm_mesh_1d(as.numeric(getDateSeq(2004, 2016, 50)), degree = 2)

temp_mesh_k20d2 <- fm_mesh_1d(as.numeric(getDateSeq(2004, 2024, 20)), degree = 2)
temp_mesh_k20d1 <- fm_mesh_1d(as.numeric(getDateSeq(2004, 2024, 20)), degree = 1)

temp_mesh_k30d2 <- fm_mesh_1d(as.numeric(getDateSeq(2004, 2024, 30)), degree = 2)
temp_mesh_k30d1 <- fm_mesh_1d(as.numeric(getDateSeq(2004, 2024, 30)), degree = 1)


tmp_meshes <- ls(pattern = "^temp_mesh_*")
lapply(tmp_meshes, function(mesh_name) saveMesh(get(mesh_name), mesh_name))
