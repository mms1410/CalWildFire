suppressPackageStartupMessages({
library(fs)
library(here)
library(sf)
library(terra)
library(ggplot2)
library(patchwork)
library(tidyterra)
library(INLA)
library(inlabru)
library(fmesher)
})
#-------------------------------------------------------------------------------
source("R/utils.R")
source("R/analysis/functions_mesh2d.R")
set.seed(123)
#-------------------------------------------------------------------------------
config_data <- readYaml("data")
config_mesh <- readYaml("mesh")
variables <- config_mesh[["variables"]]
config_mesh <- config_mesh$mesh_spat

cal_crs <- st_crs(config_data$crs)
cal_mld <- readFile(path(getwd(), "data", "assets", "cal_mld.gpkg"))
train_start <- as.Date(paste0(config_data$start_train, "-01-01"))
train_end <- as.Date(paste0(config_data$end_train, "-12-31"))

source_dir <- path(here(), "data", "preprocessed")
destination_dir <- dir_create(path(here(), "assets", "mesh", "mesh2d"))
sd_folder <- dir_create(path(here(), "assets", "tif_train"))

boundary_inner <- readFile(path(getwd(), "data", "assets", "mesh_boundary_ca.gpkg"))
boundary_inner <- fm_as_segm(boundary_inner)

boundary_total <- fm_extensions(
  boundary_inner,
  convex  = c(100e3, 240e3),   # expand 40–90 km
  concave = c(80e3, 100e3)    # controls smoothness
)
#-------------------------------------------------------------------------------
for (variable_name in variables) {
  cat(paste0("Create meshes for ", variable_name, "...\n"))
  if (variable_name %in% names(config_mesh)) {
    variable_configs <- config_mesh[[variable_name]]
  } else {
    variable_configs <- config_mesh[["default"]]
  }
  
  destination_variable <- dir_create(path(destination_dir, variable_name))
  # sample points based on temporal variation (stdev or variance) per pixel
  raster_sd <- get_or_load_sdrast(variable_name, sd_folder)
  if (variable_name == "fires") {
    for (mesh_name in names(variable_configs)) {
      
      current_config <- variable_configs[[mesh_name]]
      args_fm <- current_config$args_fm
      args_fm$boundary <- list(boundary_inner, boundary_buffer)
      args_fm$loc <- sf::st_coordinates(fires)
      args_fm$crs <- cal_crs
      
      fires <- readFile(path(getwd(),"data", "preprocessed", "calfire.gpkg"))
      mesh_fires <- do.call(fm_mesh_2d, args_fm)
      
      saveRDS(mesh_fires, path(destination_variable, paste0(mesh_name, ".rds")))
    }
    dumpYaml(variable_configs, path(destination_variable, paste0("config.yaml")))
    next
  }
  for (mesh_name in names(variable_configs)) {
      current_config <- variable_configs[[mesh_name]]
      samplesize <- current_config$samplesize
      rast_and_pts <- get_rast_and_pts(variable_name, sd_folder, samplesize)
      
      args_fm <- current_config$args_fm
      args_fm$crs <- cal_crs
      args_fm$boundary <- boundary_total
      
      args_fm_var <- args_fm
      args_fm_var$loc <- rast_and_pts$pts_var
      mesh_var <- do.call(fm_mesh_2d, args_fm_var)
      
      args_fm_sd <- args_fm
      args_fm_sd$loc <- rast_and_pts$pts_sd
      mesh_sd <- do.call(fm_mesh_2d, args_fm_sd)
      ggplot2::ggplot() + geom_fm(data = mesh_sd)
      mesh_sd$n
    
      saveRDS(mesh_var, path(destination_variable, paste0(mesh_name, "_var.rds")))
      saveRDS(mesh_sd, path(destination_variable, paste0(mesh_name, "_sd.rds")))
      st_write(rast_and_pts$pts_var, path(destination_variable, paste0(mesh_name, "_pts_var.gpkg")),
               append = FALSE)
      st_write(rast_and_pts$pts_sd, path(destination_variable, paste0(mesh_name, "_pts_sd.gpkg")),
               append = FALSE)
    }
  dumpYaml(variable_configs, path(destination_variable, paste0("config.yaml")))
}

