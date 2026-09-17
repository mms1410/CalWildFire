library(fs)
library(stringr)
library(here)
library(sf)
library(terra)
library(tidyterra)
library(ggplot2)
library(patchwork)
library(fmesher)
#-------------------------------------------------------------------------------
source("R/plots/utils.R")
source_tif <- path(here(), "assets", "mesh", "tif")
source_mesh <- path(here(), "assets", "mesh", "mesh_list")
destination_dir <- path(here(), "assets", "plots", "mesh")
dir_create(destination_dir)
#-------------------------------------------------------------------------------
plot_mesh_raster <- function(mesh_list, raster, legend_name = expression(sigma)) {
  
  
  p1 <- ggplot() +
    geom_spatraster(data = raster) +
    scale_fill_continuous(na.value = NA, name = legend_name) +
    geom_sf(data = mesh_list$samples, color = "red", size = 0.5, alpha = 0.5) +
    theme(legend.position = "left",
          axis.text = element_blank())
  
  p2 <- ggplot() +
    geom_fm(data = mesh_list$mesh) +
    theme(axis.text = element_blank())
  
  p3 <- wrap_plots(list(p1, p2), nrow = 1)
  
  return(list(raster = p1, mesh = p2, combo = p3))
}

parse_mesh_info <- function(mesh) {
  v <- mesh$n
  t <- nrow(mesh$graph$tv )
  n_sample <- length(mesh$idx$loc)
  paste0("(Samples:", n_sample, ", Vertices:", v,", Triangles:", t, ")")
}
#-------------------------------------------------------------------------------
zip <- Map(list, dir_ls(source_tif), dir_ls(source_mesh))
for(variable_folder in zip) {
  
  variable_name <- basename(variable_folder[[1]])
  folders_tif <- str_subset(unlist(variable_folder), "tif")
  folders_mesh <- str_subset(unlist(variable_folder), "mesh_list")
  
  # folder name == ...<configspec>_[sd|var].[tif|rds]
  configspec_tif <- basename(path_ext_remove(dir_ls(folders_tif)))
  #configspec_tif<- str_remove(configspec_tif, "_[^_]*$")
  configspec_mesh <- basename(path_ext_remove(dir_ls(folders_mesh)))
  #configspec_mesh<- str_remove(configspec_mesh, "_[^_]*$")
  checkmate::assert(all(configspec_tif == configspec_mesh))
  
  cat(paste0("Create plots for variable ", variable_name, "...\n"))
  for (configspec in configspec_tif) {
      cat(paste0("   conf:", configspec, "\n"))
     raster <- rast(path(folders_tif, paste0(configspec, ".tif")))
     meshlist <- readRDS(path(folders_mesh,paste0(configspec, ".rds") ))
     
     if (str_detect(configspec, "sd")) {
       legend <- expression(sigma)
     } else if (str_detect(configspec, "var")) {
       legend <- expression(sigma^2)
     } else {
       stop("Expected std or var in config")
     }
     plt_rast_mesh <- plot_mesh_raster(meshlist, raster, legend)[["combo"]] +
       labs(caption = parse_mesh_info(meshlist$mesh))
     
     destination <- path(destination_dir, variable_name)
     dir_create(destination_dir)
     gg_save(configspec, plt_rast_mesh, destination)
  }
}

