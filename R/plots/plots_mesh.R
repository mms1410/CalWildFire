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
source("R/utils.R")
source("R/plots/defaults.R")
source("R/plots/utils.R")
source("R/plots/plot_functions_mesh.R")
source_dir_mesh <- path(getwd(), "assets", "mesh", "mesh2d")
source_dir_rastsd <- path(getwd(), "assets", "tif_train")
destination_dir <- dir_create(path(getwd(), "assets", "plots", "mesh2d"))
spatial_range <- 5000
#-------------------------------------------------------------------------------
for (variable_folder in dir_ls(source_dir_mesh)) {
  variable_name <- basename(variable_folder)
  if (variable_name == "fires") next
  files_meshes <- dir_ls(variable_folder, glob = "*.rds")
  files_pts <- dir_ls(variable_folder, glob = "*.gpkg")
  meshes_split <- split(files_meshes, sub("_(sd|var)\\.rds$", "", basename(files_meshes)))
  pts_split  <- split(files_pts,sub("_pts_(sd|var)\\.gpkg$", "", basename(files_pts))) 
  
  if (variable_name != "fires") {
    
    cat(paste0("Create plots for ", variable_name, "\n"))
    rast_sd <- terra::rast(path(source_dir_rastsd, paste0(variable_name, "_sd.tif")))
    rast_var <- rast_sd^2
    
    destination_dir_variable <- dir_create(destination_dir, variable_name)
    destination_rast <- dir_create(destination_dir_variable, "rast")
    destination_mesh <- dir_create(destination_dir_variable, "mesh")
    destination_qual <- dir_create(destination_dir_variable, "quality")
    
    for (mesh_name in names(meshes_split)) {
      
      mesh_sd <- readRDS(str_subset(meshes_split[[mesh_name]],"_sd\\.rds$"))
      mesh_var <- readRDS(str_subset(meshes_split[[mesh_name]],"_var\\.rds$"))
      pts_sd <- st_read(str_subset(pts_split[[mesh_name]], "_pts_sd.gpkg"))
      pts_var <- st_read(str_subset(pts_split[[mesh_name]], "_pts_var.gpkg"))
      mesh_config <- readYaml(dir_ls(path(variable_folder), glob = "*.yaml"))
      
      mesh_config <- mesh_config[[mesh_name]]
      mesh_sd_info <- get_mesh_info(mesh_sd, mesh_config)
      
      mesh_var_info <- get_mesh_info(mesh_var, mesh_config)
      sample_info <- paste0("(Samples: ",nrow(pts_sd), ")") # same vor sd and var
      info_sample <- labs(caption = sample_info)
      info_caption <- labs(caption = mesh_sd_info)
      no_info <- labs(caption = NULL)
      
      plot_rast_sd <- ggplot() +
        geom_spatraster(data = rast_sd) +
        scale_fill_continuous(na.value = NA, name = expression(sigma[R])) +
        geom_sf(data = pts_sd, color = "red", size = 0.4, alpha = 0.6) +
        theme(legend.position = "left",
              axis.text = element_blank()) + info_sample
      
      plot_rast_var <- ggplot() +
        geom_spatraster(data = rast_var) +
        scale_fill_continuous(na.value = NA, name = expression(sigma[R]^2)) +
        geom_sf(data = pts_var, color = "red", size = 0.4, alpha = 0.6) +
        theme(legend.position = "left",
              axis.text = element_blank()) + info_sample
      
      plot_mesh_sd <- ggplot() +
        geom_fm(data = mesh_sd) +
        theme(axis.text = element_blank()) + info_caption
      
      plot_mesh_var <- ggplot() +
        geom_fm(data = mesh_var) +
        theme(axis.text = element_blank()) + info_caption
      
      plot_qual_sd <- plot_mesh_quality(mesh_sd, spatial_range) + info_caption
      plot_qual_var <- plot_mesh_quality(mesh_var, spatial_range) + info_caption
      
      name_base <- paste0(variable_name,"_", mesh_name)
      
      gg_save(paste0(name_base, "_rast_sd"), plot_rast_sd,
              destination_rast)
      gg_save(paste0(name_base, "_rast_var"), plot_rast_var,
              destination_rast)
      gg_save(paste0(name_base, "_mesh_sd"), plot_mesh_sd,
              destination_mesh)
      gg_save(paste0(name_base, "_mesh_var"), plot_mesh_var,
              destination_mesh)
      gg_save(paste0(name_base, "_qual_sd"), plot_qual_sd,
              destination_qual)
      gg_save(paste0(name_base, "_qual_var"), plot_qual_var,
              destination_qual)
      
      
      plot_all <- wrap_plots(list(plot_rast_sd + no_info,
                                  plot_rast_var + no_info,
                                  plot_mesh_sd + no_info,
                                  plot_mesh_var + no_info,
                                  plot_qual_sd  + no_info,
                                  plot_qual_var + no_info),
                             nrow = 3, ncol = 2)
      
      gg_save(paste0(name_base, "_patch"), plot_all, destination_dir_variable)
    }
  } else {
    fires <- readFile(path(getwd(), "data", "preprocessed", "calfire.gpkg"))
    dir_fires <- dir_ls(source_dir_mesh, regexp = "fires")
    files_meshes <- dir_ls(dir_fires, glob = "*.rds")
    for (mesh_name_file in files_meshes) {
     mesh <- readRDS(mesh_name_file) 
     mesh_info <- get_mesh_info(mesh)
     
    plot_mesh <- ggplot() +
      geom_fm(data = mesh, size = 1.5, color = "black", alpha = 0.7) +
      geom_sf(data = fires, color = "red", size = 0.2, alpha = 0.4) +
      theme(legend.position = "left",
            axis.text = element_blank())
    
    #plot_mesh_qual <- plot_mesh_quality(mesh, spatial_range) +
    #  labs(caption = mesh_var_info)
    gg_save(paste0(variable_name,"_", mesh_name), plot_mesh, destination_dir)
    }
  }
}
