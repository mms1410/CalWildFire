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
#-------------------------------------------------------------------------------
source("R/const.R")
source("R/utils.R")
source("R/plots/utils.R")
config <- readYaml()
train_start <- as.Date(paste0(config$start_train, "-01-01"))
train_end <- as.Date(paste0(config$end_train, "-12-31"))

config <- readYaml("mesh")
source_dir <- path(here(), "data", "preprocessed")
destination_dir_mesh <- dir_create(path(here(), "assets", "mesh"))
destination_dir_plot <- dir_create(path(here(), "assets", "plots", "mesh"))
destination_sd <- dir_create(path(here(), "assets", "tif_train"))
variable_names <- c("vs", "ppt", "vpdmax", "tmax")

boundary <- st_simplify(st_union(CA_MLD), dTolerance = config$dtolerance)
bnd <- fm_as_segm(boundary)
set.seed(123)
#-------------------------------------------------------------------------------
get_or_load_sdrast <- function(variable_name, destination_dir, source_dir = path(here(), "data", "preprocessed")) {
  raster_sd_path <- path(destination_dir, paste0(variable_name, "_sd.tif"))
  
  if (file_exists(raster_sd_path)) {
    return(rast(raster_sd_path))
  }
  cat("   create pixelwise temporal stdev...\n")
  raster <- rast(path(source_dir, paste0(variable_name, ".tif")))
  raster <- subset_raster(raster,
                          date_start = train_start,
                          date_end = train_end)
  raster_sd <- app(raster, fun = "sd")
  dir_create(destination_dir)
  writeRaster(raster_sd, raster_sd_path)
  return(raster_sd)
}
get_rast_and_pts <- function(variable_name, sd_folder , samplesize = 1000) {
  
  rast_sd <- get_or_load_sdrast(variable_name, sd_folder)
  rast_var <- rast_sd^2
  
  pts_sd <- spatSample(rast_sd, size = samplesize, method = "weights",
                       as.points = TRUE, values = FALSE, exhaustive = TRUE)
  pts_var <- spatSample(rast_var, size = samplesize, method = "weights",
                        as.points = TRUE, values = FALSE, exhaustive = TRUE)
  
  return(list(rast_sd = rast_sd, rast_var = rast_var,
              pts_sd = as_sf(pts_sd), pts_var = as_sf(pts_var)))
}

get_mesh_info <- function(mesh) {
  v <- mesh$n
  t <- nrow(mesh$graph$tv )
  n_sample <- length(mesh$idx$loc)
  paste0("(Samples:", n_sample, ", Vertices:", v,", Triangles:", t, ")")
}

plot_meshes <- function(mesh_sd, mesh_var, pts_sd, pts_var, rast_sd, rast_var, spatial_range) {
  
  
  mesh_sd_info <- get_mesh_info(mesh_sd)
  mesh_var_info <- get_mesh_info(mesh_var)
  
  plot_rast_sd <- ggplot() +
    geom_spatraster(data = rast_sd) +
    scale_fill_continuous(na.value = NA, name = expression(sigma[R])) +
    geom_sf(data = pts_sd, color = "red", size = 0.4, alpha = 0.6) +
    theme(legend.position = "left",
          axis.text = element_blank())
  
  plot_rast_var <- ggplot() +
    geom_spatraster(data = rast_var) +
    scale_fill_continuous(na.value = NA, name = expression(sigma[R]^2)) +
    geom_sf(data = pts_var, color = "red", size = 0.4, alpha = 0.6) +
    theme(legend.position = "left",
          axis.text = element_blank())
  
  plot_mesh_sd <- ggplot() +
    geom_fm(data = mesh_sd) +
    theme(axis.text = element_blank())
  
  plot_mesh_var <- ggplot() +
    geom_fm(data = mesh_var) +
    theme(axis.text = element_blank()) 
  
  plot_qual_sd <- plot_mesh_quality(mesh_sd, spatial_range) +
    labs(caption = mesh_sd_info)
  plot_qual_var <- plot_mesh_quality(mesh_var, spatial_range) +
    labs(caption = mesh_var_info)
  
  plot_all <- wrap_plots(list(plot_rast_sd, plot_rast_var,
                              plot_mesh_sd, plot_mesh_var,
                              plot_qual_sd, plot_qual_var),
                         nrow = 3, ncol = 2)
}

plot_mesh_quality <- function(mesh, spatial_range) {
  
  out <- fm_assess(mesh, spatial_range)
  
  sd.dev.limits <- 1 + c(-1, 1) * max(abs(range(out$sd.dev, na.rm = TRUE) - 1))
  col.values <- 2 * seq(0, 1, length.out = 100) - 1
  col.values <- (sign(col.values) * abs(col.values)^1.5 + 1) / 2
  
  ggplot() +
    geom_tile(data = out, aes(geometry = geometry, fill = sd.dev),
              stat = "sf_coordinates") +
    coord_sf(default = TRUE) +
    theme(axis.text = element_blank(), legend.position = "left") +
    labs(x = "", y = "") +
    scale_fill_continuous(na.value = NA, name = expression(sigma[Q]))
}

parse_config_args_mesh <- function(args_list = list()) {
  
  if (all(c("max_edge_inner", "max_edge_outer") %in% names(args_list))) {
    max.edge <- c(args_list[["max_edge_inner"]], args_list[["max_edge_outer"]])
    args_list[["max_edge_inner"]] <- NULL
    args_list[["max_edge_outer"]] <- NULL
    args_list <- append(args_list, list(max.edge = max.edge))
  }
  if (all(c("offset_inner", "offset_outer") %in% names(args_list))) {
    offset <- c(args_list[["offset_inner"]], args_list[["offset_outer"]])
    args_list[["offset_inner"]] <- NULL
    args_list[["offset_outer"]] <- NULL
    args_list <- append(args_list, list(offset = offset))
  }
  
  return(args_list)
}


make_mesh <- function(rast_and_pts, args_mesh = list(), spatial_range) {
  
  checkmate::assertList(rast_and_pts, len = 4)
  checkmate::assertNames(names(rast_and_pts),
                         permutation.of = c("pts_sd", "pts_var", "rast_sd", "rast_var"))
  checkmate::assertList(args_mesh, null.ok = TRUE)
  checkmate::assertNumeric(spatial_range)
  
  pts_sd <- rast_and_pts[["pts_sd"]]
  pts_var <- rast_and_pts[["pts_var"]]
  rast_sd <- rast_and_pts[["rast_sd"]]
  rast_var <- rast_and_pts[["rast_var"]]
  
  
  args_mesh_sd <-  append(args_mesh, list(loc = pts_sd))
  args_mesh_var <- append(args_mesh, list(loc = pts_var))
  
  mesh_sd <- do.call(fm_mesh_2d, args_mesh_sd)
  mesh_var <- do.call(fm_mesh_2d, args_mesh_var)
  
  plots <- plot_meshes(mesh_sd, mesh_var, pts_sd, pts_var,
                       rast_sd, rast_var,
                       spatial_range)
  
  return(list(mesh_sd = mesh_sd, mesh_var = mesh_var,
              plots = plots, args = args_mesh))
}
#-------------------------------------------------------------------------------
for (variable_name in variable_names) {
  
  cat(paste0("Process ", variable_name, "...\n"))
  variable_config <- config$default_grid
  if (variable_name %in% names(config)) {
    variable_config <- modifyList(variable_config, config[[variable_name]])
  }
  variable_config <- lapply(variable_config, as.numeric)
  config_grid <- expand.grid(variable_config)
  
  for (idx in rownames(config_grid)) {
    current_conf <- config_grid[idx, ]
    cat(paste0("   ", paste(names(current_conf), current_conf, collapse = " ", sep = ":"), "\n"))
    
    args <- current_conf |>
      select(-c(samplesize, spatialrange)) |>
      lapply(\(x) x[[1]])
    args_mesh <- parse_config_args_mesh(args)
    args_mesh$boundary <- bnd
    
    rast_and_pts <- get_rast_and_pts(variable_name, destination_sd,
                                     samplesize = current_conf$samplesize)
    mesh_result <- make_mesh(rast_and_pts, args_mesh,
                             spatial_range = current_conf$spatialrange)
    
    conf_name <- paste0(names(args), args, collapse = "_")
    gg_save(paste0(variable_name, "_", conf_name),plt = mesh_result$plots, destination_dir = destination_dir_plot)
    saveRDS(mesh_result$mesh_sd, path(destination_dir_mesh, paste0(variable_name, conf_name, "_sd.rds")))
    saveRDS(mesh_result$mesh_var, path(destination_dir_mesh, paste0(variable_name, conf_name, "_var.rds")))
  }
}