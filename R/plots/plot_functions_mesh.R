#'
#'
#'
#'
get_mesh_info <- function(mesh, mesh_config) {
  v <- mesh$n
  t <- nrow(mesh$graph$tv )
  n_sample <- length(mesh$idx$loc)
  config_info <- paste0(", max.edge:", paste0("(",paste0(mesh_config$args_fm$max.edge, collapse = ","), ")"),
                        ", offset:", paste0("(", paste0(mesh_config$args_fm$offset, collapse = ","), ")"),
                        ", cutoff:", mesh_config$args_fm$cutoff)
  info_string <- paste0(c("Samples:", n_sample, ", Vertices:", v,", Triangles:", t, config_info), collapse = "", sep = "")
  return(info_string)
}

#'
#'
#'
#'
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

#'
#'
#'
#'
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


