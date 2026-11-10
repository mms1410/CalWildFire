library(fs)
library(here)
library(fmesher)
library(sf)
library(ggplot2)
library(patchwork)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils", "utils_io.R"))
source(path(here(), "R", "utils", "utils_geocomp.R"))
source(path(here(), "R", "utils", "utils_mesh.R"))
source(path(here(), "R", "plots", "defaults.R"))
source(path(here(), "R", "plots", "functions_mesh.R"))
#-------------------------------------------------------------------------------
spat_meshes <- dir_ls(path(here(), "assets", "mesh"), regexp = "spat_mesh")
int_meshes <- dir_ls(path(here(), "assets", "mesh"), regexp = "int_mesh_\\d.rds")

destination_dir <- dir_create(here(), "assets", "plots", "mesh")
ca_mld <- readFile("ca_mld.gpkg")
crs_km <- calAlbersKm()
mask <- getMainlandMask() |>
  st_transform(crs_km)
#-------------------------------------------------------------------------------
plt_list_spat <- list()
plt_list_mask <- list()
plt_list_edge <- list()
plt_list_std <- list()

for (mesh_file in spat_meshes) {
  
  mesh_name <- basename(path_ext_remove(mesh_file))
  mesh <- readFile(mesh_file)
  mesh_config <- getMeshConfig(mesh_name)
  mesh_info <- getMeshInfo(mesh)
  config_info <-  paste(names(mesh_config), mesh_config, collapse = ", ", sep = ":")
  
  plt_mesh_spat <- ggplot() +
    geom_sf(data = st_transform(ca_mld, crs_km), alpha = 0.4, color = "black", linewidth = 0.8) +
    geom_fm(data = mesh, alpha = 0.7) +
    labs(title = mesh_name,
         subtitle = paste0(config_info, "\n", mesh_info))
  plt_mesh_spat

  range <- mesh_config$max.edge[[1]]
  asses <- fm_assess(mesh, 150, 2) 
  sd.dev.limits <- 1 + c(-1, 1) * max(abs(range(asses$sd.dev, na.rm = TRUE) - 1))
  col.values <- 2 * seq(0, 1, length.out = 100) - 1
  col.values <- (sign(col.values) * abs(col.values)^1.5 + 1) / 2
  
  # plots syntax from
  # https://inlabru-org.github.io/fmesher/articles/variable_mesh_quality.html
  
  plt_edge <- ggplot() +
    geom_tile(data = asses,
              aes(geometry = geometry, fill = edge.len), stat = "sf_coordinates") +
    coord_sf(default = TRUE) +
    scale_fill_distiller(type = "seq",
                         direction = 1,
                         palette = 1,
                         na.value = "transparent") +
    geom_fm(data = mesh, fill = NA) +
    labs(title = mesh_name,
         subtitle = paste0(config_info, "\n", mesh_info))
  plt_edge
  
  plt_std <- ggplot() +
    geom_tile(data = asses,
              aes(geometry = geometry, fill = sd.dev),
              stat = "sf_coordinates") +
    coord_sf(default = TRUE) +
    scale_fill_distiller(type = "div",
                         palette = "viridis",
                         na.value = "transparent",
                         limits = sd.dev.limits,
                         values = col.values)
  plt_std
    
  ggsave(filename = paste0(mesh_name, ".png"),
         path = destination_dir,
         plot = plt_mesh_spat)
  
  plt_list_spat <- append(plt_list_spat, plt_mesh_spat)
  
  #-----------------------------------------------------------------------------
  
  nodes <- getMeshNodes(mesh)
  nodes_masked <- maskMesh(mesh, mask)
  
  plt_mask <- ggplot() +
    geom_sf(data = st_crop(mask, st_bbox(nodes)), fill = "brown", alpha = 0.5) +
    geom_sf(data = ca_mld, alpha = 0.4) +
    geom_sf(data = nodes, alpha = 0.9, color = "forestgreen", size = 0.8) +
    geom_sf(data = nodes_masked$non_masked, color = "blue", size = 0.8) +
    labs(title = mesh_name)
  plt_mask
  
  ggsave(filename = paste0(mesh_name, "_mask.png"),
         path = destination_dir,
         plot = plt_mask)
  
  plt_list_mask <- append(plt_list_mask, plt_mask)
}

plt_spat_meshes <- patchwork::wrap_plots(plt_list_spat, ncol = 3)
ggsave(filename = "patch_spat_meshes.png",
       plot = plt_spat_meshes,
       path = destination_dir)

plt_mask_meshes <- patchwork::wrap_plots(plt_list_mask, ncol = 3)
ggsave(filename = "patch_mask_meshes.png",
       plot = plt_mask_meshes,
       path = destination_dir)
#-------------------------------------------------------------------------------
# integration meshes:
ca_mld <- readFile("cal_mld.gpkg") |>
  st_transform(crs_km)

for (mesh_file in int_meshes) {
  
  mesh_name <- basename(path_ext_remove(mesh_file))
  mesh <- readRDS(mesh_file)
  
  voro_file <- paste0("voronoi_", mesh_name, ".gpkg")
  voro <- readFile(voro_file)
  pts <- readFile(paste0("pts_", mesh_name, ".gpkg"))
  
  sw <- sum(voro$spat_weight)
  tex_sum <- bquote(sum(w[i]) == .(sw))
  tex_leg <- bquote(w[i])
  
  plt_voro <- ggplot() +
    geom_sf(data = voro, aes(fill = spat_weight)) +
    geom_sf(data = pts, aes(color = "mesh vertex"), size = 0.4, alpha = 0.5) +
    labs(title = paste0("Voronoi Tesselation  ", mesh_name),
         subtitle = tex_sum,
         fill = tex_leg) +
    scale_color_manual(name = NULL,
                       values = c("mesh vertex" = "red"))
  plt_voro
  ggsave(filename = paste0("voronoi_", mesh_name, ".png"),
         path = destination_dir,
         plot = plt_voro)
  
  #-----------------------------------------------------------------------------
  mld_buffer = st_buffer(ca_mld, -2)
  
  plt_int_mesh <- ggplot() +
    geom_fm(data = mesh, linewidth = 0.5, color = "black",
            defs = list(bnd = list(alpha = 0.5, linewidth = 0.3, color = "white"),
                        int = list(alpha = 0.5, color = "blue"),
                        loc = list(alpha = 0.5, color = "red", size = 0.3))) +
    geom_sf(data = mld_buffer, color = "green", fill = NA, size = 0.7)
  plt_int_mesh
  
  ggsave(filename = paste0(mesh_name, ".png"),
         path = destination_dir,
         plot = plt_int_mesh)
  
  plt_voro_int <- wrap_plots(list(plt_int_mesh, plt_voro), ncol = 2)
  ggsave(filename = paste0(mesh_name, "_voro.png"),
         path = destination_dir,
         plot = plt_voro_int)
}
