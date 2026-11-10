library(checkmate)
library(fmesher)
library(sf)
library(ggplot2)
library(here)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils", "utils_mesh.R"))
#-------------------------------------------------------------------------------
plotSpatMeshMask <- function(spat_mesh, mask, ca_mld) {
  
  checkmate::assertClass(mask, "sfc_POLYGON")
  checkmate::assertClass(spat_mesh, "fm_mesh_2d")
  checkmate::assert(fm_crs(spat_mesh) == st_crs(mask))
  checkmate::assert(fm_crs(spat_mesh) == st_crs(ca_mld))
  
  nodes_mask <- maskMesh(spat_mesh, mask)
  
  plt <- ggplot() +
    geom_fm(data = spat_mesh, alpha = 0.5, defs = list(bnd = list(linewidth = 0.5, color = "gray20"))) +
    geom_sf(data = ca_mld, alpha = 0.1, color = "black") +
    geom_sf(data = nodes_mask$masked, color = "red") +
    geom_sf(data = nodes_mask$non_masked, color = "forestgreen")
  
  return(plt)
}





