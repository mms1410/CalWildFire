library(checkmate)
library(INLA)
library(inlabru)
#-------------------------------------------------------------------------------

#' Wrap 'Intercept' component into component list
#'
#' @param name This name will be added to the component to be able to differentiate between, say, 'Intercept_tmax' vs. 'Intercep_vpdmax'
#' @param precn Small prior precision to be added. Defaults to 1e-3
#'
initCmpIntercept <- function(name = "", prec = 1e-3) {
  
  checkmate::assertString(name)
  
  suffix <- if (nzchar(name)) paste0("_", name) else ""
  
  form <- as.formula(sprintf("~ -1 + Intercept%s(1, prec.linear = %f)", suffix, prec))
  return(bru_comp_list(form))
}

#  Elevation RW2 Component

#' Wrap 'effect_elevation' into component list
#'
#' @param name This name will be added to the component to be able to differentiate between, say, 'effect_elevation_tmax' vs. 'effect_elevation_vpdmax'
#'
initCmpElevation <- function(name = "") {
  
  checkmate::assert(exists("n_region", envir = parent.frame()))
  
  suffix <- if (nzchar(name)) paste0("_", name) else ""
  
  name <- paste0("_", name)
  form <- as.formula(sprintf("~ -1 + effect_elevation%s(elevation, replicate = region, n = n_region, model = 'rw2', scale.model = TRUE)", suffix))
  return(bru_comp_list(form))
}

#' Wrap 'effect_coast_distance' into component list
#'
#' @param name This name will be added to the component to be able to differentiate between, say, 'effect_coast_distance_tmax' vs. 'effect_coast_distance_vpdmax'
#'
initCmpCoastDistance <- function(name = "") {
  
  checkmate::assert(exists("n_region", envir = parent.frame()))
  
  suffix <- if (nzchar(name)) paste0("_", name) else ""
  
  form <- as.formula(sprintf("~ -1 + effect_coast_distance%s(distance_coast, replicate = region, n = n_region, model = 'rw2', scale.model = TRUE)", suffix))
  return(bru_comp_list(form))
}

#' Wrap 'effect_region_iid' into component list
#'
#' @param name This name will be added to the component to be able to differentiate between, say, 'effect_coast_distance_tmax' vs. 'effect_coast_distance_vpdmax'
#'
initCmpRegionIID <- function(name = "") {
  
  suffix <- if (nzchar(name)) paste0("_", name) else ""
  
  form <- as.formula(sprintf("~ -1 + effect_region%s(region, model = 'iid')", suffix))
  return(bru_comp_list(form))
}

initCmpRegionBYM <- function(name = "") {
  
  checkmate::assert(exists("graph_ecoz", envir = parent.frame()))
  
  suffix <- if (nzchar(name)) paste0("_", name) else ""
  
  form <- as.formula(sprintf("~ -1 + effect_region%s(region, model = 'bym2', graph = graph_ecoz, constr = TRUE, scale.model = TRUE)", suffix))
  
  return(bru_comp_list(form))
}

initCmpWeek <- function(name = "") {
  
  checkmate::assertString(name)
  
  suffix <- if (nzchar(name)) paste0("_", name) else ""
  
  form <- as.formula(sprintf("~ -1 + effect_week%s(week, model = 'rw2', cyclic = TRUE, scale.model = TRUE)", suffix))
  
  return(bru_comp_list(form))
}
#-------------------------------------------------------------------------------


initKroneckerField <- function(name) {
  
  checkmate::assertString(name)
  checkmate::assert(exists("spde_kron", envir = parent.frame()))
  checkmate::assert(exists("mesh_temporal", envir = parent.frame()))
  
  
  suffix <- if (nzchar(name)) paste0("_", name) else ""
  
  form <- as.formula(sprintf("~ -1 + field_kron%s(main = geometry, model = spde_kron, group = time, group_mapper = bru_mapper(mesh_temporal))", suffix))  
  
  return(bru_comp_list(form))
}



#'
#'
#'
#'
#'
initSeperableSPDE <- function(mesh_spatial, mesh_temporal, priors_list) {
  spde_102 <- stModel.define(smesh = mesh_spatial,
                             tmesh = mesh_temporal,
                             model = "102", 
                             control.priors = priors_list)
  return(spde_102)
}


initCriticalDiffusionSPDE <- function(mesh_spatial, mesh_temporal, priors_list) {
  
  spde_121 <- stModel.define(smesh = mesh_spatial,
                             tmesh = mesh_temporal,
                             model = "121", 
                             control.priors = priors_list)
  return(spde_121)
}

#'
#'
#'
#'
#'
initSperable2SPDE <- function(mesh_spatial, mesh_temporal, priors_list) {
  
  spde_202 <- stModel.define(smesh = mesh_spatial,
                             tmesh = mesh_temporal,
                             model = "202", 
                             control.priors = priors_list)
  return(spde_202)
}

#'
#'
#'
#'
#'
initInteratedDiffusionSPDE <- function(mesh_spatial, mesh_temporal, priors_list) {
  
  spde_220 <- stModel.define(smesh = mesh_spatial,
                             tmesh = mesh_temporal,
                             model = "202",
                             control.priors = priors_list)
  return(spde_220)
}