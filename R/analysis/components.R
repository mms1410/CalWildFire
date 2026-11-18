library(checkmate)
library(INLA)
library(inlabru)
library(INLAspacetime)
#-------------------------------------------------------------------------------

#' Wrap 'Intercept' component into component list
#'
#' @param name This name will be added to the component to be able to differentiate between, say, 'Intercept_tmax' vs. 'Intercep_vpdmax'
#' @param precn Small prior precision to be added. Defaults to 1e-3
#'
initCmpIntercept <- function(variable_name = "", prec = 1e-3) {
  
  checkmate::assertString(variable_name)
  
  suffix <- if (nzchar(variable_name)) paste0("_", variable_name) else ""
  
  form <- as.formula(sprintf("~ -1 + Intercept%s(1, prec.linear = %f)", suffix, prec))
  return(bru_comp_list(form))
}

#  Elevation RW2 Component

#' Wrap 'effect_elevation' into component list
#'
#' @param variable_name This name will be added to the component to be able to differentiate between, say, 'effect_elevation_tmax' vs. 'effect_elevation_vpdmax'
#'
initCmpElevation <- function(variable_name = "", n_regions) {
  
  suffix <- if (nzchar(variable_name)) paste0("_", variable_name) else ""
  
  form <- as.formula(sprintf("~ -1 + effect_elevation%s(elevation, replicate = region, n = %d, model = 'rw2', scale.model = TRUE, constr = TRUE)", suffix, n_regions))
  return(bru_comp_list(form))
}

#' Wrap 'effect_coast_distance' into component list
#'
#' @param variable_name This name will be added to the component to be able to differentiate between, say, 'effect_coast_distance_tmax' vs. 'effect_coast_distance_vpdmax'
#'
initCmpCoastDistance <- function(variable_name = "", n_regions) {
  
  suffix <- if (nzchar(variable_name)) paste0("_", variable_name) else ""
  
  form <- as.formula(sprintf("~ -1 + effect_coast_distance%s(distance_coast, replicate = region, n = %d, model = 'rw2', scale.model = TRUE, constr = TRUE)", suffix, n_regions))
  return(bru_comp_list(form))
}

#'
#'
#'
#'
initCmpBesag <- function(variable_name = "", adj_graph) {
  
  adj_name <- deparse(substitute(adj_graph))
  
  suffix <- if (nzchar(variable_name)) paste0("_", variable_name) else ""
  
  form <- as.formula(sprintf("~ -1 + effect_region%s(region, model = 'besag', graph = %s,constr = TRUE, scale.model = TRUE)", suffix, adj_name))
  
  return(bru_comp_list(form))
}
#-------------------------------------------------------------------------------
initKroneckerSPDE <- function(mesh_spatial, prior_sigma, prior_range) {
  
  checkmate::assertNumeric(prior_range)
  checkmate::assertNumeric(prior_sigma)
  checkmate::assertClass(mesh_spatial, "fm_mesh_2d")
  
  return(inla.spde2.pcmatern(mesh_spatial,
                             prior.sigma = prior_sigma,
                             prior.range = prior_range))
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
#-------------------------------------------------------------------------------
initCmpKroneckerField <- function(variable_name, mesh_temporal, spde) {
  
  checkmate::assertString(variable_name)
  checkmate::assertClass(mesh_temporal, "fm_mesh_1d")
  checkmate::assertClass(spde, "inla.spde2")
  
  suffix <- if (nzchar(variable_name)) paste0("_", variable_name) else ""
  var_name_mesh <- deparse(substitute(mesh_temporal))
  var_name_spde <- deparse(substitute(spde))
  
  form <- as.formula(sprintf("~ -1 + field_kron%s(main = geometry,
                                model = %s,
                                group = time,
                                group_mapper = bru_mapper(%s))", suffix, var_name_spde, var_name_mesh))
  return(bru_comp_list(form))
}

#'
initCmpDiffusionField <- function(variable_name = "", model, spde){
  
  
  checkmate::assertString(variable_name)
  checkmate::assertChoice(model, c("102", "121","202", "220"))
  checkmate::assertClass(spde, "stModel_cgeneric")
  
  spde_spec <- deparse(substitute(spde))
  suffix <- if (nzchar(variable_name)) paste0("_", variable_name) else ""
  field_spec <- paste0(model,suffix)
  
  
  
  form <- as.formula(sprintf("~ -1 + field_%s(main = list(space = geometry, time = time),
                                                         model = %s)", field_spec, spde_spec))  
  
  return(bru_comp_list(form))
}
