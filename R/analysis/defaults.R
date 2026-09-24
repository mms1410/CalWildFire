set.seed(123)
#cat(paste0("Available latent models in R-INLA:\n, ", paste0(names(inla.models()$latent), collapse = "\t")))
#cat(paste0("Available likelihoods in R-INLA:\n, ", paste0(names(inla.models()$likelihood), collapse = "\t")))

bru_options <- list(control.inla = list(strategy = "simplified.laplace", int.strategy = "eb"),
                    control.compute = list(waic = TRUE, cpo = TRUE, po = TRUE, mlik = TRUE),
                    num.threads = "4:1",
                    verbose = TRUE)

cat_info <- function(spec) {
  cat(paste0("🛠  Fitting '", spec, "'...\n"))
  Sys.sleep(2)
}

saveFactory <- function(destination_dir, addon_folder = "") {
  if (addon_folder != "") {
    destination_dir <- path(destination_dir, addon_folder)
  }
  return(function(object, name_prefix){
    saveRDS(object, path(destination_dir, paste0(name_prefix, ".rds")))
  })
}

#-------------------------------------------------------------------------------
get_spde2d <- function(){
  spde2d <- inla.spde2.pcmatern(mesh = mesh_2d,
                      alpha = 2,
                      prior.range = c(5000, 0.5),
                      prior.sigma = c(50, 0.5),
                      constr = TRUE)
  return(spde2d)
}


get_field_spat <- function() {
  field_spat <- bru_comp("field_spat",
                       main = geometry,
                       mapper = mapper_space,
                       model = spde2d)
  return(field_spat)
}

get_field_ar1 <- function() {
  field_ar1 <- bru_comp("field_ar1",
                        main = time,
                        mapper = mapper_time,
                        model = "ar1")
  return(field_ar1)
}

get_field_spatiotemp1 <- function() {
  field_spatiotemp1 <- bru_comp("field_spatiotemp1",
                                main = geometry,
                                mapper = mapper_space,
                                group = time,
                                group_mapper = mapper_time,
                                model = spde2d,
                                control.group = list(model = "ar1"))
  return(field_spatiotemp1)
} 
#-------------------------------------------------------------------------------
do_fit1 <- function(family, data) {
  fit1 <- bru(components = c(field_spat, field_ar1),
              formula = value ~.,
              data = data,
              family = family)
  cat_info(paste0("field1 (", family, ")"))
  save_rds(fit1, paste0("fit1_", family))
}

do_fit2 <- function(family, data) {
  fit2 <- bru(components = field_spatiotemp,
              formula = value ~.,
              data = data,
              family = family)
  cat_info(paste0("field2 (", family, ")"))
  save_rds(fit2, paste0("fit2_", family))
}
