library(fs)
library(inlabru)
library(INLA)
library(INLAspacetime)
#------------------------------------------------------------------------------
source(path("R", "utils.R"))
source(path("R", "analysis", "utils_analysis.R"))
#-------------------------------------------------------------------------------
set.seed(123)
config_data <- readYaml("data")
config <- readYaml("fields_vs")
destination_dir <- path("assets", "fields", "vs")
save_rds <- saveRDSFactory(destination_dir)
dumpYaml(config, path(destination_dir, paste0("config_", config$name, ".yaml")))
#-------------------------------------------------------------------------------
bru_options_set(verbose = TRUE,
                bru_verbose = TRUE,
                bru_compress_cp = TRUE,
                num.threads = "1:1",
                control.inla = list(strategy = config$args_bru_options$strategy,
                                    int.strategy = config$args_bru_options$int.strategy),
                control.compute = list(dic = TRUE,
                                       waic = TRUE))
#-------------------------------------------------------------------------------
vs <- readFile(config$table)
vs <- vs[year(date) %in% getYearsList()$train,]
#-------------------------------------------------------------------------------
scaler_file <- paste0("scaler_", getFilename(config$table), ".rds")
if (file.exists(path(destination_dir, scaler_file))) {
  scaler <- readFile(scaler_file)
} else {
  scaler <- setScaler(vs)
  message(sprintf("Save %s", scaler_file))
  saveRDS(scaler, path(destination_dir, scaler_file))
}
mesh_spatial <- readFile(config$mesh_spatial)
mesh_temporal <- makeTemporalMesh(vs$date, 100, deg = 2) # for INLAspacetime deg1 neccessary
#-------------------------------------------------------------------------------
vs <- vs[sample(.N, config$n_sample)]; gc()
vs <- addDEM(vs)
vs <- addCoastDistance(vs)
vs[, `:=`(month = month(date),
            year = year(date),
            value = scaler$transform(value, mu = attr(scaler, "mu_train"), sigma = attr(scaler, "sigma_train")))]
vs$time <- as.numeric(vs$date)
vs <- vs[!is.na(elevation) & !is.na(eastness), ] # boundary !!!!!!
#-------------------------------------------------------------------------------
args_spde <- config$args_spde 
args_field_year <- config$args_component$field_year
args_field_month <- config$args_component$field_month

args_field_seperable <- config$args_component$field_separable
rho_prior <- args_field_seperable$control.group$hyper$rho$prior 
rho_param <- args_field_seperable$control.group$hyper$rho$param
#-------------------------------------------------------------------------------
spde <- inla.spde2.pcmatern(mesh = mesh_spatial,
                            alpha = args_spde$alpha,
                            prior.range = unlist(args_spde$prior.range),
                            prior.sigma = unlist(args_spde$prior.sigma),
                            constr = TRUE)


field_sep <- bru_comp_list(~ 0 + field_sep(main = cbind(lon, lat),
                                           model = spde,
                                           group = time,
                                           group_mapper = bru_mapper(mesh_temporal),
                                           control.group = list(
                                             model = "ar1",
                                             hyper = list(
                                               rho = list(prior = rho_prior, param = rho_param)))))
#===============================================================================
# MODEL 1: vs = field
#===============================================================================
mod1 <- bru(components = field_sep,
            bru_obs(formula = value ~ field_sep,
                    family = "gaussian",
                    data = vs))
save_rds(mod1, paste0("model1", config$name))
rm(mod1);gc()
#===============================================================================
# MODEL 2: vs = elevation + coastdistance + field_sep
#===============================================================================
mod2 <- bru(components = field_sep,
            bru_obs(formula = value ~ elevation + distance_coast + field_sep,
                    family = "gaussian",
                    data = vs))

save_rds(mod2, paste0("model2", config$name))
rm(mod2);gc()