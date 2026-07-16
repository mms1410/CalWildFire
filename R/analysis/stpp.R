library(stpp)
library(here)
library(fs)
library(sf)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils", "data_queries.R"))
conf <- get_conf()
crs <- st_crs(read_conf(conf, "crs"))
fires_stpp <- read_geodata(sf_crs = crs, filename = "calfire") |>
  to_stpp(start_date = as.Date(paste0(conf$start_year, "-01-01")))
dir_cache <- path(here(), ".cache")
obswindow <- read_geodata(keyword = "ca_state",crs) |>
  st_coordinates() |>
  (\(x) x[, 1:2])()

#-------------------------------------------------------------------------------
readCache <- function(source = dir_cache, envir = parent.frame()) {
  files <- dir_ls(dir_cache, glob = "*.RDS")
  load_helper <- function(source) {
    obj_name <- sub("\\..*$", "", basename(source))
    assign(x = obj_name, value = readRDS(source), envir = envir)
  }
  invisible(lapply(files, load_helper))
}

cacheResults <- function(to_cache, destination = dir_cache) {
  dir_create(destination)
  lapply(names(to_cache), function(x) saveRDS(to_cache[[x]], path(destination, paste0(x, ".RDS"))))
}
#-------------------------------------------------------------------------------
readCache()
plotK(stik)



#-------------------------------------------------------------------------------
# astik <- ASTIKhat(fires_stpp, s.region = obswindow)
# klista <- KLISTAhat(fires_stpp, s.region = obswindow, times = 50)
# pcf_hat <- PCFhat(fires_stpp, s.region = obswindow, times = 50)
# stik <- STIKhat(fires_stpp, s.region = obswindow, times = 50)
# cacheResults(list(astik = astik, klista = klista, pcf_hat = pcf_hat, stik = stik))



