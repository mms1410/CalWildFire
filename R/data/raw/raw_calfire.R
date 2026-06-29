library(esri2sf) # remotes::install_github("yonghah/esri2sf")
library(sf)
library(here)
library(fs)
library(yaml)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils", "data_queries.R"))
conf <- get_conf()
#-------------------------------------------------------------------------------
dir_destination <- path(here(), "data", "raw")
dir_create(dir_destination)

# TODO: validity checks
where <- paste0("YEAR_ >= ", read_conf(conf, "start_year"), " AND ", "YEAR_ <=", read_conf(conf, "end_year"))
fire_layer <- esri2sf(
  read_conf(conf, "url_fires")
  )
calfire_destination<- path(dir_destination, "calfire.gpkg")
st_write(fire_layer, calfire_destination, append = FALSE)
