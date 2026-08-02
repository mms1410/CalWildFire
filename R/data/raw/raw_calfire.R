library(esri2sf) # remotes::install_github("yonghah/esri2sf")
library(sf)
library(here)
library(fs)
library(yaml)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils", "data_queries.R"))
config <- read_yaml()
url_calfire <- config[["calfire"]][["url"]]
dir_destination <- path(here(), "data", "raw")
dir_create(dir_destination)
#-------------------------------------------------------------------------------
calfire <- esri2sf(url_calfire)
calfire_destination<- path(dir_destination, "calfire.gpkg")
st_write(calfire, calfire_destination, append = FALSE)
