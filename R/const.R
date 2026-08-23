library(sf)
library(fs)
library(dplyr)
library(USAboundaries)
#-------------------------------------------------------------------------------
source("R/utils.R")
CONF <- readYaml()
CRS <- st_crs(CONF[["crs"]])
CA <- USAboundaries::us_states(states = "California", resolution = "high") |>
  dplyr::pull(geometry) |>
  st_transform(CRS) |>
  st_as_sf()