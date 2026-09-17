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

CA_MLD <- USAboundaries::us_states(states = "California", resolution = "high")|>
  st_cast("POLYGON", warn = FALSE) |>
  mutate(area = st_area(geometry)) |>
  slice_max(area, n=1) |>
  st_transform(CRS) |>
  select(geometry)
