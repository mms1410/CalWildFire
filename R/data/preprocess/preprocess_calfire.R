library(sf)
library(fs)
library(here)
library(tidyverse)
#-------------------------------------------------------------------------------
source("R/utils/data_queries.R")
conf <- get_conf()
crs <- st_crs(read_conf(conf, "crs"))
to_date <- function(x) as.POSIXct(x/1000, origin = "1970-01-01", tz = "UTC")
dir_destination <- path(here(), "data", "preprocessed")
#-------------------------------------------------------------------------------
calfire <- st_read(path(here(), "data", "raw", "calfire.gpkg"))

fires <- calfire |> 
  mutate(date = to_date(ALARM_DATE)) |>
  filter(!is.na(date),
         year(date) >= read_conf(conf, "start_year"),
         year(date) <= read_conf(conf, "end_year"),
         OBJECTIVE == "Suppression (Wildfire)") |>
  st_transform(crs) |>
  mutate(geom = st_make_valid(geom)) |>
  select(id = OBJECTID, date, cause = CAUSE, area = Shape__Area, length = Shape__Length)


# units: acres
area <- fires |>
  select(id, date)

fires <- st_centroid(fires)

dir_create(dir_destination)
st_write(fires, path(dir_destination, "calfire.gpkg"), append = FALSE)
st_write(area, path(dir_destination, "burntarea.gpkg"), append = FALSE)