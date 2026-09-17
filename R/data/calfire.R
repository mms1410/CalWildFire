library(esri2sf) # remotes::install_github("yonghah/esri2sf")
library(sf)
library(dplyr)
library(lubridate)
library(here)
library(fs)
#-------------------------------------------------------------------------------
source(path(here(), "R", "const.R"))
source(path(here(), "R", "utils.R"))
calfire_url <- CONF$calfire$url
destination_dir <- path(here(), "data")
dir_create(destination_dir)
#-------------------------------------------------------------------------------
calfire <- esri2sf(calfire_url)

# original date is unix (base 1970-01-01) timestamp in milisecond
fires <- calfire |> 
  st_set_geometry("geometry") |> #
  mutate(date = as.Date(lubridate::as_datetime(ALARM_DATE / 1000))) |> 
  filter(!is.na(date),
         year(date) >= CONF[["start_year"]],
         year(date) <= CONF[["end_year"]],
         OBJECTIVE == "Suppression (Wildfire)",
         CAUSE != "Firefighter Training" | is.na(CAUSE)) |>
  st_make_valid() |>
  st_transform(CRS) |>
  select(calfire_id = OBJECTID, date, cause = CAUSE, area = Shape__Area, length = Shape__Length, agency = AGENCY)

# units: acres
area <- fires |>
  select(calfire_id, date)
fires <- st_centroid(fires)

st_write(fires, path(destination_dir, "calfire.gpkg"), append = FALSE)
st_write(area, path(destination_dir, "burntarea.gpkg"), append = FALSE)