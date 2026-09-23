library(esri2sf) # remotes::install_github("yonghah/esri2sf")
library(sf)
library(dplyr)
library(lubridate)
library(fs)
#-------------------------------------------------------------------------------
source(path(getwd(), "R", "utils.R"))

config <- readYaml("data")
ca_crs <- st_crs(config$crs)
start_year = config$start_year
end_year = config$end_year
calfire_url <- config$calfire$url
destination_dir <- dir_create(path(getwd(), "raw", "data"))
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
  st_transform(ca_crs) |>
  select(calfire_id = OBJECTID, date, cause = CAUSE, area = Shape__Area, length = Shape__Length, agency = AGENCY)

# units: acres
area <- fires |>
  select(calfire_id, date)
fires <- st_centroid(fires)

st_write(fires, path(destination_dir, "calfire.gpkg"), append = FALSE)
st_write(area, path(destination_dir, "burntarea.gpkg"), append = FALSE)