library(esri2sf) # remotes::install_github("yonghah/esri2sf")
library(sf)
library(dplyr)
library(lubridate)
library(fs)
library(here)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils.R"))
source(path(here(), "R", "data", "utils_data.R"))

config <- readYaml("data")
ca_crs <- st_crs(config$crs)
mask_mainland <- readFile(path(here(), "data", "auxiliary", "cal_mld.gpkg"))
start_year = config$start_year
end_year = config$end_year
calfire_url <- config$calfire$url
destination_dir <- dir_create(path(here(), "data", "preprocessed"))
#-------------------------------------------------------------------------------
calfire <- esri2sf(calfire_url)


fires <- calfire |> 
  st_set_geometry("geometry") |> #
  mutate(date = as.Date(lubridate::as_datetime(ALARM_DATE / 1000))) |>  #  alarm-date is (base 1970-01-01) timestamp in ms
  filter(!is.na(date),
         year(date) >= start_year,
         year(date) <= end_year,
         OBJECTIVE == "Suppression (Wildfire)",
         CAUSE != "Firefighter Training" | is.na(CAUSE)) |>
  st_make_valid() |>
  st_transform(ca_crs) |>
  select(calfire_id = OBJECTID, date, cause = CAUSE, area = Shape__Area, length = Shape__Length, agency = AGENCY) |>
  cropMainland(mask_mainland)

# units: acres
area <- fires |>
  select(calfire_id, date)
fires <- st_centroid(fires)

st_write(fires, path(destination_dir, "calfire.gpkg"), append = FALSE)
st_write(area, path(destination_dir, "burntarea.gpkg"), append = FALSE)