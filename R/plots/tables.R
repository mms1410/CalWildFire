library(gt)
library(janitor)
#-------------------------------------------------------------------------------
source("R/utils/data_queries.R")
source("R/plots/table_functions.R")
source("R/utils/geo_comp.R")
#-------------------------------------------------------------------------------
conf <- get_conf()
crs <- st_crs(read_conf(conf, "crs"))


#-------------------------------------------------------------------------------
fires <- read_geodata(sf_crs = crs, filename = "calfire")
fires <- fires |>
  mutate(area = area / 247.1) # acres in km2

ecoz <- read_geodata(keyword = "ecoz3", sf_crs <- crs) |>
  mutate(zone = rename_source(US_L3NAME, ecoz_names_map)) |>
  select(zone)
#-------------------------------------------------------------------------------
st_join(fires, ecoz, join = st_intersects, left = TRUE) |>
  st_drop_geometry() |>
  mutate(year = lubridate::year(date),
         month = lubridate::month(date, label = TRUE, abbr = TRUE)) |>
  select(year, month, zone) |>
  filter(!is.na(zone), year %in% 2004:2030) |>
  count(year, month, zone) |>
  pivot_wider(names_from = zone,
              values_from = n,
              values_fill = 0) |>
  arrange(year, month) |>
  rowwise() |>
  mutate(Total = sum(c_across(where(is.numeric) & !matches("year")))) |>
  ungroup() |>
  adorn_totals("row", name = "Total") |>
  # --- Table Formatting with gt ---
  gt(groupname_col = "year", rowname_col = "month") |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_body(columns = Total),
      cells_body(rows = month == "Total")))


fires |>
  mutate(year = lubridate::year(date),
         month = lubridate::month(date, abbr = TRUE, label = TRUE)) |>
  select(year, month) |>
  st_drop_geometry() |>
  count(year, month) |>
  pivot_wider(names_from = month, values_from = n, values_fill = 0) |>
  arrange(year, month)
  