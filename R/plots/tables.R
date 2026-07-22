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
      cells_body(rows = month == "Total"))) |>
  as_latex() |> cat()
# Better use:
# \begin{landscape}
# \begin{longtable}{l|rrrrrrrrrrrrrr}
# \caption{Counts per Ecoregion} \label{tab:ecoregion} \\
# \toprule
#  & \rot{Southern CA/Baja Coast} & \rot{Klamath/North Coast} & \rot{Central CA Foothills \& Coast} & \rot{Central Valley} & \rot{Sierra Nevada} & \rot{Cascades} & \rot{Central Basin \& Range} & \rot{Eastern Cascades Slopes} & \rot{Sonoran Basin} & \rot{Southern CA Mountains} & \rot{Northern Basin \& Range} & \rot{Mojave Basin} & \rot{Coast Range} & \rot{Total} \\
# \midrule
# \endfirsthead
# \multicolumn{15}{l}{\small\itshape (continued from previous page)} \\
# \toprule
#  & \rot{Southern CA/Baja Coast} & \rot{Klamath/North Coast} & \rot{Central CA Foothills \& Coast} & \rot{Central Valley} & \rot{Sierra Nevada} & \rot{Cascades} & \rot{Central Basin \& Range} & \rot{Eastern Cascades Slopes} & \rot{Sonoran Basin} & \rot{Southern CA Mountains} & \rot{Northern Basin \& Range} & \rot{Mojave Basin} & \rot{Coast Range} & \rot{Total} \\
# \midrule
# \endhead
# \midrule
# \multicolumn{15}{r}{\small\itshape continued on next page} \\
# \endfoot
# \bottomrule
# \endlastfoot
# \addlinespace[2.5pt]
# \multicolumn{15}{l}{2004} \\[2.5pt]
# \midrule\addlinespace[2.5pt]
# .....



fires |>
  mutate(year = lubridate::year(date),
         month = lubridate::month(date, abbr = TRUE, label = TRUE)) |>
  select(year, month) |>
  st_drop_geometry() |>
  count(year, month) |>
  pivot_wider(names_from = month, values_from = n, values_fill = 0) |>
  adorn_totals(where = c("row", "col")) |>
  gt() |>
  as_latex() |> cat()
  