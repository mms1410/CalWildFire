library(fs)
library(purrr)
library(here)
library(sf)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(patchwork)
library(tidyterra)
library(scales)
library(units)
library(dplyr)
#-------------------------------------------------------------------------------
source("R/utils.R")
source("R/const.R")
source("R/plots/utils.R")
plt_args_fire <- list(color = "black", shape = 19, size = 0.1, alpha = 0.6)
plt_args_barea <- list(color = "grey", alpha = 0.8)
plt_args_ca <- list(color = "grey", alpha = 0.3)
plt_args_ecoz <- list(alpha = 0.4)
plt_dir <-path(here(), "assets", "plots", "fire")
dir_create(plt_dir)
#-------------------------------------------------------------------------------
fires <- read_file("calfire.gpkg", CRS)
barea <- read_file("burntarea.gpkg", CRS)
ca_ecoz  <- read_file("ecoz.gpkg", CRS)

gg_fire <- list(do.call(geom_sf,
                        c(list(data = transmute(fires, year = lubridate::year(date))),
                          plt_args_fire)))
gg_barea <- list(do.call(geom_sf,
                         c(list(data = barea), plt_args_barea)))
gg_ecoz <- list(do.call(geom_sf,
                        c(list(data = ca_ecoz,
                               mapping = aes(fill = region)),
                          plt_args_fire)))
gg_ca <- list(do.call(geom_sf,
                      c(list(data = CA),
                      plt_args_ca)))
#-------------------------------------------------------------------------------
ggplot() +
  gg_ca +
  gg_fire +
  facet_wrap(~year) +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), aspect.ratio = 1)
gg_save("point_fires_facet_year", destination_dir = plt_dir, width = 7, height = 10)

plt_point_fires_barea_total <- ggplot() +
  gg_ca +
  gg_barea +
  gg_fire +
  labs(x = "Longitude", y = "Latitude")
gg_save("point_fires_barea_total", plt = plt_point_fires_barea_total, destination_dir = plt_dir)

for (yr in year(fires$date) |> unique()) {
  ggplot() +
    gg_ca +
    do.call(geom_sf,
            c(list(data = filter(barea, lubridate::year(date) == yr)), plt_args_barea)) + 
    do.call(geom_sf,
            c(list(data = fires |>
                     transmute(year = lubridate::year(date)) |>
                     filter(year == yr)),
              plt_args_fire)) +
    labs(x = "Longitude", y = "Latitude") + 
    ggtitle(yr)
  gg_save(paste0("point_fires_", yr), destination_dir = plt_dir)
}
gifski(dir_ls(plt_dir, regexp = "point_fires_\\d{4}"),
       path(plt_dir, "point_fires.gif"),
       delay = 0.5,
       width = 800,
       height = 600)


ggplot() +
  gg_ca +
  geom_sf(data = fires, aes(color = log1p(area)), size = 0.1, alpha = 0.7) +
  scale_color_viridis_c(option = "inferno", trans = "log", direction = -1, name = "log(1+area)") +
  xlab("Longitude") +
  ylab("Latitude")
gg_save("point_fires_total_mark", destination_dir = plt_dir)

plt_point_fires_total_ecoz <- ggplot() +
  gg_ecoz + 
  gg_fire +
  theme(legend.position = "bottom", legend.text = element_text(size = 6)) +
  labs(fill = "") +
  xlab("Longitude") +
  ylab("Latitude") +
  guides(fill = guide_legend(nrow = 5, ncol = 3, byrow = TRUE))
gg_save("point_fires_total_ecoz", plt = plt_point_fires_total_ecoz, destination_dir = plt_dir)

plt_ts_fire_counts_m <- fires |>
  select(date) |>
  st_drop_geometry() |>
  transmute(ym = floor_date(date, unit = "month")) |>
  count(ym) |>
  ggplot() +
  geom_col(aes(x = ym, y = n)) +
  xlab("Time") +
  ylab("count")
gg_save("ts_fire_counts_m", plt = plt_ts_fire_counts_m, destination_dir = plt_dir)

# TODO: y-axis count
plt_ts_fire_barea_m <- fires |>
  transmute(ym = floor_date(date, unit = "month"), log_area = log10(1+area)) |>
  st_drop_geometry() |>
  group_by(ym) |>
  summarize(sum_log_area = sum(log_area), count = n()) |>
  ggplot() +
  geom_col(aes(x = ym, y = count, fill = "Count")) +
  geom_col(aes(x = ym, y = - sum_log_area, fill = "Burnt Area")) +
  scale_y_continuous(labels = function(x) abs(x)) +
  scale_fill_manual(name = "", values = c("Burnt Area" = "grey", "Count" = "red")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(y = "log(area+1), count", x = "Time")
gg_save("ts_fire_barea_m",plt = plt_ts_fire_barea_m, destination_dir = plt_dir)

plt_ts_cummulative_count_d <- fires |>
  st_drop_geometry() |>
  select(date) |>
  count(date) |>
  arrange(date) |>
  mutate(cumcount = cumsum(n)) |>
  ggplot() +
  geom_line(aes(x = date, y= cumcount)) +
  labs(x = "Time", y = "cummulative counts")
gg_save("ts_cummulative_count_d",plt = plt_ts_cummulative_count_d, destination_dir = plt_dir)

plot_grid(plt_point_fires_barea_total + theme(aspect.ratio = 1),
          plt_ts_fire_counts_m + theme(aspect.ratio = 1),
          nrow = 1, ncol = 2,
          align = "hv", axis = "tblr")
gg_save("patch_count_ts", destination_dir = plt_dir)


fires |>
  st_drop_geometry() |>
  transmute(area = log1p(area)) |>
  ggplot() +
  geom_histogram(aes(x=area), bins = ceiling(sqrt(nrow(fires)))) +
  xlab("log(1+area)")
gg_save("hist_log_barea", destination_dir = plt_dir)

fires |>
  st_drop_geometry() |>
  ggplot() +
  geom_histogram(aes(x=area), bins = ceiling(sqrt(nrow(fires)))) +
  xlab("area")
gg_save("hist_barea", destination_dir = plt_dir)

plt_heat_fires_count <- fires |>
  mutate(year = year(date), month = lubridate::month(date, label = TRUE, abbr = TRUE)) |>
  count(year, month) |>
  complete(year, month) |>
  ggplot(aes(x = month, y = factor(year), fill = n)) +
  geom_tile(color = "white") +
  labs(x = "Month", y = "Year") +
  scale_fill_viridis_c(name = "Count", na.value = "gray90") +
  theme(panel.grid = element_blank(), legend.direction = "vertical")
gg_save("heat_fires_count",plt = plt_heat_fires_count, destination_dir = plt_dir)

wrap_plots(plt_point_fires_total_ecoz,
           plt_ts_cummulative_count_d,
           nrow = 1, ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
gg_save("patch_point_ecoz_cumcount", destination_dir = plt_dir)


wrap_plots(plt_heat_fires_count,
           plt_ts_fire_counts_m,
           nrow = 1, ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.direction = "horizontal")
gg_save("patch_heat_countts", destination_dir = plt_dir)


wrap_plots(plt_point_fires_total_ecoz + coord_sf(expand = FALSE),
           plt_ts_cummulative_count_d + theme(legend.position = "none"),
           plt_heat_fires_count,
           plt_ts_fire_counts_m + theme(legend.position = "none"),
           nrow = 2, ncol = 2) +
  plot_layout(guides = "collect",
              widths = c(1.3, 1.0),
              heights = c(1.2, 1.0)) +
  plot_annotation(tag_levels = 'a') & 
  theme(legend.position='bottom')
gg_save("patch_cumcount_count_heat_point", destination_dir = plt_dir)



plot_grid(plot_grid(plt_ts_cummulative_count_d + theme(aspect.ratio = 1,plot.margin = subplot_margins),
                    plt_ts_fire_barea_m + theme(legend.position = "none", aspect.ratio = 1, plot.margin = subplot_margins),
                    plt_heat_fires_count + theme(legend.position = "none", aspect.ratio = 1, plot.margin = subplot_margins),
                    plt_point_fires_barea_total + theme(aspect.ratio = 1, plot.margin = subplot_margins),
                    nrow = 2, ncol = 2),
          plot_grid(get_legend(plt_heat_fires_count + theme(legend.position = "bottom", legend.justification = "right")),
                    get_legend(plt_ts_fire_barea_m + theme(legend.position = "bottom", legend.justification = "left")),
                    nrow = 1, ncol = 2),
          nrow = 2, ncol = 1, rel_heights = c(1, 0.1))
gg_save("patch_cumcount_countbarea_heat_point", destination_dir = plt_dir)