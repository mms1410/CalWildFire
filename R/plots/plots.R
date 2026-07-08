library(fs)
library(purrr)
library(here)
library(sf)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(tidyterra)
library(scales)
library(units)
#-------------------------------------------------------------------------------
source("R/utils/data_queries.R")
source("R/plots/plot_functions.R")
#-------------------------------------------------------------------------------
conf <- get_conf()
crs <- st_crs(read_conf(conf, "crs"))
dir_modis <- path(here(), "assets", "plots", "modis")
dir_prism <- path(here(), "assets", "plots", "prism")
dir_results <- path(here(), "assets", "plots", "results")
dir_fire <- path(here(), "assets", "plots", "fire")
dir_misc <- path(here(), "assets", "plots", "misc")
walk(list(dir_fire, dir_modis, dir_prism, dir_misc), dir_create)
no_time <- TRUE

subplot_margins <-  margin(t = 2, r = 2, b = 2, l = 2, unit = "pt")
no_leg   <- theme(legend.position = "none")
no_yax   <- theme(axis.title.y = element_blank())
no_xax   <- theme(axis.title.x = element_blank())
no_legax  <- theme(legend.position = "none",
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank())
no_legyax <- theme(legend.position = "none", axis.title.y = element_blank())
no_legxax <- theme(legend.position = "none", axis.title.x = element_blank())
#-------------------------------------------------------------------------------
# WILDFIRES
fires <- read_geodata(sf_crs = crs, filename = "calfire")
fires <- fires |>
  mutate(area = area / 247.1) # acres in km2

barea <- read_geodata(sf_crs = crs, filename = "burntarea")
barea <- barea # TODO: crop ca

ggplot() +
  get_layer_ca(crs) +
  get_layer_fire(fires) +
  facet_wrap(~ lubridate::year(date)) +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), aspect.ratio = 1)
gg_save("point_fires_facet_year", destination_dir = dir_fire, width = 7, height = 10)


plt_point_fires_barea_total <- ggplot() +
  get_layer_ca(crs) +
  get_layer_barea(barea, alpha = 0.8) +
  get_layer_fire(fires, alpha = 0.2) +
  xlab("Longitude") +
  ylab("Latitude")
plt_point_fires_barea_total
gg_save("point_fires_barea_total", destination_dir = dir_fire)

for (y in year(fires$date) |> unique()) {
  ggplot() +
    get_layer_ca(crs) +
    get_layer_fire(filter(fires, year(date) == y), alpha = 0.9) +
    get_layer_barea(filter(barea, year(date) == y), alpha = 0.7) +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle(y)
gg_save(paste0("point_fires_", y), destination_dir = dir_fire)
}
gifski(dir_ls(dir_fire, regexp = "point_fires_\\d{4}"),
       path(dir_fire, "point_fires.gif"),
       delay = 0.5,
       width = 800,
       height = 600)

ggplot() +
  get_layer_ca(crs) +
  get_layer_fire(fires, mark = log1p(fires$area), size = 0.3, alpha = 0.4) +
  scale_color_viridis_c(option = "inferno", trans = "log", name = "log(1+area)") +
  xlab("Longitude") +
  ylab("Latitude")
gg_save("point_fires_total_mark", destination_dir = dir_fire)

ggplot() +
  get_layer_fire(fires) +
  #get_layer(fires, "fire") +
  get_layer_ecoz(crs, legend = TRUE) +
  theme(legend.position = "right", legend.text = element_text(size = 8)) +
  labs(fill = "") +
  xlab("Longitude") +
  ylab("Latitude")
gg_save("point_fires_total_ecoz", destination_dir = dir_fire)

plt_ts_fire_counts_m <- fires |>
  select(date) |>
  st_drop_geometry() |>
  transmute(ym = floor_date(date, unit = "month")) |>
  count(ym) |>
  ggplot() +
  geom_col(aes(x = ym, y = n)) +
  xlab("Time") +
  ylab("count")
plt_ts_fire_counts_m
gg_save("ts_fire_counts_m", destination_dir = dir_fire)

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
plt_ts_fire_barea_m
gg_save("ts_fire_barea_m", destination_dir = dir_fire)

plt_ts_cummulative_count_d <- fires |>
  st_drop_geometry() |>
  select(date) |>
  count(date) |>
  arrange(date) |>
  mutate(cumcount = cumsum(n)) |>
  ggplot() +
  geom_line(aes(x = date, y= cumcount)) +
  labs(x = "Time", y = "cummulative counts")
plt_ts_cummulative_count_d
gg_save("ts_cummulative_count_d", destination_dir = dir_fire)


plot_grid(plt_point_fires_barea_total + theme(aspect.ratio = 1),
          plt_ts_fire_counts_m + theme(aspect.ratio = 1),
          nrow = 1, ncol = 2,
          align = "hv", axis = "tblr")
gg_save("patch_count_ts", destination_dir = dir_fire)

## time series total wildfires ecozone
# TODO:count > 100 Cal High North dominating scale, to bottom with own scale?
fires |>
  add_layer(select(read_geodata(keyword = "ecoz3", crs), "US_L3NAME")) |>
  st_drop_geometry() |>
  mutate(ym = floor_date(date, "month"), ecoz = US_L3NAME) |>
  filter(!is.na(ecoz)) |>
  select(ym, ecoz)|>
  group_by(ecoz, ym) |>
  summarize(count = n(), .goups = "drop") |>
  ggplot() +
  geom_col(aes(x = ym, y = count)) +
  xlab("Time") +
  facet_wrap(~ecoz)
gg_save("ts_fire_facet_ecoz", destination_dir = dir_fire)

fires |>
  st_drop_geometry() |>
  transmute(area = log1p(area)) |>
  ggplot() +
  geom_histogram(aes(x=area), bins = ceiling(sqrt(nrow(fires)))) +
  xlab("log(1+area)")
gg_save("hist_log_barea", destination_dir = dir_fire)

fires |>
  st_drop_geometry() |>
  ggplot() +
  geom_histogram(aes(x=area), bins = ceiling(sqrt(nrow(fires)))) +
  xlab("area")
gg_save("hist_barea", destination_dir = dir_fire)

plt_heat_fires_count <- fires |>
  mutate(year = year(date), month = lubridate::month(date, label = TRUE, abbr = TRUE)) |>
  count(year, month) |>
  complete(year, month) |>
  ggplot(aes(x = month, y = factor(year), fill = n)) +
  geom_tile(color = "white") +
  labs(x = "Month", y = "Year") +
  scale_fill_viridis_c(name = "Count", na.value = "white") +
  theme(panel.grid = element_blank())
plt_heat_fires_count
gg_save("heat_fires_count", destination_dir = dir_fire)


plot_grid(plot_grid(plt_ts_cummulative_count_d + theme(aspect.ratio = 1,plot.margin = subplot_margins),
                    plt_ts_fire_counts_m + theme(aspect.ratio = 1, plot.margin = subplot_margins),
                    plt_heat_fires_count + theme(legend.position = "none", aspect.ratio = 1, plot.margin = subplot_margins),
                    plt_point_fires_barea_total + theme(aspect.ratio = 1, plot.margin = subplot_margins),
                    nrow = 2, ncol = 2),
          get_legend(plt_heat_fires_count + theme(legend.position = "bottom", legend.justification = "left")),
          nrow = 2, ncol = 1, rel_heights = c(1, 0.1))
gg_save("patch_cumcount_count_heat_point", destination_dir = dir_fire)


plot_grid(plot_grid(plt_ts_cummulative_count_d + theme(aspect.ratio = 1,plot.margin = subplot_margins),
                    plt_ts_fire_barea_m + theme(legend.position = "none", aspect.ratio = 1, plot.margin = subplot_margins),
                    plt_heat_fires_count + theme(legend.position = "none", aspect.ratio = 1, plot.margin = subplot_margins),
                    plt_point_fires_barea_total + theme(aspect.ratio = 1, plot.margin = subplot_margins),
                    nrow = 2, ncol = 2),
          plot_grid(get_legend(plt_heat_fires_count + theme(legend.position = "bottom", legend.justification = "right")),
                    get_legend(plt_ts_fire_barea_m + theme(legend.position = "bottom", legend.justification = "left")),
          nrow = 1, ncol = 2),
          nrow = 2, ncol = 1, rel_heights = c(1, 0.1))
gg_save("patch_cumcount_countbarea_heat_point", destination_dir = dir_fire)

rm(fires, barea)
rm(list = ls(pattern = "^plt"))
gc()
#-------------------------------------------------------------------------------
#                                 PRISM
#-------------------------------------------------------------------------------
# PRISM
tmp_ppt <- plot_and_save_frame("ppt", dir_prism)
tmp_vpdmax <- plot_and_save_frame("vpdmax", dir_prism)
tmp_tmax <- plot_and_save_frame("tmax", dir_prism)

plot_list <- list(tmp_ppt[[1]] + no_legxax ,
                  tmp_ppt[[2]] + no_legyax ,
                  tmp_vpdmax[[1]] + no_legxax ,
                  tmp_vpdmax[[2]] + no_legyax ,
                  tmp_tmax[[1]] + no_leg ,
                  tmp_tmax[[2]] + no_legyax )


# TODO: axis
plot_grid(plotlist = plot_list, nrow = 3, ncol = 2)
gg_save("patch_lonlat_prism", destination_dir = dir_prism, set_theme = FALSE)

rm(list = ls(pattern = "^tmp"))
rm(list = ls(pattern = "^plt"))
gc()
#-------------------------------------------------------------------------------
#                                 MODIS
#-------------------------------------------------------------------------------
# MODIS
tmp_lai <- plot_and_save_frame("lai_Lai_500m", dir_modis, variable_name = "lai")
tmp_evi <- plot_and_save_frame("evi__500m_16_days_EVI", dir_modis, variable_name = "evi")
tmp_dem <- plot_and_save_frame("dem_NASADEM_HGT", variable_name = "dem", dir_modis)
tmp_aspect <- plot_and_save_frame("aspect_NASADEM_HGT", variable_name = "aspect(deg)", filename = "aspect", dir_modis)
tmp_slope <- plot_and_save_frame("slope_NASADEM_HGT", variable_name = "slope(deg)", filename = "slope", dir_modis)

plot_list <- list(tmp_dem[[1]] + no_legxax ,
                  tmp_dem[[2]] + no_legax ,
                  tmp_aspect[[1]] + no_legxax ,
                  tmp_aspect[[2]] + no_legax ,
                  tmp_slope[[1]] + no_legxax ,
                  tmp_slope[[2]] + no_legax ,
                  tmp_lai[[1]] + no_legxax ,
                  tmp_lai[[2]] + no_legax ,
                  tmp_evi[[1]] + no_leg ,
                  tmp_evi[[2]] + no_legyax )


plot_grid(plotlist = plot_list, nrow = 5, ncol = 2)
gg_save("patch_lonlat_modis_all", destination_dir = dir_modis, set_theme = FALSE)


plot_grid(plotlist = tail(plot_list, 4), nrow = 2, ncol = 2)
gg_save("patch_lonlat_modis_evilai", destination_dir = dir_modis)


plot_list <- list(tmp_dem[[1]] + no_legxax,
                  tmp_dem[[2]] + no_legax,
                  tmp_aspect[[1]] + no_legxax,
                  tmp_aspect[[2]] + no_legax,
                  tmp_slope[[1]],
                  tmp_slope[[2]] + no_legyax)
plot_grid(plotlist = plot_list, nrow = 3, ncol = 2)
gg_save("patch_lonlat_demall", destination_dir = dir_modis)


plt_dem <- ggplot() +
  geom_spatraster(data = read_geodata("tif", sf_crs = crs, filename = "dem")) +
  scale_fill_viridis_c(na.value = NA, name = "elevation", guide = "colorbar") +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude")
gg_save("dem", plt = plt_dem, destination_dir = dir_modis)

plt_aspect <- ggplot() +
  geom_spatraster(data = read_geodata("tif", crs, "aspect")) +
  scale_fill_viridis_c(na.value = NA, name = "aspect(deg)", guide = "colorbar") +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude")
gg_save("aspect", plt = plt_aspect, destination_dir = dir_modis)

plt_slope <- ggplot() +
  geom_spatraster(data = read_geodata("tif", crs, "slope")) +
  scale_fill_viridis_c(na.value = NA, name = "slope(deg)", guide = "colorbar") +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude")
gg_save("slope", destination_dir = dir_modis)

# TODO color
plt_ecoz <- ggplot () +
  get_layer_ecoz(crs, legend = TRUE) +
  labs(x = "Longitude", y = "Latitude") +
  guides(fill=guide_legend(title="")) +
  theme(legend.position = "bottom",
        legend.text = element_text(size=7),
        legend.title = element_text(size=9))
plt_ecoz
gg_save("ecozone", plt = plt_ecoz, destination_dir = dir_misc)


plot_grid(plot_grid(plt_dem + theme(legend.position = "none", aspect.ratio = 1),
                    plt_ecoz + theme(legend.position = "none", aspect.ratio = 1),
                    nrow = 1, ncol = 2),
          plot_grid(get_legend(plt_dem + theme(legend.position = "right", legend.justification = "right")),
                    get_legend(plt_ecoz + theme(legend.position = "bottom", legend.justification = "left")),
                    nrow = 1, ncol = 2, rel_widths = c(1, 6.5)),
          nrow = 2, ncol = 1, rel_heights = c(1, 0.4))
gg_save("patch_modis_demecoz", destination_dir = dir_modis)
rm(list = ls(pattern = "^tmp"))
rm(list = ls(pattern = "^plt"))
gc()
#-------------------------------- Landcover ------------------------------------
plot_and_save_lc <- function(type) {
  filename <- paste0("LC_Type", type)
  legendname <- paste0("Landcover(T", type, ")")

  p1 <- ggplot() +
    geom_spatraster(data = read_geodata("tif", crs, filename)) +
    scale_fill_d3(palette = "category20", na.value = "transparent", na.translate = FALSE, name = legendname) +
    facet_wrap(~lyr)
  gg_save(paste0(filename, "_years"), plt = p1, destination_dir = dir_modis)
  
  
  p2 <- ggplot() +
    geom_spatraster(data = read_geodata("tif", crs, filename) |>
                      terra::app(fun = "modal") |>
                      as.factor() |>
                      set_raster_level_lc(type = type)) +
    scale_fill_d3(palette = "category20", na.value = "transparent", na.translate = FALSE, name = legendname)
  gg_save(paste0(filename, "_modal"), plt = p2, destination_dir = dir_modis)
  c(p1, p2)
}

plot_and_save_lc("1")
plot_and_save_lc("2")
plot_and_save_lc("3")
plot_and_save_lc("4")
plot_and_save_lc("5")
#-------------------------------------------------------------------------------
tif_prism<- c(tmax = "tmax.tif", vpdmax = "vpdmax.tif", ppt = "ppt.tif")
tif_modis <- c(evi = "evi__500m_16_days_EVI.tif",
               lai = "lai_Lai_500m.tif",
               lc5 = "landcover_LC_Type5.tif",
               lc4 = "landcover_LC_Type4.tif",
               lc3 = "landcover_LC_Type3.tif",
               lc2 = "landcover_LC_Type2.tif",
               lc1 = "landcover_LC_Type1.tif")
filenames_tif <- c(tif_prism, tif_modis)
tifs <- path(here(), "data", "preprocessed", filenames_tif)
names(tifs) <- names(filenames_tif)
# PNGs  for each year
if (!no_time) {
  # TIME ESTIMATE: > 4H !!!
  for (var_name in names(tifs)) {
    tif_file <- tifs[var_name]
    raster <- rast(tif_file)
    if (basename(tif_file) %in% tif_prism){
      destination_dir <- path(here(), "assets", "plots", "prism")
    } else {
      destination_dir <- path(here(), "assets", "plots", "modis")
    }
    destination_dir <- path(destination_dir, var_name)
    dir_create(destination_dir)
    
    rast_min <- min(global(raster, "min", na.rm = TRUE))
    rast_max <- max(global(raster, "max", na.rm = TRUE))
    rastlyr_to_pngs(raster, var_name, destination_dir,
                    limits = c(rast_min, rast_max))
  }

  pattern <- paste0("(", paste(names(tifs), collapse = "|"), ")$")
  gif_folders <- dir_ls(path(here(), "assets"), type = "directory", recurse = TRUE, regex = pattern)
  for (gfolder in gif_folders) {
    all_pngs <- fs::dir_ls(gfolder, regexp = ".png")
    gifski(all_pngs,
          path(path_dir(gfolder), paste0(basename(gfolder), ".gif")),
          delay = 0.5,   # seconds per frame
          width = 800,
          height = 600)
  }
}
rm(list = ls(pattern = "^tmp"))
rm(list = ls(pattern = "^plt"))
gc()
#-------------------------------------------------------------------------------
# ROAD
rast_road <- read_geodata(keyword = "tif", sf_crs = crs, filename = "road")
plt_road_rast_hist <- ggplot() +
  geom_histogram(data = values(rast_road, na.rm = TRUE), aes(x = length)) +
  xlab("Road area per 4km^2")
gg_save("hist_road_rast", plt = plt_road_rast_hist, destination_dir = dir_misc)

rast_log <- log1p(rast_road)
min_val <- global(rast_log, "min", na.rm = TRUE)[1,1]
max_val <- global(rast_log, "max", na.rm = TRUE)[1,1]
rast_normalized <- (rast_log - min_val) / (max_val - min_val)

plt_road_rast_hist_trafo <- ggplot() +
  geom_histogram(data = values(rast_normalized, na.rm = TRUE), aes(x = length)) +
  xlab("Transformed road area per 4km^2")
gg_save("hist_road_rast_trafo", plt = plt_road_rast_hist_trafo, destination_dir = dir_misc)

plt_road_network <- ggplot() +
  geom_sf(data = read_geodata(keyword = "road_network", sf_crs = crs), linewidth = 0.1, alpha = 0.5) +
  labs(x = "Longitude", y = "Latitude")
gg_save("road_network", plt = plt_road_network, destination_dir = dir_misc)

plt_road_raster <- ggplot() +
  geom_spatraster(data = rast_normalized) +
  scale_fill_viridis_c(na.value = NA, name = "Road Density", guide = "colorbar") +
  labs(x = "Longitude", y = "Latitude")
gg_save("road_raster", plt = plt_road_raster, destination_dir = dir_misc)


plot_grid(plt_road_network + theme(aspect.ratio = 1),
          plt_road_raster + theme(aspect.ratio = 1) + no_legyax,
          plt_road_rast_hist + theme(aspect.ratio = 1),
          plt_road_rast_hist_trafo + theme(aspect.ratio = 1),
          nrow = 2)
gg_save("patch_road", destination_dir = dir_misc)
