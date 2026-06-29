source("R/utils/data_queries.R")
source("R/utils/plot_functions.R")
source("R/utils/functions.R")
#-------------------------------------------------------------------------------
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
conf <- get_conf()
crs <- st_crs(read_conf(conf, "crs"))
dir_modis <- path(here(), "assets", "plots", "modis")
dir_prism <- path(here(), "assets", "plots", "prism")
dir_results <- path(here(), "assets", "plots", "results")
dir_fire <- path(here(), "assets", "plots", "fire")
dir_misc <- path(here(), "assets", "plots", "misc")
walk(list(dir_fire, dir_modis, dir_prism, dir_misc),
     dir_create)


fires <- read_sf_frame(sf_crs = crs, filename = "calfire")
fires <- fires |>
  mutate(area = area / 247.1) # acres in km2

barea <- read_sf_frame(sf_crs = crs, filename = "burntarea")
barea <- barea |>
  # TODO: crop area to ca (?) does not work here...
  st_intersection(read_sf_frame("ca_state", crs))

timeconstraint <- TRUE

no_leg   <- theme(legend.position = "none")
no_yax   <- theme(axis.title.y = element_blank())
no_xax   <- theme(axis.title.x = element_blank())

no_legax  <- theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
no_legyax <- theme(legend.position = "none", axis.title.y = element_blank())
no_legxax <- theme(legend.position = "none", axis.title.x = element_blank())
#-------------------------------------------------------------------------------
# WILDFIRES
ggplot() +
  get_layer_ca(crs) + 
  get_layer_fire(fires) +
  facet_wrap(~ lubridate::year(date)) +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), aspect.ratio = 1)
gg_save("point_fires_facet_year", destination_dir = dir_fire, width = 7, height = 10)


p_point_fires_barea_total <- ggplot() +
  get_layer_ca(crs) +
  get_layer_barea(barea, alpha = 0.8) +
  get_layer_fire(fires, alpha = 0.2) +
  xlab("Longitude") +
  ylab("Latitude")
p_point_fires_barea_total
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
gifski::gifski(dir_ls(dir_fire, regexp = "point_fires_\\d{4}"),
               path(dir_fire, "fires.gif"),
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
  get_layer(fires, "fire") + 
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
fires |>
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


plot_grid(p_point_fires_barea_total, plt_ts_fire_counts_m,
          nrow = 1, 
          ncol = 2,
          align = "hy",
          axis = "tblr",
          labels = c("a)", "b)"))
gg_save("patch_count_ts", destination_dir = dir_fire)


## time series total wildfires ecozone
# TODO:count > 100 Cal High North dominating scale, to bottom with own scale?
fires |> 
  add_layer(select(read_sf_frame(keyword = "ecoz3", crs), "US_L3NAME")) |>
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
  mutate(year = year(date), month = month(date, label = TRUE, abbr = TRUE)) |>
  count(year, month) |>
  complete(year, month) |>
  ggplot(aes(x = month, y = factor(year), fill = n)) +
  geom_tile(color = "white") + 
  labs(x = "Month", y = "Year") +
  scale_fill_viridis_c(name = "Count", na.value = "white") +
  theme(panel.grid = element_blank())
plt_heat_fires_count
gg_save("heat_fires_count", destination_dir = dir_fire)


plot_grid(p_ts_cummulative_count_d,
          p_ts_fire_counts_m,
          p_heat_fires_count + theme(legend.position = "bottom"),
          p_point_fires_barea_total,
          ncol = 2,
          nrow = 2, 
          align = "hy",
          axis = "tblr",
          labels = c("a)", "b)", "c)", "d)"))
gg_save("patch_cumcount_count_heat_point", destination_dir = dir_fire)

#-------------------------------------------------------------------------------
#                                 PRISM/MODIS
#-------------------------------------------------------------------------------
tmp <- plot_and_save_frame("ppt", dir_prism)
plt_lon_ppt <- tmp[[1]]
plt_lat_ppt <- tmp[[2]]

tmp <- plot_and_save_frame("vpdmax", dir_prism)
plt_lon_vpdmax <- tmp[[1]]
plt_lat_vpdmax <- tmp[[2]]


tmp <- plot_and_save_frame("tmax", dir_prism)
plt_lon_tmax <- tmp[[1]]
plt_lat_tmax <- tmp[[2]]


plt_prism_lonlat <- plot_grid(plot_grid(
  plt_lon_ppt + no_legxax,    plt_lat_ppt + no_legax,
  plt_lon_vpdmax + no_legxax + no_xax, plt_lat_vpdmax + no_legax,
  plt_lon_tmax + no_legend,         plt_lat_tmax  + no_legyax,
  nrow = 3,
  ncol = 2,
  align = "hv",
  axis = "tblr"),
  get_legend(plt_lon_ppt + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)) + 
        guides(color = guide_legend(nrow = 1, override.aes = list(size = 5)))),
        ncol = 1,
        rel_heights = c(1, 0.08))
plt_prism_lonlat
gg_save("patch_lonlat_prism", destination_dir = dir_prism)


tmp <- plot_and_save_frame("lai_Lai_500m", variable_name = "lai", dir_modis)
plt_lon_lai <- tmp[[1]]
plt_lat_lai <- tmp[[2]]
tmp <- plot_and_save_frame("evi__500m_16_days_EVI", variable_name = "evi", dir_modis)
plt_lon_evi <- tmp[[1]]
plt_lat_evit <- tmp[[2]]
# TODO: only one var
tmp <- plot_and_save_frame("dem_NASADEM_HGT", variable_name = "dem", dir_modis)

# TODO: categorical var
# plot_and_save_frame("landcover_LC_Type5", variable_name = "landcover_5", dir_modis)
# plot_and_save_frame("landcover_LC_Type4", variable_name = "landcover_4", dir_modis)
# plot_and_save_frame("landcover_LC_Type3", variable_name = "landcover_3", dir_modis)
# plot_and_save_frame("landcover_LC_Type2", variable_name = "landcover_2", dir_modis)
# plot_and_save_frame("landcover_LC_Type1", variable_name = "landcover_1", dir_modis)


tif_prism<- c(tmax = "tmax.tif", vpdmax = "vpdmax.tif", ppt = "ppt.tif")
tif_modis <- c(evi = "evi__500m_16_days_EVI.tif",
               lai = "lai_Lai_500m.tif",
               lc5 = "landcover_LC_Type5.tif",
               lc4 = "landcover_LC_Type4.tif",
               lc3 = "landcover_LC_Type3.tif",
               lc2 = "landcover_LC_Type2.tif",
               lc1 = "landcover_LC_Type1.tif",
               dem = "dem_NASADEM_HGT.tif")
filenames_tif <- c(tif_prism, tif_modis)
tifs <- path(here(), "data", "preprocessed", filenames_tif)
names(tifs) <- names(filenames_tif)



# PNGs  for each year
if (!timeconstraint) {
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
#-------------------------------------------------------------------------------
#                                   EVI
#-------------------------------------------------------------------------------
evi <- rast(path("data", "preprocessed", "evi.tif"))
evi_m <- split_names(names(evi), "m")
evi_y <- split_names(names(evi), "y")


rast_evi_m_median <- agg_raster(evi, evi_m, func = median)
names(rast_evi_m_median) <- month.name
rast_evi_m_mean <- agg_raster(evi, evi_m, func = mean)
names(rast_evi_m_mean) <- month.name
rast_evi_m_sd <- agg_raster(evi, evi_m, func = sd)
names(rast_evi_m_sd) <- month.name

rast_evi_y_median <- agg_raster(evi, evi_y, func = median)
names(rast_evi_y_median) <- 2000:2025
rast_evi_y_mean <- agg_raster(evi, evi_y, func = mean)
names(rast_evi_y_mean) <-2000:2025
rast_evi_y_sd <- agg_raster(evi, evi_y, func = sd)
names(rast_evi_y_sd) <- 2000:2025

plot_and_save_raster(rast_evi_y_median, "evi_y_median", dir_veg)
plot_and_save_raster(rast_evi_y_mean, "evi_y_mean", dir_veg)
plot_and_save_raster(rast_evi_y_sd, "evi_y_sd", dir_veg)

plot_and_save_raster(rast_evi_m_median, "evi_m_median", dir_veg)
plot_and_save_raster(rast_evi_m_mean, "evi_m_mean", dir_veg)
plot_and_save_raster(rast_evi_m_sd, "evi_m_sd", dir_veg)

#-------------------------------------------------------------------------------
#                                   MISC
#-------------------------------------------------------------------------------

# ELEVATION"data/preprocessed/dem_NASADEM_HGT.tif""data/preprocessed/dem_NASADEM_HGT.tif"
dem <- rast(path("data", "preprocessed", "dem_NASADEM_HGT.tif"))
limits <- c(min(global(dem, "min", na.rm = TRUE)), max(global(dem, "max", na.rm = TRUE)))

p_dem <- ggplot() +
  geom_spatraster(data = dem) +
  scale_fill_viridis_c(na.value = NA,
                       name = "m above sealevel",
                       guide = "colorbar",
                       limits = limits) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude")
p_dem
gg_save("dem", destination_dir = dir_top)

p_ecoz <- ggplot () +
  get_layer_ecoz(crs, legend = TRUE) +
  labs(x = "Longitude", y = "Latitude") +
  guides(fill=guide_legend(title="")) +
  theme(legend.position = "bottom",
        legend.text = element_text(size=7),
        legend.title = element_text(size=9))

p_ecoz
gg_save("ecozone", destination_dir = dir_top)
p_ecoz_legend <- cowplot::get_legend(p_ecoz)

p_top <- (p_dem + theme(plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 5.5, unit = "pt"))) |
  (p_ecoz + theme(legend.position = "none", plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 5.5, unit = "pt")))

p_top / wrap_elements(p_ecoz_legend) +
  plot_layout(heights = unit(c(1, 1.95), c("null", "cm"))) +
  plot_annotation(tag_levels = list(c("a", "b")))

gg_save("patch_dem_ecoz", destination_dir = dir_top)
#-------------------------------------------------------------------------------
aspect <- terrain(dem, v = "aspect", unit = "degrees")
northness <- cos(aspect * pi / 180) # +1 north, -1 south
eastness <- sin(aspect * pi /180) # +1 east, -1 west
slope <- terrain(dem, v = "slope", unit = "degrees")
#-------------------------------------------------------------------------------
plot_ripleyk()
gg_save("ripleyk")
