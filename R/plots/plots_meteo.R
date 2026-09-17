library(fs)
library(terra)
library(tidyterra)
library(ggplot2)
library(patchwork)
#-------------------------------------------------------------------------------
source("R/grand_util.R")
source("R/plots/utils.R")
source("R/constants.R")
#-------------------------------------------------------------------------------
agg_plt <- function(raster, name) {
  
  if (is.character(raster)) {
    raster <- read_file(raster)
  }
  
  rast_agg <- terra::tapp(raster, "month", mean)
  names(rast_agg) <- month.name
  ggplot() +
    geom_spatraster(data = rast_agg) +
    facet_wrap(~lyr) +
    scale_fill_viridis_c(na.value = NA, name = name, guide = "colorbar") +
    theme(axis.text.x = element_text(angle = 80, hjust = 1))
}

destination_dir <- path(here(), "assets", "plots", "meteo")
dir_create(destination_dir)
#-------------------------------------------------------------------------------
ppt_frame <- read_file("ppt_long.csv")
tmp_ppt <- plot_and_save_frame(ppt_frame, destination_dir,
                               "Precipitation", "ppt")

vpdmax_frame <- read_file("vpdmax_long.csv")
tmp_vpdmax <- plot_and_save_frame(vpdmax_frame, destination_dir,
                                  "Max. VPD", "vpdmax")

tmax_frame <- read_file("tmax_long.csv")
tmp_tmax <- plot_and_save_frame(tmax_frame, destination_dir,
                                "Max. Temp.", "tmax")

vs_frame <- read_file("vs_long.csv")
tmp_vs <- plot_and_save_frame(vs_frame, destination_dir,
                              "Wind Speed", "vs")

plot_list <- list(tmp_ppt[[1]] + no_xax ,
                  tmp_ppt[[2]] + no_legax ,
                  tmp_vpdmax[[1]] + no_legxax ,
                  tmp_vpdmax[[2]] + no_legax ,
                  tmp_tmax[[1]] + no_xax ,
                  tmp_tmax[[2]] + no_legax,
                  tmp_vs[[1]] + no_leg,
                  tmp_vs[[2]] + no_legyax)

wrap_plots(plot_list, nrow = 4, ncol = 2) +
  plot_layout(guides = "collect", widths = c(1, 1), heights = c(1, 1))
gg_save("patch_lonlat_meto", destination_dir = destination_dir, set_theme = FALSE)


agg_plt("ppt.tif", "ppt")
gg_save("ppt_month", destination_dir = destination_dir)

agg_plt("vpdmax.tif", "vpd")
gg_save("vpdmax_month", destination_dir = destination_dir)

agg_plt("tmax.tif", "tmax")
gg_save("tmax_month", destination_dir = destination_dir)

agg_plt("vs.tif", "vs")
gg_save("vs_month", destination_dir = destination_dir)
plotlist_and_save <- function(framename, yname, plotfilename) {
  tmp_frame <- read_file(framename)
  plot_and_save_frame(tmp_frame, destination_dir,yname, plotfilename)
}

destination_dir <- path(here(), "assets", "plots", "modis")
dir_create(destination_dir)
#-------------------------------------------------------------------------------
pltlist_lai <- plotlist_and_save("lai_Lai_500m_long.csv", "Leaf Area Index (LAI)", "lai")
pltlist_evi <- plotlist_and_save("evi__500m_16_days_EVI_long.csv", "Enhanced Vegitation Index (EVI)", "evi")
pltlist_dem <- plotlist_and_save("dem_NASADEM_HGT_long.csv", "DEM", "dem")
pltlist_aspect <- plotlist_and_save("aspect_NASADEM_HGT_long.csv", "aspect(deg)", "aspect")
pltlist_slope <- plotlist_and_save("slope_NASADEM_HGT_long.csv", "slope(deg)", "slope")


plot_list <- list(pltlist_lai[[1]] + no_xax ,
                  pltlist_lai[[2]] + no_legax ,
                  pltlist_evi[[1]] + no_leg ,
                  pltlist_evi[[2]] + no_legyax)
patchwork::wrap_plots(plot_list, nrow = 2, ncol = 2) +
  plot_layout(guides = "collect", widths = c(1, 1), heights = c(1, 1))
gg_save("patch_lonlat_laievi", destination_dir = destination_dir)

plot_list <- list(pltlist_dem[[1]] + no_xax ,
                  pltlist_dem[[2]] + no_legax ,
                  pltlist_slope[[1]] + no_xax ,
                  pltlist_slope[[2]] + no_legax ,
                  pltlist_aspect[[1]] + no_leg ,
                  pltlist_aspect[[2]] + no_legyax)
patchwork::wrap_plots(plot_list, nrow = 3, ncol = 2) +
  plot_layout(guides = "collect", widths = c(1, 1), heights = c(1, 1))
gg_save("patch_lonlat_dem", destination_dir = destination_dir)