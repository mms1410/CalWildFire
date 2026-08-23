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
