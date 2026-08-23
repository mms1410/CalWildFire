library(fs)
library(terra)
library(ggplot2)
library(tidyterra)
library(patchwork)
#-------------------------------------------------------------------------------
source("R/utils.R")
source("R/const.R")
source("R/plots/utils.R")
#-------------------------------------------------------------------------------
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
