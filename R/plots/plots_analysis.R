library(ggplot2)
library(sf)
library(here)
library(fs)
library(dplyr)
library(tidyr)
library(GGally)
#-------------------------------------------------------------------------------
source(path(here(), "R", "plots", "defaults.R"))
#-------------------------------------------------------------------------------
plotCovarHist <- function(int_pts, fires) {
  
  no_covar <- c("geometry", "time", "date")
  fixed_effects_names <- setdiff(intersect(colnames(fires), colnames(int_pts)), no_covar)
  
  fixed_effects <- bind_rows(select(st_drop_geometry(int_pts), all_of(fixed_effects_names)),
            select(st_drop_geometry(fires), all_of(fixed_effects_names)))
  
 fixed_effects_long <- fixed_effects |>
   pivot_longer(everything(), names_to = "variable", values_to = "value") 
 
 summary(fixed_effects$distance_road)
 # road_distance: gamma
 # evi: truncated normal [-1, 1]
 # lai: tweedie
 
 
 
 
}