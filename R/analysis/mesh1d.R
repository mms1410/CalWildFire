suppressPackageStartupMessages({
library(tidyr)
library(lubridate)
library(ggplot2)
library(fmesher)
})
#-------------------------------------------------------------------------------
source("R/utils.R")
source("R/plots/utils.R")
config_data <- readYaml("data")
config_mesh <- readYaml("mesh")
end_train <- config_data$end_train
start_train <- config_data$start_train
destination_dir_mesh <- dir_create(path(getwd(), "assets", "mesh", "mesh1d"))
#destination_regular <- dir_create(destination_dir_mesh, "regular")
#destination_irreg <- dir_create(destination_dir_mesh, "irregular")
destination_dir_plot <- dir_create(path(getwd(), "assets", "plots", "mesh1d"))
set.seed(123)
#-------------------------------------------------------------------------------
make_mesh1d <- function(dataset, n_knots) {

  # Density-based sampling for irregular knots
  dens <- density(as.numeric(dataset$date))
  dens_fun <- approxfun(dens$x, dens$y)
  dens_weight_date <- dens_fun(as.numeric(dataset$date))
  
  # Ensure sampled dates are sorted for 1D mesh construction
  time_seq_irreg <- dataset |>
    slice_sample(n = n_knots, weight_by = dens_weight_date) |>
    pull(date) |>
    sort()
  
  
  # Regular date sequence using full year bounds
  date_min <- as.Date(paste0(min(year(dataset$date)), "-01-01"))
  date_max <- as.Date(paste0(max(year(dataset$date)), "-12-31"))
  
  time_seq_reg_numeric <- seq(from = as.numeric(date_min),
                      to = as.numeric(date_max),
                      length.out = n_knots)
  time_seq_reg <- as.Date(time_seq_reg_numeric, origin = "1970-01-01")
  
  # Create 1D Meshes
  mesh_reg <- fm_mesh_1d(loc = as.numeric(time_seq_reg))
  mesh_irreg <- fm_mesh_1d(loc = as.numeric(time_seq_irreg))
  
  
  knots_df <- data.frame(
    date = c(time_seq_reg, time_seq_irreg),
    type = factor(rep(c("Regular", "Irregular"), each = n_knots)))
  
  plt <- ggplot(dataset, aes(x = date)) +
    geom_density(fill = "grey80", alpha = 0.5, color = "grey40") +
    geom_rug(data = knots_df, aes(x = date, color = type), sides = "b", length = unit(0.05, "npc"), alpha = 0.8) +
    scale_color_manual(values = c("Regular" = "blue", "Irregular" = "red")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(y = "Density", x = "Date", color = "Mesh Knots",
         caption = paste0("(Knots: ", n_knots, ")"))
  
  plt <- ggplot(dataset, aes(x = date)) +
    geom_density(fill = "grey80", alpha = 0.5, color = "grey40") +
    geom_rug(data = subset(knots_df, type == "Regular"), aes(x = date, color = type),
             sides = "t", length = unit(0.05, "npc"), alpha = 0.8, linewidth = 0.2) +
    geom_rug(data = subset(knots_df, type == "Irregular"), aes(x = date, color = type),
             sides = "b", length = unit(0.05, "npc"), alpha = 0.8, linewidth = 0.2) +
    scale_color_manual(values = c("Regular" = "blue", "Irregular" = "red")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(y = "Density", x = "Date", color = "Mesh Knots",
         caption = paste0("(Knots: ", n_knots, ")"))
  return(list(mesh_reg = mesh_reg, 
              mesh_irreg = mesh_irreg,
              plt = plt))
}

fires <- readFile(path(getwd(), "data", "preprocessed", "calfire.gpkg")) |>
  filter(year(date) <= end_train)

config_mesh <- config_mesh$mesh_time
for (mesh_name in names(config_mesh)) {
  
  total_knots <- config_mesh[[mesh_name]][["total_knots"]]
  mesh_plt <- make_mesh1d(dataset = fires, n_knots = total_knots)
  
  gg_save(mesh_name, mesh_plt$plt, destination_dir_plot)
  saveRDS(mesh_plt$mesh_irreg, path(destination_dir_mesh, paste0(mesh_name, "_irreg.rds")))
  saveRDS(mesh_plt$mesh_reg, path(destination_dir_mesh, paste0(mesh_name, "_reg.rds")))
  
}