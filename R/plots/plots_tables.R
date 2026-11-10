library(fs)
library(data.table)
library(ggplot2)
library(patchwork)
#-------------------------------------------------------------------------------
source("R/utils.R")
source("R/plots/utils_plots.R")
source("R/plots/defaults_plots.R")
config <- readYaml("data")
years_train <- seq(config$start_train, config$end_train)
years_test <- seq(config$start_test, config$end_test)

destination_dir <- dir_create(path("assets", "plots", "eda", "aggLonLat"))
gg_save <- ggSaveFactory(destination_dir)
source_files <- dir_ls(path("data", "preprocessed", "tables"), regexp = "long")
#-------------------------------------------------------------------------------
ggHelper1 <- function(dataset, variable_name, lon_or_lat, value = "mean_val", group = "Month") {
  
  checkmate::assert(all(c("mean_val", "Month") %in% colnames(dataset)))
  checkmate::assertChoice(lon_or_lat, c("lon", "lat"))
  checkmate::assert(any(c("lon", "lat") %in% colnames(dataset)))
  
  return(
    ggplot(dataset) +
      geom_point(aes(x = .data[[lon_or_lat]], y = .data[[value]],
                     group = .data[[group]], color = .data[[group]]), shape = 3) +
      ggsci::scale_color_d3(palette = "category20") +
      geom_line(aes(x = .data[[lon_or_lat]], y = .data[[value]],
                    group = .data[[group]], color = .data[[group]]), linewidth = 0.5) +
      labs(x = paste0(lon_or_lat, "(deg)"), y = variable_name)
  )
}
#-------------------------------------------------------------------------------
for (table_path in source_files) {
  filename <- sub("_long", "", getFilename(table_path))
  variable_name <- sub("_.*$", "", getFilename(table_path)) # anything after first _ sign
  cat(paste0("🛠 creating plots for table ", getFilename(table_path), "...\n"))

  dtbl <- fread(table_path)
  
  plt_lon_all <- dtbl[, .(mean_val = mean(value, na.rm = TRUE)), by = .(lon, Month = lubridate::month(date, label = TRUE))] |>
    ggHelper1(variable_name, "lon")
  
  plt_lat_all <- dtbl[, .(mean_val = mean(value, na.rm = TRUE)), by = .(lat, Month = lubridate::month(date, label = TRUE))] |>
    ggHelper1(variable_name, "lat")

  gg_save(paste0(filename, "_aggLon.png"), plt_lon_all)
  gg_save(paste0(filename, "_aggLat.png"), plt_lat_all)
  
  
  plt_lon_test <- dtbl[year(date) %in% years_test, .(mean_val = mean(value, na.rm = TRUE)),
                  by = .(lon, Month = lubridate::month(date, label = TRUE))] |>
    ggHelper1(variable_name, "lon")
  
  plt_lat_test <- dtbl[year(date) %in% years_test, .(mean_val = mean(value, na.rm = TRUE)),
                  by = .(lat, Month = lubridate::month(date, label = TRUE))] |>
    ggHelper1(variable_name, "lat")
  
  gg_save(paste0(filename, "_aggLon_testData.png"), plt_lon_test)
  gg_save(paste0(filename, "_aggLat_testData.png"), plt_lat_test)
  
}
