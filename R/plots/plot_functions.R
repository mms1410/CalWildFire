library(ggplot2)
library(cowplot)
library(gifski)
library(tidyverse)
library(tidyterra)
library(yaml)
library(fs)
library(lubridate)
library(sf)
library(ggsci)
library(magick)
#-------------------------------------------------------------------------------
theme_set(theme_light())
theme_ecoregion <- theme(
  legend.position = "bottom",
  legend.text = element_text(size = 5),
  legend.title = element_text(size = 6),
  legend.key.size = unit(0.2, "cm")
)
options(
  # Discrete scales
  ggplot2.discrete.colour = function(...)
    ggsci::scale_color_d3(palette = "category20", ...),
  
  ggplot2.discrete.fill = function(...)
    ggsci::scale_fill_d3(palette = "category20", ...),
  
  # Continuous scales
  ggplot2.continuous.colour = function(...)
    viridis::scale_color_viridis(...),
  
  ggplot2.continuous.fill = function(...)
    viridis::scale_fill_viridis(...)
)

scale_fill <- function(...) scale_fill_viridis_c(...)
scale_colour <- function(...) scale_colour_viridis_c(...)
month_colors <- function() {
  ggsci::scale_color_d3(palette = "category20")
}
#-------------------------------------------------------------------------------
#' Save last plot as png
#'
#' @param filename name of png file (.png will be added)
#' @param desintation_dir location where png image will be stored
#'
gg_save <- function(filename, plt = get_last_plot(), destination_dir = path(here(), "assets", "plots"), set_theme = TRUE, trim = TRUE, dpi = 300, width = 8, height = 8) {
  
  dir_create(destination_dir)
  #p <- last_plot()
  if (set_theme) {
    plt <- plt +
      theme(aspect.ratio = 1,
            plot.margin = margin(0, 0, 0, 0, "pt"),
            legend.margin = margin(0, 0, 0, 0, "pt"),
            legend.box.margin = margin(0, 0, 0, 0, "pt"))
  }
  
  out_path <- path(destination_dir, paste0(filename, ".png"))
  
  ggsave(out_path,
         plot = plt,
         dpi = dpi,
         width = width,
         height = height,
         units = "in")
  
  if (trim) {
    img <- image_read(out_path)
    img <- image_trim(img)
    image_write(img, out_path)
  }
}

png_save <- function(plot_expr,
                     filename,
                     destination_dir = path(here(), "assets", "plots"),
                     ...) {
  
  dir.create(
    destination_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  png(
    filename = path(destination_dir, paste0(filename, ".png")),
    ...
  )
  
  on.exit(dev.off(), add = TRUE)
  
  eval.parent(substitute(plot_expr))
}
#-------------------------------------------------------------------------------


#'
#' @tif_name
#' @sf_crs
#' @dir_dest
#' @legend_title
#'
plot_rast_agg <- function(tif_name, sf_crs, dir_dest, legend_title = NULL){
  
  if(is.null(legend_title)) {
    legend_title <- tif_name
  }
  raster <- read_geodata(keyword = "tif", sf_crs = sf_crs, filename = tif_name)
  
  plt_patch_y <- ggplot() +
    geom_spatraster(data = agg_raster(raster, 
                                      split(names(raster), lubridate::year(names(raster))),
                                      func = "median")) +
    raster_theme(legend_title, facet = TRUE)
  filename_y <- paste0("patch_", tif_name, "_y")
  gg_save(filename_y, plt = plt_patch_y, destination_dir = dir_dest)
  
  plt_patch_m <- ggplot() +
    geom_spatraster(data = agg_raster(raster, split(names(raster), lubridate::month(names(raster), label = TRUE)))) +
    raster_theme(legend_title, facet = TRUE)
  filename_m <- paste0("patch_", tif_name, "_m")
  gg_save(filename_m, plt = plt_patch_m,  destination_dir = dir_dest)
}




#'
#' @frame
#' @lonlat
#' @ylab
#'
#'
plot_lonlat <- function(frame, lonlat = "lon", ylab = NULL) {
  
  checkmate::assertDataFrame(frame)
  checkmate::assertChoice(lonlat, c("lon", "lat"))
  checkmate::assert(all(c("lon", "lat", "date", "value") %in% colnames(frame)))
  
  xlab <- ifelse(lonlat == "lon", "Longitude(deg)", "Latitude(deg)")
  if (is.null(ylab)) {
    ylab <- deparse(substitute(frame))
  }
  single_date <- length(unique(frame$date)) == 1

  if (!single_date) {
    plt <- frame |>
      mutate(year = year(date),
              month = factor(lubridate::month(date, label = TRUE, abbr = FALSE), levels = month.name)) |>
      group_by(.data[[lonlat]], month) |>
      summarize(value = mean(value, na.rm = TRUE), .groups = "drop") |>
      ggplot() +
      geom_point(aes(x = .data[[lonlat]], y = value,
                      group = month, color = month), size = 0.5, shape = 3) +
      month_colors() +
      stat_summary(aes(x = .data[[lonlat]], y = value, group = 1),
                    fun = mean, geom = "line", color = "black", linewidth = 0.8) +
      labs(x= xlab, y = ylab, color = "Month")
  } else {
    plt <- frame |>
      group_by(.data[[lonlat]]) |>
      summarize(value = mean(value, na.rm = TRUE), .groups = "drop")|>
      ggplot() +
      geom_line(aes(x = .data[[lonlat]], y = value), color = "black", linewidth = 0.8) +
      labs(x = xlab, y = ylab)
  }
  plt
}

#'
#'
#'
#'
#'
rastlyr_to_pngs <- function(raster, variable_name, destination_dir, legend_name = "Value", show_legend = TRUE, limits = NULL, levels = NULL) {
  
  for (lyr_name in names(raster)) {
    
      r <- raster[[lyr_name]]
      
      title <- paste0(variable_name, ": ", names(r))
      guide <- ifelse(show_legend, "colorbar", "none")
      filename <- paste0(variable_name, "_", lyr_name)
      
      p <- ggplot() +
        geom_spatraster(data = r)
      if (is.null(levels)) {
        p <- p +
          scale_fill_viridis_c(
            na.value = NA,
            name = legend_name,
            guide = guide,
            limits = limits
          )
      } else {
        p <- p +
          scale_fill_discrete(labels = levels)
      }
      p <- p +
        theme_minimal() +
        ggtitle(title)
      gg_save(filename = filename, plt = p, destination_dir = destination_dir)
  }
}

png_bins_to_gif <- function(bin_list, destination_dir, filenames = NULL) {
  
  if(!is.null(filenames)) {
    checkmate::assert(length(filenames) == length(names(bin_list)))
  } else {
    filenames <-paste0(names(bin_list), ".gif")
  }
  filenames <- as.list(setNames(filenames, names(bin_list)))
  for (element in names(bin_list)) {
    pngs <- bin_list[[element]]
    filename <- filenames[[element]]
    gifski(pngs,
           path(destination_dir, filename),
           delay = 0.5,   # seconds per frame
           width = 800,
           height = 600)
  }
}

raster_theme <- function(name = "", guide = "colorbar", categorical = FALSE, facet = FALSE) {
  # Determine color scale
  thm <- if (categorical) {
    scale_fill_d3(palette = "category20", na.value = "transparent", na.translate = FALSE, name = name)
  } else {
    scale_fill_viridis_c(na.value = NA, name = name, guide = guide)
  }
  
  # Base components
  components <- list(
    thm,
    theme_minimal(),
    labs(x = "Longitude", y = "Latitude")
  )
  
  # Append faceting and specific axis adjustments if requested
  if (facet) {
    components <- c(list(facet_wrap(~lyr)), components)
    components <- c(components, list(theme(axis.text.x = element_text(angle = 45, hjust = 1))))
  }
  components
}


plot_and_save_raster <- function(raster, filename, dir_destination) {
  
  plt <- ggplot() +
    geom_spatraster(data = raster) +
    scale_fill_viridis_c(na.value = NA) +
    facet_wrap(~lyr) +
    theme(axis.text = element_blank(), axis.ticks = element_blank()) + 
    labs(fill = "")
  plt
  gg_save(filename, destination_dir = dir_destination)
  plt
}


plot_and_save_frame <- function(frame_name, destination_dir, variable_name = NULL, filename = NULL) {
  
  if (is.null(variable_name)) variable_name <- frame_name
  if (is.null(filename)) filename <- variable_name
  
  rast_frame <- read_rast_frame(frame_name)
  single_date <- length(unique(rast_frame$date)) == 1

  p1 <- plot_lonlat(rast_frame, ylab = variable_name)
  p2 <- plot_lonlat(rast_frame, ylab = variable_name, lonlat = "lat")
  p1_y_range <- ggplot_build(p1)$layout$panel_params[[1]]$y$get_limits()
  p2_y_range <- ggplot_build(p2)$layout$panel_params[[1]]$y$get_limits()
  y_range_max <- max(p1_y_range[[2]], p2_y_range[[2]])
  y_range_min <- min(p1_y_range[[1]], p2_y_range[[1]])
  y_range <- c(y_range_min, y_range_max)
  p1 <- p1 + ylim(y_range)
  p2 <- p2 + ylim(y_range)
  
  
  gg_save(paste0("lon_", filename, "_sphagetti"), plt = p1, destination_dir = destination_dir)
  gg_save(paste0("lat_", filename, "_sphagetti"), plt = p2, destination_dir = destination_dir)

  q <- cowplot::plot_grid(p1, p2, nrow = 1, ncol = 2)
  gg_save(paste0("patch_lonlat_", filename, "_sphagetti"), plt = q, destination_dir = destination_dir)

  if (!single_date) {
    p3 <- rast_frame |>
      mutate(date = floor_date(date, "month")) |>
      group_by(date) |>
      summarize(
        mean = mean(value),
        median = median(value),
        max = max(value),
        min = min(value),
        .groups = "drop"
      ) |>
      ggplot(aes(x = date)) +
      geom_line(aes(y = mean, color = "mean")) +
      geom_line(aes(y = median, color = "median")) +
      geom_line(aes(y = max, color = "max")) +
      geom_line(aes(y = min, color = "min")) +
      labs(y = variable_name, x = "time", color = "")
    gg_save(paste0("ts_", filename), plt = p3, destination_dir = destination_dir)
    return(c(p1, p2, p3))
  }
  return(c(p1, p2))
}


set_raster_level_lc <- function(raster, type = "1") {
  
  levels_frame <- levels(raster)[[1]]
  checkmate::assert("ID" %in% colnames(levels_frame))
  
  lookup_table <- get_lc_lookup(type = type)
  to_set <- levels_frame |> 
    select(ID) |>
    left_join(lookup_table, by = c("ID" = "value")) |>
    rename(value = ID)
  levels(raster) <- to_set
  raster
}


get_lc_lookup <- function(type = "1") {
  
  checkmate::assertCharacter(type)
  
  # LC_Type1 — IGBP classification, range [1,17]
  lc_type1 <- data.frame(
    value = 1:17,
    class = c(
      "Evergreen Needleleaf Forests",
      "Evergreen Broadleaf Forests",
      "Deciduous Needleleaf Forests",
      "Deciduous Broadleaf Forests",
      "Mixed Forests",
      "Closed Shrublands",
      "Open Shrublands",
      "Woody Savannas",
      "Savannas",
      "Grasslands",
      "Permanent Wetlands",
      "Croplands",
      "Urban and Built-up Lands",
      "Cropland/Natural Vegetation Mosaics",
      "Permanent Snow and Ice",
      "Barren",
      "Water Bodies"))
  
  # LC_Type2 — University of Maryland (UMD) classification, range [0,15]
  lc_type2 <- data.frame(
    value = 0:15,
    class = c(
      "Water Bodies",
      "Evergreen Needleleaf Forests",
      "Evergreen Broadleaf Forests",
      "Deciduous Needleleaf Forests",
      "Deciduous Broadleaf Forests",
      "Mixed Forests",
      "Closed Shrublands",
      "Open Shrublands",
      "Woody Savannas",
      "Savannas",
      "Grasslands",
      "Croplands",
      "Urban and Built-up Lands",
      "Cropland/Natural Vegetation Mosaics",
      "Non-Vegetated Lands",
      "Unclassified"))
  
  # LC_Type3 — LAI/fPAR classification (Myneni et al.), range [0,10]
  lc_type3 <- data.frame(
    value = 0:10,
    class = c(
      "Water Bodies",
      "Grasslands",
      "Shrublands",
      "Broadleaf Croplands",
      "Savannas",
      "Evergreen Broadleaf Forests",
      "Deciduous Broadleaf Forests",
      "Evergreen Needleleaf Forests",
      "Deciduous Needleleaf Forests",
      "Non-Vegetated Lands",
      "Urban and Built-up Lands"))
  
  # LC_Type4 — BIOME-BGC classification (Running et al.), range [0,8]
  lc_type4 <- data.frame(
    value = 0:8,
    class = c(
      "Water Bodies",
      "Evergreen Needleleaf Vegetation",
      "Evergreen Broadleaf Vegetation",
      "Deciduous Needleleaf Vegetation",
      "Deciduous Broadleaf Vegetation",
      "Annual Broadleaf Vegetation",
      "Annual Grass Vegetation",
      "Non-Vegetated Land",
      "Urban and Built-up Lands"))
  
  # LC_Type5 — Plant Functional Type classification (Bonan et al.), range [0,11]
  lc_type5 <- data.frame(
    value = 0:11,
    class = c(
      "Water Bodies",
      "Evergreen Needleleaf Trees",
      "Evergreen Broadleaf Trees",
      "Deciduous Needleleaf Trees",
      "Deciduous Broadleaf Trees",
      "Shrub",
      "Grass",
      "Cereal Croplands",
      "Broadleaf Croplands",
      "Urban and Built-up Lands",
      "Permanent Snow and Ice",
      "Non-Vegetated Lands"))
  
  switch(type,
         "1" = lc_type1,
         "2" = lc_type2,
         "3" = lc_type3,
         "4" = lc_type4,
         "5" = lc_type5)
}