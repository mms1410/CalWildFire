library(terra)
library(sf)
library(checkmate)
library(fs)
#-------------------------------------------------------------------------------
#                       operations on raster data
#-------------------------------------------------------------------------------
update_conf <- function(conf, key, value) {
  checkmate::assert(key %in% names(conf))
  conf[[key]] <- value
  return(conf)
}


parsers_rast <- list(
  parser1 = function(names) {
    names |>
      str_extract("\\d{6}") |>
      sub("(\\d{4})(\\d{2})", "\\1-\\2", x = _)
  },
  parser2 = function(names) {
    as.yearmon(names, format = "%Y_%m")
  }
)

parsers_prism <- list(
  parser1 = function(names) {
    names |>
      str_extract("\\d{6}(?=.zip)") |>
      ym()
  }
)

parsers_modis <- list(
  parser1 = function(names) {
    names |>
      str_extract("doy(\\d{7})", group = 1) |>
      (\(x) as.Date(paste0(substr(x, 1, 4), "-01-01")) + as.integer(substr(x, 5, 7)) - 1)()
  }
)




#' Parse a character vector
#'
#' @param strings vector containing strings to be parsed
#' @param parserlist a list of parsers to use in succession
#'
#'
parse_string <- function(strings, parserlist) {
  
  checkmate::assertCharacter(strings)
  checkmate::assertList(parserlist)
  
  # try parser chain
  for (parser_name in names(parserlist)) {
    parser <- parserlist[[parser_name]]
    result <- parser(strings)
  }
  if (!all(is.na(result))) return(result)
  return(rep(NA, length(strings)))
}



read_zip_raster <- function(filepath, pattern = "\\.tif$") {
  
  checkmate::assertFileExists(filepath, extension = "zip")
  data_file <- str_subset(unzip(filepath, list = TRUE)$Name, pattern)
  rast(paste0("/vsizip/", filepath, "/", data_file))
}



#' Calculate aggregated statistic for a goups of rasters
#'
#' For names_split use for example
#'  - split(raster_names, sub('.*_', '', raster_names)) for monthly aggregation
#'  - split(raster_names, sub('_.*', '', raster_names)) for yearly aggregation
#'
#' @param raster (terra-) raster
#' @param names_split names list containing the groups of raster names to use
#' @param func aggregation function defaulting to mean
#'
agg_raster <- function(raster, names_split, func = mean) {
  
  checkmate::assert(inherits(raster, "SpatRaster"))
  checkmate::assertList(names_split)
                    
  agg <- function(group_name) {
    raster_names <- names_split[[group_name]]
    rast_agg <- raster[[raster_names]]
    terra::app(rast_agg, func)
  }
  rast_out <- lapply(names(names_split), agg)
  names(rast_out) <- names(names_split)
  # TODO: handle as.numeric
  rast_out <- rast_out[order(as.numeric(names(rast_out)))]
  rast(rast_out)
}

#-------------------------------------------------------------------------------
#                       other
#-------------------------------------------------------------------------------
split_names <- function(raster_names, keyword = "m") {
  
  pattern <- switch(keyword,
                    "m" = ".*_",
                    "y" = "_.*",
                    NULL)
  
  split(raster_names, sub(pattern, "", raster_names))
}



#' Jitter time or spatial dimension
#'
#' @param dframe 
#' @param keywords
#' @param jitter_density
#'
jitter_frame <- function(dframe, keywords, jitter_density = rnorm) {
  
  checkmate::assertDataFrame(dframe)
  checkmate::assert(all(keywords %in% c("date", "location")))
  
  if ("date" %in% keywords) {
    checkmate::assert("date" %in% colnames(dframe))
    dframe <- dframe |>
      mutate(date = as.POSIXct(jitter(as.numeric(date))))
  }
  
  if ("location" %in% keywords) {
    checkmate::assert(inherits(dframe, "sf") | all(c("lon", "lat") %in% colnames(dframe)))
    n <- nrow(dframe)
    epsilon_x <- jitter_density(n = n)
    epsilon_y <- jitter_density(n = n)
    
    if (inherits(dfrmame, "sf")) {
      checkmate::assertSubset(unique(st_geometry(dframe)), choices = "POINT")
      
      coords <- st_coordinates(dframe)
      x_new <- coords[, "X"] + epsilon_x
      y_new <- coords[, "Y"] + epsilon_y
      
      dframe <- dframe |>
        st_drop_geometry() |>
        mutate(x_new = x_new, y_new = y_new) |>
        st_as_sf(coords = c("x_new", "y_new"), crs = st_crs(dframe))
      
    } else {
      dframe <- dframe |>
        mutate(lon = lon + epsilon_x, lat = lat + epsilon_y)
    }
  }
  return(dframe)
}

#'
#'
#'
#'
agg_tifs <- function(source_dir, destination_dir) {
  prism_variables <- dir_ls(source_dir, type = "directory")
  for (prism_variable in prism_variables) {
    destination <- path(destination_dir, basename(prism_variable))
    years_path <- dir_ls(prism_variable, regexp = ".tif$")
    years_path <- sort(years_path)
    rast_all <- rast(years_path)
    names_new <- parse_rast_names(names(rast_all))
    names(rast_all) <- names_new
    writeRaster(rast_all,
               filename = path(destination_dir,
                               paste0(basename(prism_variable), ".tif"))
               )
 }
}
#'
#'
#'
agg_nlyr_rast <- function(nlyrrast, groups, agg_func = mean) {
  # yearly: '_.*', monthly: '.*_'
  groups <- groups[order(as.numeric(names(groups)))]
  out <- tapp(nlyrrast, groups, fun = agg_func)
  names(out) <- names(groups)
  out
}

#'
#'
#'
query_raster_points <- function(raster, point_frame) {
  
  checkmate::assert(crs(raster) == crs(point_frame))
  terra::extract(raster, vect(point_frame))
  
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