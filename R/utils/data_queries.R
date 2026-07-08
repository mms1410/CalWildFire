library(fs)
library(here)
library(yaml)
library(checkmate)
library(sf)
library(terra)
library(spatstat)
library(tidyverse)
#-------------------------------------------------------------------------------

#' Read configuration file
#'
#' @param filename
#' @param source_dir
#' 
#'
get_conf <- function(filename = "data", source_dir = path(here(), "conf")) {
  file <- path(source_dir, paste0(filename, ".yaml"))
  checkmate::assertFile(file)
  yaml::read_yaml(file)
}


#' Read conf entry
#'
#'
read_conf <- function(conf, key) {
  checkmate::assert(key %in% names(conf))
  conf[[key]]
}

#' Read simple-feature data frame
#'
#' @param keyword Indicates how to read and find a dataset.
#' By default this is 'gpkg' and a filename is expected. For some keywords no filename is
#' expected.
#' @param sf_crs Simple-feature coordinate system to which the dataset is projected.
#' @param filename For keyword 'gpkg' the full path to file.
#' @param source Default folder where 'gpkg' files are located,
#'
read_geodata <- function(keyword = "gpkg", sf_crs = NULL, filename = NULL, source = path(here(), "data", "preprocessed")) {
   #TODO: just ecoz
  if(!is.null(sf_crs)) checkmate::assertTRUE(inherits(sf_crs, "crs"))
  allowed_keywords <- c("gpkg", "tif", "ca_state", "ecoz4", "ecoz3", "road_network")
  checkmate::assertChoice(keyword, allowed_keywords)
  
  # keyword needs filename
  if (keyword == "gpkg" | keyword == "tif"){
    checkmate::assert(!is.null(filename))
    checkmate::assertString(filename)
  
  files_match <- dir_ls(source, recurse = TRUE, glob = paste0("*.", keyword))
  if (is.empty(files_match)) stop(past0("No files of type ", keyword, " in ", source))
  filename <- keep(files_match, ~str_detect(.x, filename))
  } else {
    filename <- switch(keyword, 
                       "ecoz3" = paste0("/vsizip/",path(here(), "assets", "ca_ecoz3.zip")),
                       "ecoz4" = paste0("/vsizip/",path(here(), "assets", "ca_ecoz4.zip")),
                       "ca_state" = paste0("/vsizip/",path(here(), "assets", "ca_state.zip")),
                       "road_network" = paste0(path(here(), "data", "raw", "caltrans.gpkg"))
                       )
  }
  checkmate::assertFileExists(sub("/vsizip/", "", filename)) # virtual file system
  
  
  if (keyword == "tif") {
    out <- rast(filename)
    if(st_crs(out) != sf_crs) {
      out <- project(out, sf_crs$wkt)
    }
  } else {
    out <- st_read(filename, quiet = TRUE) |>
      (\(dframe) if (is.null(sf_crs)) st_drop_geometry(dframe) else st_transform(dframe, crs = sf_crs))()
  }
  out
}

#' Read raster data
#'
#' @param filename tif filename.
#' @param sf_crs Simple-feature coordinate system to which the dataset is projected.
#' @param source Default directory where raster data is located.
#'
read_tif <- function(filename, sf_crs, source = path(here(), "data", "preprocessed")) {
  
  checkmate::assertString(filename)
  checkmate::assertClass(sf_crs, "crs")
  checkmate::assertDirectory(source)
  
  if (!str_detect(filename, regex("\\.tif$", ignore_case = TRUE))) {
    filename <- paste0(filename, ".tif")
  }
  checkmate::assertFileExists(path(source, filename))
  
  rast(path(source, filename)) |>
    project(sf_crs$wkt)
}

#' Get the dataframe representation of a tif (raster) file.
#'
#' @param name name of data file
#' @param source Directory where data is stored. Defaults to root/data/preprocessed
#'
read_rast_frame <- function(filename, suffix  = "long", source = path(here(), "data", "preprocessed")) {
  # TODO: check long lat deg
  checkmate::assertChoice(suffix, c("long", "wide"))
  checkmate::assertString(filename)
  checkmate::assertDirectory(source)
  filename <- paste0(filename, "_", suffix)
  
  files <- dir_ls(source)
  idx <- str_detect(basename(files), filename)
  checkmate::assert(sum(idx) == 1)
  file <- files[idx]
  
  data.table::fread(file)
}
#-------------------------------------------------------------------------------
add_layer <- function(data, to_add, sf_crs = NULL) {
  
  checkmate::assert(inherits(to_add, "sf"))
  if (inherits(data, "sf")){
    checkmate::assert(length(unique(st_geometry_type(data))) == 1)
   result <- st_join(data, to_add, join = st_within)
  } else if (inherits(data, "data.frame")) {
    checkmate::assertNamed(data, type = "named", c("lon", "lat"))
    checkmate::assert(inherits(sf_crs, "crs"))
    # TODO: check
    result <- data |> st_as_sf(coords = c("lon", "lat"), crs = sf_crs) |>
      st_join(to_add, join = st_within)
  } else {
    stop("Dont know how to handle data")
  }
  return(result)
}

#-------------------------------------------------------------------------------
get_timeseries <- function(dframe, resolution = "m") {
  
  checkmate::assert(inherits(dframe, "data.frame"))
  checkmate::assert("date" %in% colnames(dframe))
  checkmate::assertChoice(resolution, c("m", "d"))
  
  date_handler <- function(dates, resolution) {
    out <- switch(resolution,
           "m" = tsibble::yearmonth(dates),
           "d" = as.Date(dates),
           NULL)
    if (is.null(out) | all(is.na(out))) stop(paste0("Failed to transform date to resolution ", resolution))
    out
  }
  
  dframe |>
    transmute(date = date_handler(date, resolution)) |>
    group_by(date) |>
    summarize(count = n()) |>
    as_tsibble(index = date) |>
    fill_gaps(count = 0L)
}


#-------------------------------------------------------------------------------
query_geom_raster_points <- function(sf_frame, raster) {
  
  
  checkmate::assert(st_crs(sf_frame)$wkt == crs(raster))
  checkmate::assert("date" %in% colnames(sf_frame))
  checkmate::assert("geom" %in% colnames(sf_frame))
  
  names(raster) <- str_extract(names(raster), "\\d{6}")
  
  query_raster <- function(yym, dt) {
    if (!yym %in% names(raster)) {
      values <- rep(NA, nrow(dt))
    } else {
      r <- raster[[yym]]
      values <- terra::extract(r, vect(dt, geom = c("x", "y")))
      values <- values[,2]
    }
    dt |>
      mutate(value = values)
  }
  
  dates_lags <- function(date) {
    
  }
  
  query <- sf_frame |>
    transmute(yym = format(date, "%Y%m"),
              x = st_coordinates(geom)[, 1],
              y = st_coordinates(geom)[, 2]) |>
    st_drop_geometry() |>
    nest(.by = yym)
  
  query |>
    rowwise() |>
    transmute(result = list(query_raster(yym, data))) |>
    unnest(cols = c(result)) |>
    st_as_sf(coords = c("x", "y"), crs = crs(sf_frame))
    
  
}