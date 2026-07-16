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
library(stpp)
to_stpp <- function(sf_frame, start_date, time_units = "day") {
  
  checkmate::assert(inherits(sf_frame, "sf"))
  checkmate::assert(all(st_is_valid(sf_frame)))

  
  
  dates <- sf_frame$date
  time <- as.numeric(difftime(dates, start_date, units = time_units))
  
  data <- cbind(st_coordinates(sf_frame), time)
  point_process <- as.3dpoints(data)
  point_process
}
