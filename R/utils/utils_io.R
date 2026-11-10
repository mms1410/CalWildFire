library(checkmate)
library(fs)
library(tools)
library(httr2)
library(sf)
library(terra)
library(spdep)
#-------------------------------------------------------------------------------


reduceYamlPath <- function(yaml_path) {
  Reduce(path, yaml_path, init = here())
}


#' Provide adjusted path for zip files containing geodata with shape files etc.
#' 
#' Some datasources are bundled into a zip with shp, proj, etc. Using GDALS
#' virtal file system this can be read without unzipping. This function
#' provides an adjusted path specifier to read final data.
#' 
#'
#' @param zip_path the complete path to zip file
#'
getGeozipPath <- function(zip_path) {
  checkmate::assertFileExists(zip_path)
  content <- unzip(zip_path, list = TRUE)$Name
  shapefile <- content[grepl("\\.shp$", content)]
  checkmate::assertTRUE(length(shapefile) == 1)
  paste0("/vsizip/", zip_path, "/", shapefile)
}


#' # TODO: improve
#' 
#' #' Look for given filename_pattern in cwd.
#' #'
#' #' If multiple matches exists priority will be given to files in 'preprocesses' by default
#' #'
#' #' @param filename_pattern string of pattern to be used in regex for search
#' #' @param source_dir folder which is recursively searched for filename_pattern
#' #' @param priority_dir folder with priority in case of multiple matches
#' #'
#' .searchFile <- function(filename_pattern, source_dir = getwd(), priority_dir = "preprocessed") {
#'   
#'   potential_files <- dir_ls(source_dir, recurse = TRUE, type = "file", regexp = filename_pattern)
#'   if (length(potential_files) > 1) {
#'     idx <- grepl(pattern = priority_dir, potential_files)
#'     if (any(idx)) {
#'       return(potential_files[idx])
#'     }
#'     return(0)
#'   }
#'   if (length(potential_files) == 0) {
#'     message(sprintf("No match for %s ", filename_pattern))
#'     return(-1)
#'   }
#'     
#'   return(potential_files)
#' }



#'
#'
#'
getFileMatches <- function(file_pattern, source_dir = here::here()) {
  return(dir_ls(source_dir, recurse = TRUE, type  = "file", regexp = file_pattern))
}




#'
#'
#'
#'
searchFile <- function(file_pattern, source_dir = here::here(), priority = "preprocessed") {
  
  checkmate::assertCharacter(file_pattern)
  checkmate::assertDirectoryExists(source_dir)
  checkmate::assertCharacter(priority)
  
  matches <- getFileMatches(file_pattern, source_dir)
  
  if (length(matches) == 0) {
    message(sprintf("No match for %s", file_pattern))
    return(character(0))
  }
  if (length(matches) > 1) {
    msg <- sprintf("Multiple files match '%s': \n%s", file_pattern, paste0(matches, collapse = "\n"))
    message(msg)
    prio <- grepl(priority, matches)
    if (sum(prio) == 1) {
      message(sprintf("Picked the (only) one in priority dir '%s'", priority))
      return(matches[prio][1])
    } else if (sum(prio) > 1) {
      message(sprintf("Picked first match in priority dir: %s", matches[prio][1]))
      return(matches[prio][1])
    } else {
      message("Return character(0)")
      return(character(0))
    }
  }
  if (length(matches) == 1) {
    return(matches)
  }
  return(character(0))
}


#'
#'
#'
#'
#'
readFile <- function(filename_pattern, source_dir = here()) {
  
  checkmate::assertCharacter(filename_pattern)

  file_path <- searchFile(filename_pattern)
  checkmate::assert(length(file_path) == 1)
  filetype <- path_ext(file_path)
  switch(filetype,
         "tif"   = return(terra::rast(file_path)),
         "gpkg"  = return(sf::st_set_geometry(sf::read_sf(file_path), "geometry")),
         "csv"   = return(data.table::fread(file_path)),
         "rds"   = return(readRDS(file_path)),
         "adj"  = return(readLines(file_path)),
         stop(paste0("No file reader implemented for type ", filetype, "'\n")))
}


#' Read yaml file into named list
#'
#' @param filename Name of file which can/not be full path and can/not have yaml ending
#' @param source_dir Directory where yaml is located, by default <root>/conf
#'
readYaml <- function(filename = "data", source_dir = NULL) {
  
  if (is.null(source_dir)) source_dir <- path(getwd(), "conf")
  checkmate::assertDirectoryExists(source_dir)
  
  if (tools::file_ext(filename) == "") {
    filename <- paste0(filename, ".yaml")
  } else if (!(tools::file_ext(filename) %in% c("yaml", "yml"))) {
    stop("Can read yaml files only")
  }
  if (dirname(filename) == ".")
    filename <- path(source_dir, filename) 
  checkmate::assertFile(filename)
  
  return(yaml::read_yaml(filename))
}


#' Read a zip file with geodata from remote url
#'
#' Download zip file from url, read specified layer and return it as sf frame.
#'
#' @param url url path to zip file
#' @param crs_sf destination crs of returned sf frame
#' @param layer_name name of layer to select. If NULL first available layer is selected. Default to NULL
#' @param timout_sec timeout for request in seconds. Default to 300
#' @param max_retries max retries if no response previously. Default to 3
#'
readGeoZip <- function(url, crs_sf, layer_name = NULL, timeout_sec = 300, max_retries = 3) {
  
  checkmate::assertCharacter(url)
  checkmate::assertClass(crs_sf, "crs")
  checkmate::assertCharacter(layer_name, null.ok = TRUE)
  checkmate::assertNumeric(timeout_sec)
  checkmate::assertNumeric(max_retries)
  
  tmp <- tempfile(fileext = ".zip")
  
  request(url) |>
    req_timeout(timeout_sec) |> 
    req_retry(max_tries = max_retries) |>
    req_perform(path = tmp)
  zip_path <- paste0("/vsizip/", tmp)
  layers <- st_layers(zip_path)
  
  if (is.null(layer_name)) {
    layer_name <- layers$name[1]  
  } else {
    checkmate::assert(any(layer_name %in% layers$name))
  }
  geodata <- st_read(zip_path, layer = layer_name) |>
    st_transform(crs_sf)
  unlink(tmp)
  return(geodata)
}

setPath <- function(dir_seq, make = TRUE) {
  checkmate::assertCharacter(dir_seq)
  
  p <- Reduce(path, dir_seq, init = here())
  if (make)
    return(dir_create(p))
  
  return(p)
}
#-------------------------------------------------------------------------------
headMessage <- function(to_print) {
  
  checkmate::assertString(to_print)
  
  sep <- strrep("=", 78)
  message(paste(sep, to_print, sep, sep = "\n"))
}

infoMessage <- function(to_print) {
  
  checkmate::assertString(to_print)
  
  malus <- nchar(to_print)
  if (malus >= 78) {
    message(to_print)
    return(NULL)
  }
  nlines <- ceiling((78 - malus - 2) / 2)
  plines <- paste0(rep("=", nlines), collapse = "")
  message(paste0(plines, " ", to_print, " ", plines), collapse = "")
  return(NULL)
}