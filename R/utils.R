suppressPackageStartupMessages({
library(httr2)
library(fs)
library(tools)
library(here)
library(yaml)
library(dplyr)
library(checkmate)
library(data.table)
library(terra)
library(sf)
library(esri2sf)
library(USAboundaries)
})
#-------------------------------------------------------------------------------

subset_train_test <- function(geodata, train = TRUE) {
  config_data <- readYaml("data")
  if (train) {
    time_start <- config_data$start_train
    time_end <- config_data$end_train
  } else {
    time_start <- config_data$start_test
    time_end <- config_data$end_test
  }
  if (inherits(geodata, "sf")) {
   result <- geodata |>
     filter(year(date) >= time_start & year(date) <= time_end)
  } else if (inherits(geodata, "SpatRaster")) {
    idx <- (year(terra::time(geodata)) >= time_start) & (year(terra::time(geodata)) <= time_end)
    result <- geodata[[idx]]
  } else {
    stop(paste0("unkown geodata type", class(geodata)))
  }
  return(result)
}

#' Provide adjusted path for zip files containing geodata
#' 
#' Some datasources are bundled into a zip with shp, proj, etc. Using GDALS
#' virtal file system this can be read without unzipping. This function
#' provides an adjusted path specifier to read final data.
#' 
#'
#' @param zip_path the complete path to zip file
#'
.getGeozipPath <- function(zip_path) {
  checkmate::assertFileExists(zip_path)
  content <- unzip(zip_path, list = TRUE)$Name
  shapefile <- content[grepl("\\.shp$", content)]
  checkmate::assertTRUE(length(shapefile) == 1)
  paste0("/vsizip/", zip_path, "/", shapefile)
}



#' Find a matching full path of a filename in a given directory
#'
#' @param filename String with filename that is searched for
#' @param base_dir The directory where the given filename will be searched for
#'
.findDataset <- function(filename, base_dir) {

  checkmate::assertCharacter(filename)
  checkmate::assertCharacter(base_dir)
  checkmate::assertDirectoryExists(base_dir)
  
  all_files <- dir_ls(base_dir, recurse = TRUE, type = "file")
  all_files  <- all_files[file_ext(all_files) == file_ext(filename)]
  idx_matches <- basename(all_files) == filename
  if (sum(idx_matches) == 1) {
    return(all_files[idx_matches])
  } else if (sum(idx_matches) > 1) {
    msg <- paste0("Ambigious potential matches for filename ", filename, "':\n", all_files[idx_matches])
    stop(msg)
  } else {
    msg <- paste0("Did not find any match for filename '", filename, "' in ", base_dir)
    stop(msg)
  }
}



searchFile <- function(filename_pattern, source_dir = path("data"), priority_dir = "preprocessed") {
  
  
  
  potential_files <- dir_ls(source_dir, recurse = TRUE, type = "file", regexp = filename_pattern)
  if (length(potential_files) > 1) {
    idx <- grepl(pattern = priority_dir, potential_files)
    if (any(idx)) {
      return(potential_files[idx])
    }
    stop(paste0("Multiple matches for '", filename_pattern, "'\n"))
  }
  if (length(potential_files) == 0)
    stop(paste0("No match for '", filename_pattern, "'\n"))
  return(potential_files)
}


readFile <- function(filename_pattern) {
  
  file_path <- searchFile(filename_pattern)
  filetype <- path_ext(file_path)
  switch(filetype,
         "tif" = return(terra::rast(file_path)),
         "gpkg" = return(sf::read_sf(file_path)),
         "csv" = return(data.table::fread(file_path)),
         stop(paste0("No file reader implemented for type ", filetype, "'\n")))
}




#' Read a single file of variable format
#'
#' User can provide the full name including path. If no path is included
#' the only matching file in data directory is used.
#'
#' @param filename String of filename
#' @param filetype Optional seperate specification of filetype
#' 
#' @examples
#' precipitation <- read_file('ppt.tif') # OK
#' precipitation <- read_file('ppt') # ERROR
#' precipitation <- read_file('ppt', 'tif') # OK
#' precipitation <- read_file('ppt', '.tif') # OK
#'
readFileLegacy <- function(filename, destination_crs = NULL, filetype = NULL) {
  
  ## read or infer filetype and set read function
  checkmate::assertCharacter(filename)
  
  if (is.null(filetype)) {
    filetype <- file_ext(filename)
    checkmate::assertTRUE(filetype != "")
  }
  if (!startsWith(filetype, "."))
    filetype <- paste0(".", filetype)
  
  
  ## get full path 
  if (dirname(filename) != ".") {
    # directory path is given
    checkmate::assertDirectoryExists(dirname(filename))
    if (file_ext(filename) == "")
      filename <- paste0(filename, filetype)
    checkmate::assertFileExists(filename)
  } else {
  ## only file name at hand, have to find directory
  ## default preprocessed
    if (file_ext(filename) == "")
      filename <- paste0(filename, filetype)
    filename <- .findDataset(filename, path("data", "preprocessed"))
  }
  ## handle zip and virtual file system
  if (filetype == ".zip") {
    filename <- .getGeozipPath(filename)
  }
  
  geodata <- do.call(read_func, list(filename))
  ## handle coordinate reference system
  if (!is.null(destination_crs)) {
    checkmate::assertMultiClass(destination_crs, classes = c("numeric", "crs"))
    if (inherits(destination_crs, "numeric"))
      destination_crs <- st_crs(destination_crs)
    if (st_crs(geodata) != destination_crs)
      geodata <- switch(filetype,
                        ".tif" = terra::project(geodata, destination_crs),
                        ".gpkg" = sf::st_transform(geodata, destination_crs),
                        ".zip" = sf::st_transform(geodata, destination_crs))
  }
  return(geodata)
}


#' Read yaml file into named list
#'
#' @param filename Name of file which can/not be full path and can/not have yaml ending
#' @param source_dir Directory where yaml is located, by default <root>/conf
#'
readYaml <- function(filename = "data", source_dir = NULL) {
  
  if (is.null(source_dir)) source_dir <- path(here(), "conf")
  checkmate::assertDirectoryExists(source_dir)
  
  if (file_ext(filename) == "") {
    filename <- paste0(filename, ".yaml")
  } else if (!(file_ext(filename) %in% c("yaml", "yml"))) {
     stop("Can read yaml files only")
  }
  if (dirname(filename) == ".")
    filename <- path(source_dir, filename) 
  checkmate::assertFile(filename)
  
  return(yaml::read_yaml(filename))
}

dumpYaml <- function(yaml_file, destintation) {
  checkmate::assertDirectoryExists(dirname(destination_dir))
  checkmate::assertClass(yaml_file, "list")
  yaml::write_yaml(yaml_file, destintation)
}

#' Return largest polygon of geomframe
#'
#' @param sf_frame spatial data frame
#' @aparam n nth largest polygon to maintain (defaults to one)
#'
cropMainland <- function(geodata, mask_mainland, destination) {
  checkmate::assert(inherits(geodata, "SpatRaster") || inherits(geodata, "sf"))
  checkmate::assert(inherits(mask_mainland, "sf"))
  
  mask_mainland <- st_transform(mask_mainland, crs(geodata))
  
  if (inherits(geodata, "sf")) {
    # select largest polygon
    geodata <- st_make_valid(geodata)
    geodata <- st_intersection(geodata, mask_mainland)
  } else if (inherits(geodata, "SpatRaster")) {
    mask_mainland <- vect(mask_mainland)
    geodata <- terra::crop(geodata, mask_mainland)
    geodata <- terra::mask(geodata, mask_mainland)
  }
  return(geodata) 
}


getResKm <- function(raster) {
  checkmate::assertClass(raster, "SpatRaster")
  
  st_crs <- sf::st_crs(raster)
  crs_units <- st_crs$units
  res_raster <- res(raster)[1]
  if (sf::st_is_longlat(st_crs)) {
    ## Geographic (degrees) - convert to km
    ## 1 degree ≈ 111.32 km
    res_km <- res_raster * 111.32
  } else{
    if (crs_units == "metre" || crs_units == "m") {
      ## m to km
      res_km <- res_raster / 1000
    } else {
      stop(paste0("Unknown/not implemented crs units ", crs_units))
    }
  }
  return(res_km)
}


#' Downsample a raster in spatial or temporal dimension 
#'
#' @param raster Raster (SpatRaster)
#' @param tmp_res temporal resolution (unit in days)
#' @param spat_res spatial resolution (unit in km)
#' @param func_tmp aggregation function applied to rasters between consecutive dates to maintain . If NULL
#' no aggregation is performed (intermediate rasters are dropped).
#' @param func_spat aggregation function applied to pixels within spat_res. Defaults to mean when a spatial resolution
#' is given.
#'
aggSpatTemp <- function(raster, tmp_res = NULL, spat_res = NULL, func_tmp = NULL, func_spat = NULL) {
  
  ## checks
  checkmate::assertClass(raster, "SpatRaster", null.ok = TRUE)
  checkmate::assertIntegerish(tmp_res, null.ok = TRUE)
  checkmate::assertNumeric(spat_res, null.ok = TRUE)
  checkmate::assertFunction(func_tmp, null.ok = TRUE)
  checkmate::assertFunction(func_spat, null.ok = TRUE)
  
  raster_agg <- raster
  if (!is.null(tmp_res)){
    checkmate::assert(has.time(raster))
    source_dates <- time(raster)
    source_dates <- sort(source_dates)
    checkmate::assertDate(source_dates)
  }
  
  ## defaults
  if (!is.null(spat_res)) {
    if (is.null(func_spat))
      func_spat <- "mean"
  }
  if (!is.null(tmp_res)) {
    if(is.null(func_tmp))
      func_tmp <- "mean"
  }
  
  ## spatial aggregation
  spat_res_source <- getResKm(raster)
  if(!is.null(spat_res) && (spat_res_source < spat_res)){
    agg_fact <- round(spat_res / spat_res_source) # number of cells in each direction
    raster_agg <- terra::aggregate(raster_agg, fact = agg_fact, fun = func_spat)
  }
  
  ## temporal aggregation
  tmp_res_source <- as.numeric(min(diff(time(raster))))
  if((!is.null(tmp_res)) && (tmp_res_source < tmp_res)) {
    chunk_breaks <- seq(min(source_dates), max(source_dates) + tmp_res,
                       by = paste(tmp_res, "days"))
    chunk_idx <- as.numeric(cut(source_dates, breaks = chunk_breaks))
    raster_agg <- terra::tapp(raster_agg, index = chunk_idx, fun = func_tmp)
    
    dates_agg <- seq(min(source_dates), by = paste(tmp_res, "days"), 
                     length.out = terra::nlyr(raster_agg))
    terra::time(raster_agg) <- dates_agg
    names(raster_agg) <- dates_agg
  }
 return(raster_agg)
}


get_filename <- function(complete_path) {
  return(fs::path_ext_remove(basename(complete_path)))
}

replaceDefault <- function(config, var_name) {
  
  if (!(var_name %in% names(config))) {
    return(config$default)
  }
  result <- config$default
  new <- config[[var_name]]
  names_intersect <- intersect(names(result), names(new))
  new_names <- setdiff(names(new), names(result))
  new_values <- new[new_names]
  result[names_intersect] <- new[names_intersect]
  result <- append(result, new_values)
  return(result)
}



getGeoZip <- function(url, crs_sf, layer_name = NULL, timeout_sec = 300, max_retries = 3) {
  
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



#'
#'
#'
#' > args <- c("control.inla==list(strategy='laplace',int.strategy='eb')", "control.bla==list(foo='buzz')")
#' > parseArgs(args)
#' > $control.inla
#' > $control.inla$strategy
#' > [1] "laplace"
#'
#' > $control.inla$int.strategy
#' > [1] "eb"
#'
#'
#' > $control.bla
#' > $control.bla$foo
#' > [1] "buzz"
#'
parseArgs <- function(args) {
  options <- list()
  #browser()
  for (arg in args) {
    parts <- strsplit(arg, "==", fixed = TRUE)[[1]]
    if (length(parts) == 2) {
      arg_name <- parts[1]
      arg <- parts[2]
      parsed_arg <- tryCatch({
        eval(parse(text = arg))
      },
      error = function(e)
        stop(paste0("Error when parsing '", arg, "\n")))
      options[[arg_name]] <- parsed_arg
    } else {
      stop("Expected arguments to be '==' seperated tuple")
    }
  }
  return(options)
}
