library(fs)
library(tools)
library(here)
library(yaml)
library(dplyr)
library(checkmate)
library(data.table)
library(terra)
library(USAboundaries)
#-------------------------------------------------------------------------------

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
readFile <- function(filename, destination_crs = NULL, filetype = NULL) {
  
  ## read or infer filetype and set read function
  assertCharacter(filename)
  if (is.null(filetype)) {
    filetype <- file_ext(filename)
    checkmate::assertTRUE(filetype != "")
  }
  if (!startsWith(filetype, "."))
    filetype <- paste0(".", filetype)
  read_func <- switch(filetype,
                      ".tif" = terra::rast,
                      ".gpkg" = sf::read_sf,
                      ".csv" = data.table::fread,
                      ".zip" = sf::read_sf,
                      stop(paste0("No file reader implemented for filetype '", filetype, "'\n")))
  
  ## get full path 
  if (dirname(filename) != ".") {
    # directory path is given
    checkmate::assertDirectoryExists(dirname(filename))
    if(file_ext(filename) == "")
      filename <- paste0(filename, filetype)
    checkmate::assertFileExists(filename)
  } else {
  ## only file name at hand, have to find directory
    if (file_ext(filename) == "")
      filename <- paste0(filename, filetype)
    filename <- .findDataset(filename, path(here(), "data"))
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
      res_km <- res_raster/ 1000
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
  if(!is.null(spat_res) & (spat_res_source < spat_res)){
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

subset_raster <- function(raster, date_start, date_end) {
  idx <- which(terra::time(raster) >= date_start & terra::time(raster) <= date_end)
  raster[[idx]]
}

