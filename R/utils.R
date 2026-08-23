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
  } else if (!file_ext(fileanme) %in% c("yaml", "yml")) {
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
cropMainland <- function(dataset) {
  checkmate::assert(inherits(dataset, "SpatRaster") || inherits(dataset, "sf"))
  
  if (inherits(dataset, "sf")) {
    # Cast to POLYGON and keep only the single largest feature by area
    dataset <- dataset |>
      sf::st_cast("POLYGON", warn = FALSE) |>
      dplyr::mutate(tmp_area = sf::st_area(geometry)) |>
      dplyr::slice_max(tmp_area, n = 1, with_ties = FALSE) |>
      dplyr::select(-tmp_area)
    
  } else if (inherits(dataset, "SpatRaster")) {
    # If a custom spatial mask is provided, use it; otherwise fetch California
    if (is.null(mask)) {
      mask <- USAboundaries::us_states(states = "California", resolution = "high")
    }
    
    ca_mask <- mask |>
      sf::st_transform(terra::crs(dataset)) |>
      sf::st_cast("POLYGON", warn = FALSE) |>
      dplyr::mutate(tmp_area = sf::st_area(geometry)) |>
      dplyr::slice_max(tmp_area, n = 1, with_ties = FALSE)
    ca_mask <- vect(ca_mask)
    
    # Mask and crop the SpatRaster to the mainland boundary
    dataset <- terra::crop(dataset, ca_mask)
    dataset <- terra::mask(dataset, ca_mask)
    terra::tmpFiles(current = TRUE, orphan = TRUE, remove = TRUE)
  }
  
  return(dataset) 
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
  if(!is.null(spat_res)){
    ## check rectangular grid and spatial unit
    checkmate::assert(res(raster)[1] == res(raster)[2])
    crs_info <- crs(raster)
    rast_res <- res(raster)[1]
    if (grepl("metre", crs_info,, ignore.case = TRUE)) {
      rast_res_km <- rast_res / 1000
    } else if (grepl("degree", crs_string, ignore.case = TRUE)) {
      rast_res_km <- rast_res * 111 # 40,000km / 360deg ~~ 111.1km
    } else {
      stop("Cannot determine units of CRS ")
    }
    agg_fact <- round(spat_res / rast_res_km) # number of cells in each direction
    rast_dest <- terra::aggregate(raster, fact = agg_fact, fun = func_spat)
  }
  
  ## temporal aggregation
  if(!is.null(tmp_res)) {
    chunk_breaks <- seq(min(source_dates), max(source_dates) + tmp_res,
                       by = paste(tmp_res, "days"))
    chunk_idx <- as.numeric(cut(source_dates, breaks = chunk_breaks))
    rast_dest <- terra::tapp(raster, index = chunk_idx, fun = func_tmp)
    #TODO: names and time
  }
 return(rast_dest)
}