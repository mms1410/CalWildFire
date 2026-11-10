library(fs)
library(here)
library(checkmate)
library(sf)
library(terra)
library(dplyr)
library(tidyr)
library(data.table)
library(rnaturalearth)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils", "utils_io.R"))
#-------------------------------------------------------------------------------

#' California Albers Projection with units in km
#'
#'
calAlbersKm <- function() {
  return(sf::st_crs("+proj=aea +lat_0=0 +lon_0=-120 +lat_1=34 +lat_2=40.5 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=km +no_defs"))
}


#' Wrapper around st_within for logical indexing
#'
#'
#'
#'
stWithinLogical <- function(x, y) {
  
  checkmate::assert(any(inherits(x, c("sf", "sfc"))))
  checkmate::assert(any(inherits(y, c("sf", "sfc"))))
  
  return(lengths(st_within(x, y)) > 0)
}


#' Add raster values to sf frame of point locations
#'
#' Read coordinates of point geometries and add values from raster.
#' If there is a date column in sf_frame only raster values for specific dates are added.
#'
#' @param sf_frame sf frame with point geometry and optionally a date column
#' @param raster_name the name of raster file that will be loaded
#' @param colname the new columnname of raster values 
#'
addRastToSfPoint <- function(sf_frame, raster_name, colname) {
  
  checkmate::assertClass(sf_frame, "sf")
  checkmate::assert("date" %in% colnames(sf_frame))
  checkmate::assertCharacter(colname)
  checkmate::assert(all(st_geometry_type(sf_frame) == "POINT"))
  
  raster <- readFile(raster_name)
  
  if (st_crs(sf_frame) != st_crs(raster)) {
    query_frame <- st_transform(sf_frame, st_crs(raster))
    values <- queryRaster(query_frame, raster)
  } else {
    values <- queryRaster(sf_frame, raster)
  }
  checkmate::assert(length(values) == nrow(sf_frame))
  return(dplyr::mutate(sf_frame, !!colname := values))
}


#' Query raster values at given locations
#'
#' Read coordinates of point geometries from raster and if date column exists,
#' extract values only for those specific dates.
#' 
#' @param sf_frame sf frame with point geometries
#' @param raster the raster object to be queried
#'
queryRaster <- function(sf_frame, raster) {
  
  checkmate::assertClass(sf_frame, "sf")
  checkmate::assertClass(raster, "SpatRaster")
  checkmate::assert("geometry" %in% colnames(sf_frame))
  
  crs_frame <- st_crs(sf_frame)
  crs_raster <- st_crs(crs(raster))
  
  # cheaper to transform frame than raster
  if (crs_frame != crs_raster) {
    sf_frame <- st_transform(sf_frame, crs_raster)
  }
  
  if ("date" %in% colnames(sf_frame)) {
    checkmate::assertClass(sf_frame$date, "Date")
    raster_dates <- as.Date(names(raster))
    # TODO: fallback time
    idx_layer <- matchRasterDates(query_dates = sf_frame$date, raster_dates)
    checkmate::assert(length(idx_layer) == nrow(sf_frame))
    checkmate::assert(!any(is.na(idx_layer)))
    
    values <- terra::extract(raster, vect(sf_frame), layer = idx_layer, ID = FALSE) |>
      unlist()
  } else {
    values <- terra::extract(raster[[1]], vect(sf_frame$geometry), xy = FALSE, ID = FALSE) |>
      unlist()
  }
  
  checkmate::assert(length(values) == nrow(sf_frame))
  msg <- sprintf("queried %s", terra::sources(raster))
  message(msg)
  
  return(values)
}




addRastToTbl <- function(dtbl, crs_dtbl, raster_name, colname) {
 
  
  if ("date" %in% colnames(dtbl)) {
    sf_frame <- st_as_sf(dtbl[, .(x,y, date)], coords = c("x", "y"), crs = crs_dtbl)
  } else{
    sf_frame <- st_as_sf(dtbl[, .(x,y)], coords = c("x", "y"), crs = crs_dtbl)
  }
  
  raster <- readFile(raster_name)
  values <- queryRaster(sf_frame, raster)
  
  checkmate::assert(length(values) == nrow(dtbl))
  dtbl[, (colname) := values]
  
  return(dtbl)
}


#' Match the raster dates to the nearest query date
#'
#' If there is no exact match of query date in raster dates select the date
#' that is nearest in absolute value.
#' 
#' @param query_dates vector of Dates to be queried
#' @param raster_dates vector of raster dates (by construction the name of layers typically)
#' @param return_idx if TRUE return index in of raster_dates, otw. return date. By default TRUE
#'
matchRasterDates <- function(query_dates, raster_dates, return_idx = TRUE) {
  
  checkmate::assertClass(query_dates, "Date")
  checkmate::assertClass(raster_dates, "Date")
  
  idx <- sapply(query_dates, function(dt) which.min(abs(raster_dates - dt)))
  if (return_idx) {
    return(idx)
  } else {
    return(raster_dates[idx])
  }
}

  


#' return raster resolution in km
#'
#' @param raster SpatRaster
#'
.getResKm <- function(raster) {
  checkmate::assertClass(raster, "SpatRaster")
  
  st_crs <- sf::st_crs(raster)
  crs_units <- st_crs$units
  res_raster <- res(raster)[1]
  if (sf::st_is_longlat(st_crs)) {
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
  if (!is.null(tmp_res)) {
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
    if (is.null(func_tmp))
      func_tmp <- "mean"
  }
  
  ## spatial aggregation
  spat_res_source <- .getResKm(raster)
  if (!is.null(spat_res) && (spat_res_source < spat_res)) {
    agg_fact <- round(spat_res / spat_res_source) # number of cells in each direction
    if (agg_fact == 1) {
      warning(sprintf("Source grid has %s spatial resolution and target resolution is %s yielding aggregation factor of 1,
                      skipped spatial aggregation.", spat_res_source, spat_res))
    } else {
      raster_agg <- terra::aggregate(raster_agg, fact = agg_fact, fun = func_spat, na.rm = TRUE) 
    }
  }
  
  ## temporal aggregation
  tmp_res_source <- as.numeric(min(diff(time(raster))))
  if ((!is.null(tmp_res)) && (tmp_res_source < tmp_res)) {
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



#' Compute nearest euclidean distance between sf points and sf features
#'
#' @param points Spatial Points for distance computation
#' @param feature The feature to which distance is computed
#'
#'
getNearestDistance <- function(points, feature) {
  
  checkmate::assertClass(points, "sfc_POINT")
  checkmate::assertClass(feature, "sf")
  
  nearest_feature <- st_nearest_feature(points, feature)
  
  checkmate::assert(length(nearest_feature) == length(points))
  nearest_distance <- as.numeric(st_distance(points, feature[nearest_feature, ], by_element = TRUE))
  
  return(nearest_distance)
}


#' Compute euclidean distance to roads dataset
#'
#'
getRoadDistance <- function(sf_frame) {
  
  checkmate::assertClass(sf_frame, "sf")
  checkmate::assertClass(st_geometry(sf_frame), "sfc_POINT")
  
  roads <- readFile("roads.gpkg") |>
    st_make_valid()
  
  if (st_crs(roads) != st_crs(sf_frame)) {
    roads <- st_transform(roads, st_crs(sf_frame))
  }
  
  distance <- getNearestDistance(st_geometry(sf_frame), roads)
  message("queried road distance")
  
  return(distance)
}


addRoadDistance <- function(sf_frame) {
  
  checkmate::assertClass(sf_frame, "sf")
  checkmate::assertClass(st_geometry(sf_frame), "sfc_POINT")
  
  distance <- getRoadDistance(sf_frame) / 1000 # km
  return(dplyr::mutate(sf_frame, distance_road = distance))
}


getCoastDistance <- function(sf_frame) {
  
  checkmate::assertClass(sf_frame, "sf")
  checkmate::assertClass(st_geometry(sf_frame), "sfc_POINT")
  
  coastline <- readFile("coastline.gpkg")
  if (st_crs(sf_frame) != st_crs(coastline)) {
    coastline <- st_transform(coastline, st_crs(sf_frame))
  }
  
  distance <- getNearestDistance(st_geometry(sf_frame), coastline)
  message("queried coast distance")
  return(distance)
}

addCoastDistance <- function(sf_frame) {
  
  checkmate::assertClass(sf_frame, "sf")
  checkmate::assertClass(st_geometry(sf_frame), "sfc_POINT")
  
  
  distance <- getCoastDistance(sf_frame) / 1000 # Km
  return(dplyr::mutate(sf_frame, distance_coast = distance ))
}

addCoastDistanceTbl <- function(dtbl, crs_dtbl, colname = "distance_coast") {
  
  checkmate::assertDataTable(dtbl)
  checkmate::assert(all(c("x", "y") %in% colnames(dtbl)))
  
  sf_frame <- st_as_sf(dtbl[, .(x, y)], coords = c("x", "y"), crs = crs_dtbl)
  values <- getCoastDistance(sf_frame)
  
  checkmate::assert(length(values) == nrow(dtbl))
  dtbl[, (colname) := values]
  
  return(dtbl)
}


getEcozRaster <- function() {
  
  ecoz <- readFile("ecoz.gpkg")
  checkmate::assert("region" %in% colnames(ecoz))
  
  rast_tmplt <- rast(vect(ecoz), resolution = 1000)
  return(rasterize(vect(ecoz), rast_tmplt, field = "region"))
}


cropToLargestPoly <- function(sf_frame, n_largest = 1) {
  
  checkmate::assertClass(sf_frame, "sf")
  checkmate::assert("geometry" %in% colnames(sf_frame))
  
  return(sf_frame |>
    st_cast("POLYGON", warn = FALSE) |>
    mutate(area = st_area(geometry)) |>
    slice_max(area, n = n_largest, with_ties = FALSE) |>
    st_set_geometry("geometry") |>
    select(geometry))
}


addEcozToSf <- function(sf_frame, ecoz) {
  
  checkmate::assertClass(sf_frame, "sf")
  
  ecoz <- ecoz |>
    st_transform(st_crs(sf_frame)) 
  
  result <- st_join(sf_frame, ecoz["region"], join = st_intersects, left = TRUE)

  message("joined ecoz to data")
  return(result)
}



addEcozTbl <- function(dtbl, crs_dtbl, ecoz) {
  
  checkmate::assertClass(dtbl, "data.table")
  checkmate::assert(all(c("x", "y") %in% colnames(dtbl)))
  checkmate::assertClass(crs_dtbl, "crs")
  
  sf_frame <- st_as_sf(dtbl[, .(x, y)], coords = c("x", "y"), crs = crs_dtbl)
  sf_frame <- addEcozToSf(sf_frame, ecoz) |>
    filter(!is.na(region))
  
  checkmate::assert(nrow(dtbl) == nrow(sf_frame))
  dtbl[, region := sf_frame$region]
  
  return(dtbl)
}


getMainlandMask <- function() {
  mex <- rnaturalearth::ne_countries(type = "countries", country = "Mexico", scale = 110) |>
    select(geometry) |>
    cropToLargestPoly()
  usa <- rnaturalearth::ne_countries(type = "countries", country = "United States of America", scale = 110) |>
    select(geometry) |>
    cropToLargestPoly()
  
  return(st_union(rbind(mex, usa)))
}