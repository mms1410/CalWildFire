library(sf)
library(terra)
library(checkmate)
#-------------------------------------------------------------------------------
source("R/utils/data_queries.R")
#-------------------------------------------------------------------------------
#' Calculate aggregated statistic for a goups of rasters
#'
#' @param raster (terra-) raster
#' @param names_split names list containing the groups of raster names to use
#' @param func aggregation function defaulting to mean
#'
agg_raster <- function(raster, names_split, func = mean) {
  
  checkmate::assert(inherits(raster, "SpatRaster"))
  checkmate::assertList(names_split)
  
  aggregates <- lapply(names_split, function(chunk) {
    raster[[chunk]] |>
      app(func)
  })
  rast(aggregates)
}

find_date <- function(query_dates, target_dates) {
  # 1. Convert to Date objects to ensure proper distance calculation
  query_dates  <- as.Date(query_dates)
  target_dates <- as.Date(target_dates)
  
  # findInterval requires sorted targets for binary search
  if (is.unsorted(target_dates)) {
    stop("target_dates must be sorted in ascending order.")
  }
  
  # 2. Find the floor index
  idx <- findInterval(query_dates, target_dates)
  idx[idx == 0] <- 1 
  
  # 3. Check if the next interval index is actually closer
  idx_next <- pmin(idx + 1, length(target_dates))
  dist_curr <- abs(as.numeric(query_dates - target_dates[idx]))
  dist_next <- abs(as.numeric(query_dates - target_dates[idx_next]))
  
  ifelse(dist_next < dist_curr, idx_next, idx)
}


get_point_from_rasternames <- function(sf_points, raster_names) {
  checkmate::assertCharacter(raster_names)
  helper_function <- function(rastername) {
    raster <- read_geodata(keyword = "tif", sf_crs = st_crs(sf_points), filename = rastername)
    get_point_from_raster(sf_points, raster)
  }
  sapply(raster_names, helper_function)
  
}

#'
#'
#'
#'
get_point_from_raster <- function(sf_points, raster) {
  
  checkmate::assert(st_crs(raster) == st_crs(sf_points))
  checkmate::assert(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", names(raster))))
  checkmate::assert("date" %in% colnames(sf_points))
  checkmate::assertTRUE(all(sf::st_geometry_type(sf_points) == "POINT"))
  
  raster_time <- as.Date(time(raster))
  points_time <- as.Date(sf_points$date)
  idx <- find_date(points_time, raster_time)
  values <- extract(raster,
                    vect(select(sf_points, geom)),
                    layer = idx, ID = FALSE, raw = TRUE)

}


#'
#'
#'
#'
get_mainland <- function(sf_frame) {
  
  polys <- st_cast(st_geometry(sf_frame), "POLYGON")
  areas <- st_area(polys)
  mainland <- polys[which.max(areas)]
  st_geometry(sf_frame) <- st_sfc(mainland, crs = st_crs(sf_frame))
  sf_frame
}
