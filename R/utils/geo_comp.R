library(sf)
library(terra)
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
