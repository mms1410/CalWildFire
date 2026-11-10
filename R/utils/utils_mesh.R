library(fmesher)
library(sf)
library(checkmate)
#-------------------------------------------------------------------------------
integrationMeshLookup <- function(name_mesh_spat, name_mesh_temp) {
  
  mesh_spat <- readFile(name_mesh_spat)
  mesh_temp <- readFile(name_mesh_temp)
  
  
}

meshInfoPriorRange <- function(spatial_mesh) {
  
  checkmate::assertClass(spatial_mesh, "fm_mesh_2d")
  
  el <- getMeshEdgeLengths(spatial_mesh)
  
  info <- c(min(el), mean(el), median(el), max(el))
  
  info_string <- paste(c("min", "mean", "median", "max"), round(info, 2), collapse = "\n", sep = ": ")
  
  lower <- round(info, 2) * 3
  upper <- round(info, 2) * 5
  range_recommended <- paste(c("min", "mean", "median", "max"), paste(lower, "--", upper, sep = ""), sep = ": ", collapse = "\n")
  msg <- paste0("Suggested prior Range should be at least 3-5 times as large as 'typical edge length'. Edge info(in mesh units):\n", info_string,
                "\nThe recommended range should be >= than:\n", range_recommended)
  message(msg)
}



getMeshEdgeLengths <- function(mesh_spatial) {
  
  triangles <- mesh_spatial$graph$tv
  edges <- rbind(triangles[, c(1, 2)],
                 triangles[, c(2, 3)],
                 triangles[, c(3, 1)])
  
  p1 <- mesh_spatial$loc[edges[, 1], 1:2]
  p2 <- mesh_spatial$loc[edges[, 2], 1:2]
  edge_lengths <- sqrt(rowSums((p1 - p2)^2))
  
  return(edge_lengths)
}


getMeshNodes <- function(spat_mesh) {

  checkmate::assertClass(spat_mesh, "fm_mesh_2d")

  locs <- as.data.frame(spat_mesh$loc)[c("V1", "V2")]
  crs <- fm_crs(spat_mesh)
  pts <- st_as_sf(x = locs,
                  coords = colnames(locs)[1:2],
                  crs  = fm_crs(spat_mesh))

  return(pts)
}

getMeshEdgeInfo <- function(mesh_spatial) {
  
  el <- getMeshEdgeLengths(mesh_spatial)
  
  edge_stats <- data.frame(Metric = c("Minimum", "Mean", "Median", "Maximum"),
                           Length = c(min(el), mean(el), median(el), max(el)))
}


maskMesh <- function(spat_mesh, mask) {
  
  checkmate::assertClass(spat_mesh, "fm_mesh_2d")
  checkmate::assert(inherits(mask, "sfc_POLYGON") || inherits(mask, "sf"))
  checkmate::assert(fm_crs(spat_mesh) == st_crs(mask))

  nodes <- getMeshNodes(spat_mesh)

  idx <- st_within(nodes, mask, sparse = FALSE) 
  
  return(list(masked = nodes[idx, ],
              non_masked  = nodes[!idx,],
              idx = idx))
}


getMeshConfig <- function(spat_mesh_name) {
  config <- readYaml("mesh")
  if (!spat_mesh_name %in% names(config))
    return(list())
  return(config[[spat_mesh_name]])
}

getMeshInfo <- function(spat_mesh, infoString = TRUE) {
  
  checkmate::assertClass(spat_mesh, "fm_mesh_2d")
  
  edges <- rbind(spat_mesh$graph$tv[, c(1, 2)],
                 spat_mesh$graph$tv[, c(2, 3)],
                 spat_mesh$graph$tv[, c(3, 1)])
  e <- nrow(unique(t(apply(edges, 1, sort))))
  v <- spat_mesh$n
  t <- nrow(spat_mesh$graph$tv)
  
  result <- c(v = v, e = e, t = t)
  if (!infoString)
    return(result)
  
  return(paste(c("verices", "edges", "triangles"), result, sep = ":", collapse = ", "))
}


makeTemporalMesh <- function(start_year, end_year, knots, int_diff = FALSE, degree = 2) {
  
  checkmate::assertIntegerish(start_year)
  checkmate::assertIntegerish(end_year)
  checkmate::assertIntegerish(knots)
  checkmate::assertLogical(int_diff)
  checkmate::assertIntegerish(degree)
  
  start_date <- as.Date(paste0(start_year, "-01-01"))
  end_date <- as.Date(paste0(end_year, "-12-31"))
  
  dates_seq <- seq(start_date, end_date, length.out = knots)
  if (int_diff) {
    total_days <-  as.integer(end_date - start_date)
    divisors <- which(total_days %% seq_len(total_days) == 0L)
    steps <- max(divisors[divisors <= total_days %/% (knots - 1)])
    k_new <- total_days / steps + 1L
    
    dates_seq <- seq(start_date, end_date, length.out = k_new)
  }
  return(fmesher::fm_mesh_1d(loc = as.numeric(dates_seq), degree = degree))
}
