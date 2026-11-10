library(fs)
library(scales)
library(sf)
library(dplyr)
library(lubridate)
library(checkmate)
#-------------------------------------------------------------------------------
source(path(getwd(), "R", "utils", "utils_io.R"))
source(path(getwd(), "R", "utils", "utils_geocomp.R"))
#-------------------------------------------------------------------------------

#===============================================================================
#                             Data Query
#===============================================================================


#-------------------------------------------------------------------------------

#'
#' @param fires
#' @param cache
#' @param filename
#' @param dir_cache
#'
cacheFires <- function(fires = NULL, filename = "fires_chached.gpkg", dir_cache = path(here(), "data", "cache")) {
  
  checkmate::assertString(filename)
  checkmate::assertString(dir_cache)
  
  if (file_exists(path(dir_cache, filename))) {
    return(readFile(path(dir_cache, filename)))
  }
  
  checkmate::assertClass(fires, "sf")
  
  fires <- fires |>
    addRastToSfPoint("evi.tif", "evi") |>
    addRastToSfPoint("lai.tif", "lai") |>
    addRastToSfPoint("tmax.tif", "tmax") |>
    addRoadDistance() |>
    addCoastDistance()
  
  dir_create(dir_cache)
  
  st_write(fires, path(dir_cache, filename), append = FALSE)
  message(sprintf("Cached fires at: %s", path(dir_cache, filename)))
  return(fires)
}


#' Combine spatial integration mesh with temporal mesh to a spatial-temporal integration scheme
#' 
#' @param spat_ips
#' @param mesh_temporal
#' @param samplers
#' @param filename
#' @param dir_cache
#'
cacheIntegrationMesh <- function(mesh_int = NULL, mesh_temporal = NULL, samplers = NULL, filename = "int_grid_cached.rds", dir_cache = path(here(), "data", "cache")) {
  
  checkmate::assertString(filename)
  checkmate::assertString(dir_cache)
  
  if (file_exists(path(dir_cache, filename)))  {
    return(readRDS(path(dir_cache, filename)))
  }
  
  
  checkmate::assertClass(mesh_int, "fm_mesh_2d")
  checkmate::assertClass(mesh_temporal, "fm_mesh_1d")
  checkmate::assertClass(samplers, "sf")
  
  message(sprintf("Setup integration grid %s...", filename))
  
  
  
  message("created spatial-temporal integration mesh, query covariates now...")
  
  int_mesh <- int_mesh |>
    mutate(date = as.Date(time)) |>
    addRastToSfPoint("evi.tif", "evi") |>
    addRastToSfPoint("lai.tif", "lai") |>
    addRoadDistance()
  
  message("Data query finished")
  dir_create(dir_cache)
  saveRDS(int_mesh, path(dir_cache, filename))
  message(sprintf("Cached integration grid at: %s", path(dir_cache, filename)))
  return(int_mesh)
}



filterInsideMesh <- function(fires, mesh) {
  
  fires |>
    filter(st_within_logical(geometry,fm_as_sfc(mesh)))
}


cacheDataMeteo <- function(filename, variable) {
  
  matches <- getFileMatches(filename)
  
  if (length(matches) == 0) {
    
  }
}


#===============================================================================

buildScaler <- function(mu, sigma) {
  force(mu); force(sigma)
  scaler <- scales::trans_new(name      = "standardize",
                              transform = function(x) (x - mu) / sigma,
                              inverse   = function(z) z * sigma + mu)
  attr(scaler, "mu_train")    <- mu
  attr(scaler, "sigma_train") <- sigma
  return(scaler)
}


makeScaler <- function(dtbl) {
  checkmate::assertNames(colnames(dtbl), must.include = "value")
  mu    <- mean(dtbl$value, na.rm = TRUE)
  sigma <- sd(dtbl$value, na.rm = TRUE)
  return(buildScaler(mu, sigma))
}


getTrainYears <- function(sequence = FALSE) {
  
  config <- readYaml("data")
  if (sequence)
    return(seq(config$start_train, config$end_train))
  
  return(c(as.Date(paste0(config$start_train, "-01", "-01")),
           as.Date(paste0(config$end_train, "-12", "-31"))
           ))
}

setScaler <- function(variable_name, tbl_spec, destination_dir = path("assets", "scalers")) {
  
  checkmate::assertCharacter(variable_name)
  checkmate::assertCharacter(tbl_spec)
  
  
  scaler_file <- paste0("scaler_", variable_name, "_", tbl_spec, ".rds")
  
  if (length(searchFile(scaler_file)) == 1) {
    scaler <- readFile(scaler_file)
  } else {
    dtbl <- readFile(paste0(variable_name, "_", tbl_spec, ".csv"))
    scaler <- makeScaler(dtbl)
    message(sprintf("Save new %s to %s", scaler_file, destination_dir))
    dir_create(destination_dir)
    saveRDS(scaler, path(destination_dir, scaler_file))
  }
  return(scaler)
}




queryStation <- function(raster) {
  
  
  ca_mld <- readFile("cal_mld.gpkg")
  
  stations <- readFile("stations.gpkg") |>
    st_transform(st_crs(raster))
  
  values <- terra::extract(raster, stations)
  
  # stations <- readr::read_fwf("https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt",
  #                       col_positions = readr::fwf_positions(start = c(1,  13, 22, 32, 39, 42, 73, 77, 81),
  #                                                     end =   c(11, 20, 30, 37, 40, 71, 75, 79, 85),
  #                                                     col_names = c("id", "latitude", "longitude", "elevation", "state", "name", "gsn_flag", "hcn_crn_flag", "wmo_id")))
  #  stations <- stations |>
  #    dplyr::filter(state == "CA") |>
  #    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  # st_write(stations, path(here(), "data", "auxiliary", "stations.gpkg"), append = FALSE)
  # 
}


getEvalGrid <- function(start_date, end_date, by = "day", cellsize = 5) {
  
  eval_dates <- seq.Date(from = start_date, end_date, by = by)
  
  eval_grid <- st_sf(geometry = st_make_grid(ca_mld, what = "centers", cellsize = cellsize)) |>
    st_filter(ca_mld) |>
    mutate(id = row_number())
  
  eval_grid <- tidyr::crossing(id = eval_grid$id, date = eval_dates) |>
    left_join(eval_grid, by = "id") |>
    st_as_sf()
  
  return(eval_grid)
}


saveModelFactory <- function(destination_dir, spec) {
    return(function(model, model_spec){
      saveRDS(model, path(destination_dir, paste0("fit_", spec, "_", model_spec, ".rds")))
    })
}
