library(terra)
library(fs)
library(here)
library(stringr)
library(rvest)
library(httr2)
#-------------------------------------------------------------------------------
source(path(here(), "R", "const.R"))
url_gridmet <- CONF$gridmet$url
variables <- CONF$gridmet$variables
destination_folder <- path(here(), "data")
dir_create(destination_folder)
#-------------------------------------------------------------------------------
html_page <-  read_html(url_gridmet)
remote_files <- html_page |>
  html_elements("a") |>
  html_attr("href") |>
  str_subset(regex(paste0(variables, collapse = "|"))) |>
  str_subset(regex(paste0(CONF$start_year:CONF$end_year, collapse = "|")))

variable_files_chunk <- split(remote_files, sub("_.*", "", remote_files))

for (variable in names(variable_files_chunk)) {
  variable_files <- variable_files_chunk[[variable]]
  raster_list <- list()
  cat(paste0("Process variable '", variable, "'\n"))
  for (variable_file in variable_files) {
    year <- str_extract(variable_file, "\\d{4}")
    cat(paste0("   Process year ", year, " ....\n"))
    tmp_file <- tempfile(variable_file, fileext = ".nc")
    
    request(paste0(url_gridmet, variable_file)) |>
      req_retry(max_tries = 3, max_second = 60, retry_on_failure = TRUE) |>
      req_perform(tmp_file)

    raster <- terra::rast(tmp_file, drivers = "NETCDF")
    CA <- st_transform(CA, crs(raster))
    
    raster <- raster |>
      crop(CA) |>
      mask(CA) |>
      project(CRS$wkt)
    names(raster) <- time(raster)
    raster_list[[year]] <- raster
    unlink(tmp_file)
  }
  variable_raster <- rast(raster_list)
  names(variable_raster) <- time(variable_raster)
  writeRaster(variable_raster,
              filename = path(destination_folder, paste0(variable, ".tif")),
              overwrite = TRUE)
}