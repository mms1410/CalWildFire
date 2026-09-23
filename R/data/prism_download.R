library(rvest)
library(httr2)
library(stringr)
library(checkmate)
library(fs)
library(here)
library(terra)
library(sf)
#-------------------------------------------------------------------------------
source(path(getwd(), "R", "utils.R"))
config <- readYaml("data")
cal_crs <- st_crs(config$crs)
years <- config$start_year:config$end_year
prism_url <- config$prism$url
prism_variables <- config$prism$variables
cal <- readFile(path(getwd(), "data", "assets", "cal.gpkg"))
destination_dir <- dir_create(path(getwd(), "data", "raw"))
#-------------------------------------------------------------------------------
page <-  read_html(prism_url)
remote_folders <- page |>
  html_elements("a") |>
  html_attr("href")  |>
  str_subset("^[^/]+/$") |>
  str_subset(regex(paste0("^", prism_variables, collapse = "|")))

checkmate::assertTRUE(all(paste0(prism_variables, "/") %in% remote_folders))

for (folder_variable in remote_folders) {
  cat(paste0("Process PRISM folder '", folder_variable, "'\n"))
  url_prism_variable <- paste0(prism_url, folder_variable, "daily")
  page_prism_variable <- read_html(url_prism_variable)
  for (yr in years) {
    
    cat(paste0("... Process year ", yr, "\n"))
    # download zip file (complete usa), crop to ca, save tif and delete zip
    
    url_prism_variable_year <- paste0(url_prism_variable,"/", yr)
    folder_prism_var_year <- path(here(), "data", "raw", "prism", folder_variable, yr)
    dir_create(folder_prism_var_year, recurse = TRUE)
    
    # get all zip files of geodata for this year
    files_data_zip <- read_html(url_prism_variable_year) |>
      html_elements("a") |>
      html_attr("href") |>
      str_subset(regex("\\.zip$", ignore_case = TRUE))

    for (zip_file_name in files_data_zip) {
      url_zip_file <- paste0(url_prism_variable_year, "/", zip_file_name)
      zip_file <- path(folder_prism_var_year, zip_file_name)
      
      request(url_zip_file) |>
        req_retry(max_tries = 3, max_seconds = 60, retry_on_failure = TRUE) |>
        req_perform(zip_file) # download
      
      checkmate::assertFile(zip_file)
      name_tif_file <-  sub("zip", "tif", basename(zip_file))
      file_zip_rast <- paste0("/vsizip/", # virtual filesystem to read zip
                             zip_file,   # zip file full path
                             "/",
                             name_tif_file)    # tif inside zip
      
      # prism source for complete usa only
      # transform crs of ca temporarily to that of usa raster
      # then crop and transform to desired crs
      raster_usa <- rast(file_zip_rast)
      ca_crop <- st_transform(cal, crs(raster_usa))
      raster_ca <- raster_usa |>
        crop(ca_crop) |>
        mask(ca_crop) |>
        project(cal_crs$wkt)
      
      writeRaster(raster_ca, path(folder_prism_var_year, name_tif_file))
      file_delete(zip_file)
      cat(paste0("... ", name_tif_file, " completed\n"))
    }
  }
  cat(paste0("...Finished year ", yr, "\n"))
}