library(checkmate)
library(fs)
library(here)
library(rvest)
library(httr2)
library(terra)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils", "data_queries.R"))
config <- read_yaml()
years <- config[["start_year"]]:config[["end_year"]]
url_prism <- config[["prism"]][["url"]]
variables <- config[["prism"]][["variables"]]
crs <- st_crs(config[["crs"]])
ca <- read_geodata(sf_crs = crs, keyword = "ca_state")
#-------------------------------------------------------------------------------
page_entry <-  read_html(url_prism)
folders_entry <- page_entry |>
  html_elements("a") |>
  html_attr("href")  |>
  str_subset("^[^/]+/$") |>
  str_subset(regex(paste0("^", variables, collapse = "|")))

checkmate::assertTRUE(all(paste0(variables, "/") %in% folders_entry))

for (folder_variable in folders_entry) {
  cat(paste0("Process PRISM folder '", folder_variable, "'\n"))
  url_prism_variable <- paste0(url_prism, folder_variable, "daily")
  page_prism_variable <- read_html(url_prism_variable)
  for (yr in years) {
    
    cat(paste0("... Process year ", yr, "\n"))
    # download zip file (complete usa), crop to ca, save tif and delete zip
    
    url_prism_variable_year <- paste0(url_prism_variable,"/", yr)
    folder_prism_var_year <- path(here(), "data", "raw", "prism", folder_variable, yr)
    dir_create(folder_prism_var_year, recurse = TRUE)
    files_data_zip <- read_html(url_prism_variable_year)|>
      html_elements("a") |>
      html_attr("href") |>
      str_subset(regex("\\.zip$", ignore_case = TRUE))

    for (zip_file_name in files_data_zip) {
      url_zip_file <- paste0(url_prism_variable_year, "/", zip_file_name)
      zip_file <- path(folder_prism_var_year, zip_file_name)
      
      request(url_zip_file) |>
        # req_timeout(300) |>
        req_retry(max_tries = 3, max_second = 60, retry_on_failure = TRUE) |>
        req_perform(zip_file)
      
      checkmate::assertFile(zip_file)
      tif_file <-  sub("zip", "tif", basename(zip_file))
      file_zip_rast <-paste0("/vsizip/", # virtual filesystem to read zip
                             zip_file,   # zip file full path
                             "/",
                            tif_file)    # tif inside zip
      
      # prism source for complete usa only
      # transform crs of ca temporarily to that of usa raster
      # then crop and transform to original desired crs
      raster_usa <- rast(file_zip_rast)
      ca_crop <- st_transform(ca, st_crs(raster_usa))
      raster_ca <- raster_usa |>
        crop(ca_crop) |>
        mask(ca_crop) |>
        project(crs$wkt)
      
      writeRaster(raster_ca, path(folder_prism_var_year, tif_file))
      file_delete(zip_file)
      cat(paste0("... ", tif_file, " completed\n"))
    }
  }
  cat(paste0("...Finished year ", yr, "\n"))
}
