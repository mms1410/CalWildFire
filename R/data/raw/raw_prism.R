library(fs)
library(rvest)
library(httr2)
library(here)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils", "data_queries.R"))
config <- get_conf()
# https://data.prism.oregonstate.edu/PRISM_datasets.pdf
#url_prism <- "https://data.prism.oregonstate.edu/time_series/us/lt/800m/"
options(timeout = 300)
# TODO: validity checks
years <- config[["start_year"]]:config[["end_year"]]
prism_page <- read_html(config[["url_prism"]])

variables <- c("ppt", "tmax", "vpdmax")
selection <- paste0("^(", paste(variables, collapse = "|"), ")/$")
#-------------------------------------------------------------------------------
log_message_fail <- function(msg, log_fails_file) {
  timestamped <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", msg)
  message(timestamped)
  cat(timestamped, "\n", file = log_fails_file, append = TRUE)
}

download_with_retry <- function(url, destfile, retries = 3) {
  for (i in seq_len(retries)) {
    tryCatch({
      download.file(url, destfile, mode = "wb", quiet = TRUE)
      return(invisible(TRUE))
    }, error = function(e) {
      # wait before retrying
      Sys.sleep(5)
    })
  }
  # do not throw error but return FALSE and give warning
  warning("Failed after ", retries, " attempts: ", url)
  return(invisible(FALSE))
}

folders <- prism_page |>
  html_elements("a") |>
  html_attr("href")
subfolders <- folders[grepl("^[^/]+/$", folders)]
subfolders <- subfolders[grepl(selection, subfolders)]


for (subfolder in subfolders) {
  query_url <- paste0(config[["url_prism"]], subfolder, "monthly")
  query_page <- read_html(query_url)
  folders <- query_page |>
    html_elements("a") |>
    html_attr("href")
  for (year in years) {
    destination_dir <- path(here(), "data", "raw", "prism", subfolder)
    dir_create(destination_dir)
    query_folder_url <- paste0(query_url, "/", year)
    subquery_page <- read_html(query_folder_url)
    subquery_items <- subquery_page |>
      html_elements("a") |>
      html_attr("href")
    zips <- subquery_items[grepl(".zip$", subquery_items)]
    final_destination <- path(destination_dir, year)
    dir_create(final_destination)
    for (zipfile in zips) {
      download_url <- paste0(query_folder_url, "/", zipfile)  # path uses https:/.. instead of https://...
      download_with_retry(download_url, path(final_destination, zipfile))
    }
  }
}
