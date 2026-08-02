library(here)
library(fs)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils", "data_queries.R"))
config <- read_yaml()
#-------------------------------------------------------------------------------
destination_folder <- path(here(), "assets")
dir_create(destination_folder)

url_ca_ecoz3 <- config[["assets"]][["url_ca_ecoz_l3"]]
url_ca_ecoz4 <- config[["assets"]][["url_ca_ecoz_l4"]]
url_ca_state <- config[["assets"]][["url_ca_state"]]

download.file(url = url_ca_ecoz3, destfile = path(destination_folder, "ca_ecoz3.zip"))
download.file(url = url_ca_ecoz4, destfile = path(destination_folder, "ca_ecoz4.zip"))
download.file(url = url_ca_state, destfile = path(destination_folder, "ca_state.zip"))