library(here)
library(fs)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils", "data_queries.R"))
conf <- get_conf()
#-------------------------------------------------------------------------------
destination_folder <- path(here(), "assets")
dir_create(destination_folder)

url_ca_ecoz3 <- read_conf(conf, "url_ca_ecoz3")
url_ca_ecoz4 <- read_conf(conf, "url_ca_ecoz4")
url_ca_state <- read_conf(conf, "url_ca_state")
url_ca_county <- read_conf(conf, "url_ca_county")

download.file(url = url_ca_ecoz3, destfile = path(destination_folder, "ca_ecoz3.zip"))
download.file(url = url_ca_ecoz4, destfile = path(destination_folder, "ca_ecoz4.zip"))
download.file(url = url_ca_state, destfile = path(destination_folder, "ca_state.zip"))
download.file(url = url_ca_county, destfile = path(destination_folder, "ca_county.zip"))
