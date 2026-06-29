#-------------------------------------------------------------------------------
#                 APPEEARS CREDENTIALS NECESSARY
#-------------------------------------------------------------------------------
library(sf)
library(dotenv)
library(appeears)
#-------------------------------------------------------------------------------
source("R/utils/data_queries.R")
conf <- get_conf()
apeears <- read_conf(conf, "appeears")
crs <- st_crs(read_conf(conf, "crs"))
ca <- read_sf_frame(keyword = "ca_state", sf_crs = crs)
ca_bbox <- ca |> st_transform(4326) |> st_bbox() |> as.numeric()
#-------------------------------------------------------------------------------
dotenv::load_dot_env()
checkmate::assert("EARTHDATA_USER" %in% names(Sys.getenv()),
                  "Expected to find 'EARTHDATA_USER' in environment to login at appeears")
checkmate::assert("EARTHDATA_PASSWORD" %in% names(Sys.getenv()),
                  "Expected to find 'EARTHDATA_PASSWORD' in environment to login at appeears")
rs_set_key(user = Sys.getenv("EARTHDATA_USER"), password = Sys.getenv("EARTHDATA_PASSWORD"))

query_start <- paste0(read_conf(conf, "start_year"), "-01", "-01")
query_end <- paste0(read_conf(conf, "end_year"), "-12", "-31")
#-------------------------------------------------------------------------------
# info:
# available products => rs_products()
# available layers => rs_layers(product_query)
destination_dir <- path(here(), "data", "raw")
dir_create(destination_dir)

for (variable in names(apeears)) {
  query_data <- apeears[[variable]]
  message(paste0("Query variable '", variable, "' ..."))

  query_frame <- data.frame(
    task = variable,
    subtask = paste0(variable, "_", query_data$product),
    product = query_data$product,
    layer = query_data$layer,
    start = query_start,
    end = query_end)
  
  task <- rs_build_task(
    df = query_frame,
    roi = ca,
    format = "geotiff")
  
  query_request <- rs_request(
    request = task,
    user = Sys.getenv()[["EARTHDATA_USER"]],
    transfer = TRUE,
    path = destination_dir,
    verbose = TRUE,
    # if time out reached rs_transfer(id) can be used
    # where id is prompted at beginning of request to console
    # or alternatively found in appeears account
    time_out = 18000)
}
rs_transfer("9c85dc85-bf43-4768-bac1-2c1b11e8b4dc", user =  Sys.getenv()[["EARTHDATA_USER"]], path = path(here(), "data", "raw"))