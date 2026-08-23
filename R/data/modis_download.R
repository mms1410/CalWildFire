#-------------------------------------------------------------------------------
#                 APPEEARS CREDENTIALS NECESSARY
#-------------------------------------------------------------------------------
library(sf)
library(dotenv)
library(appeears)
#-------------------------------------------------------------------------------
source("R/const.R")
apeears <- CONF[["modis"]][["appeears"]]
destination_dir <- path(here(), "data", "raw")
dir_create(destination_dir)
#-------------------------------------------------------------------------------
dotenv::load_dot_env()
checkmate::assert("EARTHDATA_USER" %in% names(Sys.getenv()),
                  "Expected to find 'EARTHDATA_USER' in environment to login at appeears")
checkmate::assert("EARTHDATA_PASSWORD" %in% names(Sys.getenv()),
                  "Expected to find 'EARTHDATA_PASSWORD' in environment to login at appeears")
rs_set_key(user = Sys.getenv("EARTHDATA_USER"), password = Sys.getenv("EARTHDATA_PASSWORD"))
query_start <- paste0(CONF[["start_year"]], "-01", "-01")
query_end <- paste0(CONF[["end_year"]], "-12", "-31")
#-------------------------------------------------------------------------------
# info:
# available products => rs_products()
# available layers => rs_layers(product_query)

# appeears config consists of one entry for each product containing 
# further specifications
available_products <- rs_products()
for (item in names(apeears)) {
  cat(paste0("Setup Query for Item '", item, "' ...\n"))
  
  
  item_data <- apeears[[item]]
  checkmate::assert(all(c("product", "layer") %in% names(item_data)))
  available_layers <- rs_layers(item_data$product)
  checkmate::assert(item_data$layer %in% unlist(available_layers$Layer))
  
  
  query_frame <- data.frame(
    task = item,
    subtask = paste0(item_data$product, "_", item_data$product),
    product = item_data$product,
    layer = item_data$layer,
    start = query_start,
    end = query_end)
  
  task <- rs_build_task(
    df = query_frame,
    roi = CA,
    format = "geotiff")
  
  query_request <- rs_request(
    request = task,
    user = Sys.getenv()[["EARTHDATA_USER"]],
    transfer = TRUE,
    path = destination_dir,
    verbose = TRUE,
    # if time out reached rs_transfer(id) can be used or task id looked up at appeears account
    # where id is prompted at beginning of request to console
    # or alternatively can be found in appeears account
    time_out = 18000) #5h
}