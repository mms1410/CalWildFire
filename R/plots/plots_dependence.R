library(terra)
#-------------------------------------------------------------------------------
source("R/grand_util.R")
source("R/plots/utils.R")
source("R/constants.R")

extract_location <- function(points, raster, win = 90) {
  
  locations <- terra::extract(raster, fires, ID = FALSE)
  mask_past <- outer(targets, as.Date(colnames(locations)), FUN = "<")
  
  locations_masked <- locations
  locations_masked[mask_past] <- NA
  
  if (!is.null(win)) {
    mask_win<-  outer(targets, as.Date(colnames(locations)),
                          FUN = function(target, date_val) date_val > (target - win))
    locations_masked[mask_win] <- NA
  }
  return(locations_masked)
}
#-------------------------------------------------------------------------------
fires <- read_file(path(here(), "data",  "preprocessed", "calfire.gpkg"), CRS) |>
  mutate(area = area / 247.1)
barea <- read_file("burntarea.gpkg", CRS)
ecoz  <- read_file("ca_ecoz3.zip", CRS)
fires_ecoz <- st_join(fires, ecoz, join = st_intersects, left = TRUE)
ppt <- read_file("ppt.tif", CRS) 
ppt_fires <- extract_location(fires, ppt)


ppt_fires_long <- ppt_fires %>%
  rowid_to_column("fire_id") %>%
  pivot_longer(-fire_id, names_to = "date", values_to = "ppt") %>%
  mutate(date = as.Date(date))

ggplot(ppt_fires_long, aes(x = date, y = ppt, group = fire_id, color = fire_id)) +
  geom_line(alpha = 0.6, size = 0.8) +
  labs(x = "Date", y = "Precipitation (mm)",
       title = "Antecedent precipitation patterns") +
  theme_minimal()
