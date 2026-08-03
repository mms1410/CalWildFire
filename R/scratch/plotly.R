library(plotly)
#-------------------------------------------------------------------------------
source("R/utils/data_queries.R")
conf <- get_conf()
crs <- st_crs(read_conf(conf, "crs"))
fires <- read_geodata(sf_crs = crs, filename = "calfire")
#-------------------------------------------------------------------------------
fires_stpp <- fires |>
  transmute(time = as.numeric(date) ,
            long = st_coordinates(fires)[,1], 
            lat = st_coordinates(fires)[,2]) |> 
  st_drop_geometry() |>
  mutate(across(c(time ,long, lat), ~(.x - min(.x)) / (max(.x) - min(.x))))

plot_ly(fires_stpp, x = ~long, y = ~lat, z = ~time, color = ~time, size = 0.008)  
