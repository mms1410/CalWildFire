library(inlabru)
library(INLA)
library(fmesher)
#-------------------------------------------------------------------------------
source("R/utils/data_queries.R")
source("R/utils/geo_comp.R")
#-------------------------------------------------------------------------------
conf <- get_conf()
crs <- st_crs(read_conf(conf, "crs"))
dest <- path(here::here(), "assets", "plots", "results", "lgcp")
dir_create(dest)



rescale_sf <- function(x, factor = 1000, keep_crs = FALSE) {
  stopifnot(inherits(x, "sf"))
  
  if (is.na(sf::st_crs(x))) {
    warning("Input sf object has no CRS -- it may already be rescaled. Proceeding anyway.")
  }
  
  orig_crs <- sf::st_crs(x)
  sf::st_geometry(x) <- sf::st_geometry(x) / factor
  sf::st_crs(x) <- NA
  
  if (keep_crs) attr(x, "orig_crs") <- orig_crs
  
  x
}


month_index <- function(t) {
  origin_date <-  min(fires$date)
  actual_date <- origin_date + lubridate::ddays(t)
  lubridate::month(actual_date)
}
year_index <- function(t) {
  origin_date <-  min(fires$date)
  actual_date <- origin_date + lubridate::ddays(t)
  lubridate::year(actual_date) - lubridate::year(origin_date) + 1L
}
season_index <- function(t) {
  origin_date <-  min(fires$date)
  actual_date <- origin_date + lubridate::ddays(t)
  m <- lubridate::month(actual_date)
  dplyr::case_when(
    m %in% c(12, 1, 2) ~ 1L,
    m %in% c(3, 4, 5)  ~ 2L,
    m %in% c(6, 7, 8)  ~ 3L,
    TRUE                ~ 4L)
}
#-------------------------------------------------------------------------------
ca <- read_geodata(keyword = "ca_state", sf_crs = crs) |>
  rescale_sf() |>
  st_set_geometry("geometry") |>
  get_mainland()


fires <- read_geodata(sf_crs = crs, filename = "calfire") |>
  select(date) |>
  rescale_sf() |> # rescale to km
  st_set_geometry("geometry") |>
  st_filter(ca) |>
  mutate(time = as.numeric(difftime(date, min(date), units = "days")))

duplicates <- duplicated(st_geometry(fires))
fires <- fires[!duplicates, ]



all(month_index(fires$time) == lubridate::month(fires$date))
all(season_index(fires$time) == dplyr::case_when(
  lubridate::month(fires$date) %in% c(12,1,2) ~ 1L,
  lubridate::month(fires$date) %in% c(3,4,5)  ~ 2L,
  lubridate::month(fires$date) %in% c(6,7,8)  ~ 3L,
  TRUE ~ 4L))


mesh <- fm_mesh_2d_inla(
  boundary = ca,
  max.edge = c(10, 40),   # km
  cutoff   = 2            # km
)

mesh_spatial <- fmesher::fm_mesh_2d(boundary = ca,
                                 max.edge = c(50, 150),
                                 cutoff = 2,
                                 offset = c(50, 200))
mesh_temporal <- fmesher::fm_mesh_1d(seq(min(fires$time), max(fires$time), length.out = 12 * 21))


spde <- INLA::inla.spde2.pcmatern(mesh_spatial,
                                  prior.range = c(50, 0.5),
                                  prior.sigma = c(1, 0.5))
#-------------------------------------------------------------------------------
cmp1 <-  geometry ~ Intercept(1) + field(geometry, model = spde)
fit1 <- inlabru::lgcp(cmp1,
              data = fires,
              samplers =  st_sf(geometry = st_geometry(ca)),
              domain = list(geometry = mesh_spatial),
              options = list(control.inla = list(int.strategy = "eb")))


cmp2 <- geometry + time ~ Intercept(1) +
  spatial(geometry, model = spde) +
  trend(year_index(time), model = "rw1") +
  seasonal(season_index(time), model = "iid")

fit2 <- inlabru::lgcp(cmp2,
                     data = fires,
                     domain = list(geometry = mesh_spatial, time = mesh_temporal),
                     samplers = ca,
                     options = list(control.inla = list(int.strategy = "eb", strategy = "gaussian")))
#-------------------------------------------------------------------------------
ggplot() +
  gg(mesh_spatial) +
  geom_sf(data = ca, fill = NA, color = "black", linewidth = 0.5) +
  geom_sf(data= fires, color = "red", size= 0.08, alpha = 0.3) +
  coord_sf(datum = NA) +
  labs(x = "", y = "") +
  theme_minimal()
ggsave(path(dest, "spatialmesh.png"))


pred_pixels <- fmesher::fm_pixels(mesh_spatial, mask = ca, dims = c(150, 150))
pred_spatial <- predict(fit, pred_pixels, ~ spatial)
trend_df <- fit$summary.random$trend |>
  dplyr::mutate(year = ID)
season_labels <- c("1" = "Winter", "2" = "Spring", "3" = "Summer", "4" = "Fall")
seasonal_df <- fit$summary.random$seasonal |>
  dplyr::mutate(season = factor(season_labels[as.character(ID)],
                                levels = c("Winter", "Spring", "Summer", "Fall")))

ggplot() +
  gg(pred_spatial, geom = "tile", aes(fill = mean)) +
  scale_fill_viridis_c(name = expression(xi(s))) +
  geom_sf(data = ca, fill = NA, color = "black", linewidth = 0.3) +
  coord_sf(datum = NA) +
  labs(x = "", y = "") +
  theme_minimal()
ggsave(filename = path(dest, "lgcp_xi.png"))

ggplot(trend_df, aes(x = year, y = mean)) +
  geom_ribbon(aes(ymin = `0.025quant`, ymax = `0.975quant`), fill = "steelblue", alpha = 0.2) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue") +
  labs(y = expression(tau(y))) +
  theme_minimal()
ggsave(filename = path(dest, "lgcp_tau.png"))


ggplot(seasonal_df, aes(x = season, y = mean)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(ymin = `0.025quant`, ymax = `0.975quant`), color = "darkorange") +
  labs(x = NULL, y = expression(sigma(q))) +
  theme_minimal()
ggsave(filename = path(dest, "lgcp_sigma.png"))

