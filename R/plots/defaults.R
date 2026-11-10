library(ggplot2)
library(ggsci)
library(fmesher)
#-------------------------------------------------------------------------------
theme_set(theme_light())
theme_ecoregion <- theme(
  legend.position = "bottom",
  legend.text = element_text(size = 5),
  legend.title = element_text(size = 6),
  legend.key.size = unit(0.2, "cm"))


options(
  ggplot2.discrete.colour = function(...) ggsci::scale_color_d3(palette = "category20", ...),
  ggplot2.discrete.fill   = function(...) ggsci::scale_fill_d3(palette = "category20", ...),
  ggplot2.continuous.colour = function(...) viridis::scale_color_viridis(...),
  ggplot2.continuous.fill   = function(...) viridis::scale_fill_viridis(...)
)
