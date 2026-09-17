library(ggplot2)
#-------------------------------------------------------------------------------
theme_set(theme_light())
options(
  # Discrete scales
  ggplot2.discrete.colour = function(...)
    ggsci::scale_color_d3(palette = "category20", ...),
  
  ggplot2.discrete.fill = function(...)
    ggsci::scale_fill_d3(palette = "category20", ...),
  
  # Continuous scales
  ggplot2.continuous.colour = function(...)
    viridis::scale_color_viridis(...),
  
  ggplot2.continuous.fill = function(...)
    viridis::scale_fill_viridis(...)
)
