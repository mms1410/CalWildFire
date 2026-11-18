compute_z_score <- function(values) {
  mean_sample <- mean(values)
  sd_sample <- sd(values)
  return((values - mean_sample) / sd_sample)
}
scale_area <- function(area) {
  return(log1p(area))
}

scale_tmax <- function(values) {
  return(compute_z_score(values))
}

scale_vs <- function(values) {
  return(sqrt(values) + 1)
}

scale_vpdmax <- function(values) {
  return(compute_z_score(values))
}


scale_variable <- function(value, variable_name) {
  switch(variable_name,
         "ppt"    = value,
         "vs"     = scale_vs(value),
         "tmax"   = scale_tmax(value),
         "vpdmax" = scale_vpdmax(value),
         stop(sprintf("no scaler implemented for variable %s", variable_name)))
}
