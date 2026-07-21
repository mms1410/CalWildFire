wrap_latex_headers <- function(headers, width = "1.1cm") {
  # Avoid wrapping empty strings or LaTeX special placeholders (like "")
  ifelse(
    headers == "" | is.na(headers),
    headers,
    sprintf("\\parbox{%s}{\\centering %s}", width, headers)
  )
}


ecoz_names_map <- c(
  "Coast Range" = "Coast Range",
  "Central Basin and Range" = "Central Basin & Range",
  "Mojave Basin and Range" = "Mojave Basin",
  "Cascades" = "Cascades",
  "Sierra Nevada" = "Sierra Nevada",
  "Central California Foothills and Coastal Mountains" = "Central CA Foothills & Coast",
  "Central California Valley" = "Central Valley",
  "Klamath Mountains/California High North Coast Range" = "Klamath/North Coast",
  "Southern California Mountains" = "Southern CA Mountains",
  "Northern Basin and Range" = "Northern Basin & Range",
  "Sonoran Basin and Range" = "Sonoran Basin",
  "Southern California/Northern Baja Coast" = "Southern CA/Baja Coast",
  "Eastern Cascades Slopes and Foothills" = "Eastern Cascades Slopes"
)
rename_source <- function(source, mapping) {
  checkmate::assertTRUE(all(source %in% names(mapping)))
  unname(mapping[source])
}

