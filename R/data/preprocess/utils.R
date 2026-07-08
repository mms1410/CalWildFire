#' Parse a character vector
#'
#' @param strings vector containing strings to be parsed
#' @param parserlist a list of parsers to use in succession
#'
#'
parse_string <- function(strings, parserlist) {
  
  checkmate::assertCharacter(strings)
  checkmate::assertList(parserlist)
  
  # try parser chain
  for (parser_name in names(parserlist)) {
    parser <- parserlist[[parser_name]]
    result <- parser(strings)
  }
  if (!all(is.na(result))) return(result)
  return(rep(NA, length(strings)))
}

parsers_prism <- list(
  parser1 = function(names) {
    names |>
      str_extract("\\d{6}(?=.zip)") |>
      ym()
  }
)

parsers_modis <- list(
  parser1 = function(names) {
    names |>
      str_extract("doy(\\d{7})", group = 1) |>
      (\(x) as.Date(paste0(substr(x, 1, 4), "-01-01")) + as.integer(substr(x, 5, 7)) - 1)()
  }
)

read_zip_raster <- function(filepath, pattern = "\\.tif$") {
  
  checkmate::assertFileExists(filepath, extension = "zip")
  data_file <- str_subset(unzip(filepath, list = TRUE)$Name, pattern)
  rast(paste0("/vsizip/", filepath, "/", data_file))
}

set_raster_level_lc <- function(raster, type = "1") {
  
  levels_frame <- levels(raster)[[1]]
  checkmate::assert("ID" %in% colnames(levels_frame))
  
  lookup_table <- get_lc_lookup(type = type)
  to_set <- levels_frame |> 
    select(ID) |>
    left_join(lookup_table, by = c("ID" = "value")) |>
    rename(value = ID)
  levels(raster) <- to_set
  raster
}


get_lc_lookup <- function(type = "1") {
  
  checkmate::assertCharacter(type)
  
  # LC_Type1 — IGBP classification, range [1,17]
  lc_type1 <- data.frame(
    value = 1:17,
    class = c(
      "Evergreen Needleleaf Forests",
      "Evergreen Broadleaf Forests",
      "Deciduous Needleleaf Forests",
      "Deciduous Broadleaf Forests",
      "Mixed Forests",
      "Closed Shrublands",
      "Open Shrublands",
      "Woody Savannas",
      "Savannas",
      "Grasslands",
      "Permanent Wetlands",
      "Croplands",
      "Urban and Built-up Lands",
      "Cropland/Natural Vegetation Mosaics",
      "Permanent Snow and Ice",
      "Barren",
      "Water Bodies"))
  
  # LC_Type2 — University of Maryland (UMD) classification, range [0,15]
  lc_type2 <- data.frame(
    value = 0:15,
    class = c(
      "Water Bodies",
      "Evergreen Needleleaf Forests",
      "Evergreen Broadleaf Forests",
      "Deciduous Needleleaf Forests",
      "Deciduous Broadleaf Forests",
      "Mixed Forests",
      "Closed Shrublands",
      "Open Shrublands",
      "Woody Savannas",
      "Savannas",
      "Grasslands",
      "Croplands",
      "Urban and Built-up Lands",
      "Cropland/Natural Vegetation Mosaics",
      "Non-Vegetated Lands",
      "Unclassified"))
  
  # LC_Type3 — LAI/fPAR classification (Myneni et al.), range [0,10]
  lc_type3 <- data.frame(
    value = 0:10,
    class = c(
      "Water Bodies",
      "Grasslands",
      "Shrublands",
      "Broadleaf Croplands",
      "Savannas",
      "Evergreen Broadleaf Forests",
      "Deciduous Broadleaf Forests",
      "Evergreen Needleleaf Forests",
      "Deciduous Needleleaf Forests",
      "Non-Vegetated Lands",
      "Urban and Built-up Lands"))
  
  # LC_Type4 — BIOME-BGC classification (Running et al.), range [0,8]
  lc_type4 <- data.frame(
    value = 0:8,
    class = c(
      "Water Bodies",
      "Evergreen Needleleaf Vegetation",
      "Evergreen Broadleaf Vegetation",
      "Deciduous Needleleaf Vegetation",
      "Deciduous Broadleaf Vegetation",
      "Annual Broadleaf Vegetation",
      "Annual Grass Vegetation",
      "Non-Vegetated Land",
      "Urban and Built-up Lands"))
  
  # LC_Type5 — Plant Functional Type classification (Bonan et al.), range [0,11]
  lc_type5 <- data.frame(
    value = 0:11,
    class = c(
      "Water Bodies",
      "Evergreen Needleleaf Trees",
      "Evergreen Broadleaf Trees",
      "Deciduous Needleleaf Trees",
      "Deciduous Broadleaf Trees",
      "Shrub",
      "Grass",
      "Cereal Croplands",
      "Broadleaf Croplands",
      "Urban and Built-up Lands",
      "Permanent Snow and Ice",
      "Non-Vegetated Lands"))
  
  switch(type,
         "1" = lc_type1,
         "2" = lc_type2,
         "3" = lc_type3,
         "4" = lc_type4,
         "5" = lc_type5)
}