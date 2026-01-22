library(tidyverse)
library(openxlsx)
library(janitor)
library(sf)
library(leaflet)

# --------------------------------------------------
# Input/Output paths
# --------------------------------------------------
input_file <- "./data_results/Whirling_Disease_2025_Sample_Tracking.xlsx"
output_file <- "./output/Whirling_Disease_2025_Sample_Tracking_correct_coords.xlsx"

# --------------------------------------------------
# Get all sheet names
# --------------------------------------------------
sheets <- openxlsx::getSheetNames(input_file)

# --------------------------------------------------
# Conversion functions
# --------------------------------------------------

# Updated dms_to_dd to handle space-separated DMS without symbols.
dms_to_dd <- function(x) {
  if (is.na(x) || x == "") return(NA_real_)
  
  # Clean the string: replace common symbols with a space
  x_clean <- str_replace_all(x, "[NSEWnsew°'\"]", " ")
  
  # Extract all numbers
  nums <- str_extract_all(x_clean, "\\d+\\.*\\d*")[[1]] %>% 
    as.numeric()
  
  # Check if numbers were extracted successfully
  if (length(nums) == 0 || any(is.na(nums))) return(NA_real_)
  
  # Calculate Decimal Degrees
  if (length(nums) >= 3) {
    # Degrees, Minutes, Seconds (DMS)
    dd <- nums[1] + nums[2]/60 + nums[3]/3600
  } else if (length(nums) == 2) {
    # Degrees, Minutes (DM)
    dd <- nums[1] + nums[2]/60
  } else if (length(nums) == 1) {
    # Decimal Degrees (DD)
    dd <- nums[1]
  } else {
    return(NA_real_)
  }
  
  # Apply negative sign if S or W is present
  if (str_detect(x, "[swSW]")) dd <- -dd
  
  return(dd)
}

# Updated detect_type to better identify space-separated DMS.
detect_type <- function(x) {
  if (is.na(x) || x == "") return("unknown")
  x_str <- as.character(x)
  val <- suppressWarnings(as.numeric(x_str))
  
  # Explicit DMS checks (symbols or space-separated numbers)
  if (str_detect(x_str, "°|'|\"")) return("dms")
  if (str_detect(x_str, "[NSEWnsew]")) return("dms")
  
  # Check for space-separated numbers (e.g., "49 17 848")
  # Requires at least one space and the string must start with a digit
  if (str_detect(x_str, "^\\d+.*\\s+\\d+")) return("dms") 
  
  # Check for Decimal or Projected
  if (!is.na(val)) {
    if (val > 300000) return("projected") # UTM or Albers (assumed)
    return("decimal")
  }
  
  "unknown"
}

convert_coords <- function(df) {
  df <- df %>% 
    rowid_to_column("row_id") %>%
    mutate(
      # Coerce to character first to maintain DMS/Projected format during type detection
      latitude_char = as.character(latitude),
      longitude_char = as.character(longitude),
      lat_type = sapply(latitude_char, detect_type),
      lon_type = sapply(longitude_char, detect_type),
      lat_dd = NA_real_,
      lon_dd = NA_real_
    )
  
  # Step 1: Decimal/DMS conversion
  df <- df %>%
    mutate(
      lat_dd = case_when(
        lat_type == "decimal" ~ suppressWarnings(as.numeric(latitude_char)),
        lat_type == "dms" ~ sapply(latitude_char, dms_to_dd),
        TRUE ~ lat_dd
      ),
      lon_dd = case_when(
        lon_type == "decimal" ~ suppressWarnings(as.numeric(longitude_char)),
        lon_type == "dms" ~ sapply(longitude_char, dms_to_dd),
        TRUE ~ lon_dd
      )
    )
  
  # Step 2: Projected coordinates (UTM or Albers)
  proj_rows <- which(df$lat_type == "projected" & df$lon_type == "projected")
  
  if (length(proj_rows) > 0) {
    proj_df <- df[proj_rows, ] %>%
      mutate(
        latitude = suppressWarnings(as.numeric(latitude_char)),
        longitude = suppressWarnings(as.numeric(longitude_char)),
        # Assume smaller is Easting and larger is Northing for UTM/Albers
        easting = pmin(latitude, longitude),
        northing = pmax(latitude, longitude)
      )
    
    sf_dd <- NULL
    
    # --- Try UTM Zone 11N first ---
    try({
      sf_utm <- st_as_sf(proj_df, coords = c("easting", "northing"), crs = 26911)
      sf_dd_utm <- st_transform(sf_utm, 4326)
      if (all(!st_is_empty(sf_dd_utm))) sf_dd <- sf_dd_utm
    }, silent = TRUE)
    
    # --- Fallback to BC Albers if UTM fails ---
    if (is.null(sf_dd)) {
      try({
        sf_alb <- st_as_sf(proj_df, coords = c("easting", "northing"), crs = 3005)
        sf_dd_alb <- st_transform(sf_alb, 4326)
        if (all(!st_is_empty(sf_dd_alb))) sf_dd <- sf_dd_alb
      }, silent = TRUE)
    }
    
    # --- Join converted coordinates back ---
    if (!is.null(sf_dd)) {
      coords <- st_coordinates(sf_dd)
      converted <- data.frame(
        row_id = proj_df$row_id,
        lon_dd_proj = coords[,1],
        lat_dd_proj = coords[,2]
      )
      
      df <- df %>%
        left_join(converted, by = "row_id") %>%
        mutate(
          # Use projected coordinates if conversion was successful
          lat_dd = coalesce(lat_dd_proj, lat_dd),
          lon_dd = coalesce(lon_dd_proj, lon_dd)
        ) %>%
        select(-lat_dd_proj, -lon_dd_proj)
    }
  }
  
  # Final output cleanup
  df %>%
    mutate(latitude = lat_dd, longitude = lon_dd) %>%
    select(-lat_dd, -lon_dd, -lat_type, -lon_type, -row_id, -latitude_char, -longitude_char)
}

# --------------------------------------------------
# Loop through sheets
# --------------------------------------------------
all_sheets <- lapply(sheets, function(sh) {
  message("Processing sheet: ", sh)
  # Read all columns as text to preserve coordinate strings
  df <- read.xlsx(input_file, sheet = sh, detectDates = FALSE) %>% 
    clean_names()
  
  convert_coords(df)
})
names(all_sheets) <- sheets

# --------------------------------------------------
# Save all sheets to new Excel
# --------------------------------------------------
write.xlsx(all_sheets, output_file, overwrite = TRUE)
message("All sheets converted and saved to: ", output_file)
