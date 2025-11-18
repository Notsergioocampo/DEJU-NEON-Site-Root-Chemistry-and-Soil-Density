#!/usr/bin/env Rscript

# Debug soil processing specifically
source("R/data_processing.R")

message("Debugging soil bulk density processing...")

# Load raw data
raw_data <- readr::read_csv('data/raw_data/soil_bulk_density.csv', show_col_types = FALSE)
message(sprintf("Raw data: %d rows", nrow(raw_data)))
message(sprintf("Site IDs in data: %s", paste(unique(raw_data$siteID), collapse = ", ")))

# Test site filtering
site_id <- "DEJU"
message(sprintf("Filtering for site: %s", site_id))

tryCatch({
  site_data <- raw_data %>%
    filter(siteID == site_id)
  
  message(sprintf("Filtered data: %d rows", nrow(site_data)))
  message("First few rows of filtered data:")
  print(head(site_data[, c("siteID", "domainID", "pitID", "horizonName")]))
  
  # Test the rest of the processing
  config <- list(depth_breaks = c(0, 10, 30, 60, 100, 200))
  
  processed_data <- site_data %>%
    # Remove samples with missing bulk density
    filter(!is.na(bulkDensExclCoarseFrag)) %>%
    # Use corrected bulk density (excluding coarse fragments) if available
    mutate(
      bulk_density = bulkDensExclCoarseFrag
    ) %>%
    # Create depth categories
    mutate(
      depth_category = create_depth_categories(bulkDensCenterDepth, config$depth_breaks)
    ) %>%
    # Remove extreme outliers
    filter(
      abs(bulk_density - mean(bulk_density, na.rm = TRUE)) <= 
        4 * sd(bulk_density, na.rm = TRUE)
    ) %>%
    # Standardize column names
    select(
      siteID, domainID, pitID, horizonName,
      depth_cm = bulkDensCenterDepth,
      depth_cm_top = bulkDensTopDepth,
      depth_cm_bottom = bulkDensBottomDepth,
      bulk_density,
      depth_category,
      everything()
    )
  
  message(sprintf("Final processed data: %d rows", nrow(processed_data)))
  
}, error = function(e) {
  message(sprintf("Error: %s", e$message))
  message("Full error:")
  print(e)
})
