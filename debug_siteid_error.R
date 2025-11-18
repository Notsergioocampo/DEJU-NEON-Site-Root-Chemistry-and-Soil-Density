#!/usr/bin/env Rscript

# Debug the exact siteID == site_id error location
source("R/data_processing.R")

message("Debugging siteID == site_id error...")

# Test each processing function individually with detailed error tracking
data_dir <- "data/raw_data"
site_id <- "DEJU"
config <- list(
  depth_breaks = c(0, 10, 30, 60, 100, 200),
  root_size_classes = list(fine = "≤4mm", coarse = ">4mm")
)

message("\n1. Testing process_root_chemistry...")
tryCatch({
  root_data <- process_root_chemistry(
    file.path(data_dir, "megapit_carbon_nitrogen.csv"),
    site_id = site_id,
    config = config,
    validate = FALSE
  )
  message(sprintf("✓ Root chemistry: %d rows", nrow(root_data)))
}, error = function(e) {
  message(sprintf("✗ Root chemistry error: %s", e$message))
  print(e)
})

message("\n2. Testing process_soil_bulk_density...")
tryCatch({
  soil_data <- process_soil_bulk_density(
    file.path(data_dir, "soil_bulk_density.csv"),
    site_id = site_id,
    config = config,
    validate = FALSE
  )
  message(sprintf("✓ Soil bulk density: %d rows", nrow(soil_data)))
}, error = function(e) {
  message(sprintf("✗ Soil bulk density error: %s", e$message))
  print(e)
})

message("\n3. Testing process_neon_data...")
tryCatch({
  data_list <- process_neon_data(
    data_dir = data_dir,
    site_id = site_id,
    config = config,
    validate = FALSE
  )
  message(sprintf("✓ Processed data list with %d items", length(data_list)))
  for (name in names(data_list)) {
    if (!is.null(data_list[[name]])) {
      message(sprintf("  - %s: %d rows", name, nrow(data_list[[name]])))
    }
  }
}, error = function(e) {
  message(sprintf("✗ process_neon_data error: %s", e$message))
  print(e)
})

message("\nDebug complete.")
