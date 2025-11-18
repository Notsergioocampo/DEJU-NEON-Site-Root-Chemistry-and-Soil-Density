#!/usr/bin/env Rscript

# Debug script to identify the join issue
source("R/data_processing.R")

message("Testing individual components...")

# Test 1: Load and process root chemistry
message("\n1. Testing root chemistry processing...")
tryCatch({
  root_data <- process_root_chemistry("data/raw_data/megapit_carbon_nitrogen.csv", "DEJU", list(depth_breaks = c(0, 10, 30, 60, 100, 200)), validate = FALSE)
  message(sprintf("Root chemistry: %d rows, columns: %s", nrow(root_data), paste(names(root_data), collapse = ", ")))
}, error = function(e) {
  message(sprintf("Error in root chemistry: %s", e$message))
})

# Test 2: Load and process soil data
message("\n2. Testing soil bulk density processing...")
tryCatch({
  soil_data <- process_soil_bulk_density("data/raw_data/soil_bulk_density.csv", "DEJU", list(depth_breaks = c(0, 10, 30, 60, 100, 200)), validate = FALSE)
  message(sprintf("Soil bulk density: %d rows, columns: %s", nrow(soil_data), paste(names(soil_data), collapse = ", ")))
}, error = function(e) {
  message(sprintf("Error in soil processing: %s", e$message))
})

# Test 3: Test merge function
message("\n3. Testing merge function...")
if (exists("root_data") && exists("soil_data")) {
  tryCatch({
    message("Checking data structures before merge...")
    message(sprintf("Root data depth range: %s", paste(range(root_data$depth_cm, na.rm = TRUE), collapse = " to ")))
    message(sprintf("Soil data depth range: %s", paste(range(soil_data$depth_cm, na.rm = TRUE), collapse = " to ")))
    
    merged_data <- merge_root_soil_data(root_data, soil_data, "DEJU", list(depth_breaks = c(0, 10, 30, 60, 100, 200)))
    message(sprintf("Merged data: %d rows", nrow(merged_data)))
    if (nrow(merged_data) > 0) {
      message(sprintf("Merged data columns: %s", paste(names(merged_data), collapse = ", ")))
    }
  }, error = function(e) {
    message(sprintf("Error in merge: %s", e$message))
    message("Full error traceback:")
    print(traceback())
  })
}

message("\nDebug complete.")
