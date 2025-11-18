#!/usr/bin/env Rscript

# Debug the merge function specifically
source("R/data_processing.R")

message("Debugging merge_root_soil_data function...")

# Load and process both datasets
data_dir <- "data/raw_data"
site_id <- "DEJU"
config <- list(
  depth_breaks = c(0, 10, 30, 60, 100, 200),
  root_size_classes = list(fine = "≤4mm", coarse = ">4mm")
)

message("Loading and processing data...")
root_chemistry <- process_root_chemistry(
  file.path(data_dir, "megapit_carbon_nitrogen.csv"),
  site_id = site_id,
  config = config,
  validate = FALSE
)

soil_bulk_density <- process_soil_bulk_density(
  file.path(data_dir, "soil_bulk_density.csv"),
  site_id = site_id,
  config = config,
  validate = FALSE
)

message(sprintf("Root chemistry: %d rows", nrow(root_chemistry)))
message(sprintf("Soil bulk density: %d rows", nrow(soil_bulk_density)))

message("\nChecking required columns...")
message(sprintf("Root data has depth_cm: %s", "depth_cm" %in% names(root_chemistry)))
message(sprintf("Soil data has depth_cm: %s", "depth_cm" %in% names(soil_bulk_density)))
message(sprintf("Soil data has bulk_density: %s", "bulk_density" %in% names(soil_bulk_density)))
message(sprintf("Soil data has horizonName: %s", "horizonName" %in% names(soil_bulk_density)))

message("\nChecking depth ranges...")
message(sprintf("Root depth range: %s", paste(range(root_chemistry$depth_cm, na.rm = TRUE), collapse = " to ")))
message(sprintf("Soil depth range: %s", paste(range(soil_bulk_density$depth_cm, na.rm = TRUE), collapse = " to ")))

message("\nTesting merge step by step...")

# Test the rowwise operation
tryCatch({
  message("Testing rowwise operation...")
  
  # Create local copy to avoid variable shadowing in rowwise context
  soil_df <- soil_bulk_density
  
  test_result <- root_chemistry %>%
    slice(1:3) %>%  # Just test first 3 rows
    rowwise() %>%
    mutate(
      closest_soil_idx = {
        diffs <- abs(soil_df$depth_cm - depth_cm)
        which.min(diffs)
      },
      soil_bulk_density = soil_df$bulk_density[closest_soil_idx],
      soil_depth_cm = soil_df$depth_cm[closest_soil_idx],
      soil_depth_category = soil_df$depth_category[closest_soil_idx],
      soil_horizon = soil_df$horizonName[closest_soil_idx]
    ) %>%
    ungroup()
  
  message("Rowwise test successful!")
  print(head(test_result[, c("cnSampleID", "depth_cm", "soil_bulk_density", "soil_depth_cm")]))
  
}, error = function(e) {
  message(sprintf("Error in rowwise operation: %s", e$message))
  message("Full error:")
  print(e)
})

message("\nTesting complete merge...")
tryCatch({
  merged_data <- merge_root_soil_data(root_chemistry, soil_bulk_density, site_id, config)
  message(sprintf("Merge successful! %d rows", nrow(merged_data)))
  message("First few rows of merged data:")
  print(head(merged_data[, c("cnSampleID", "depth_cm", "bulk_density", "carbonPercent", "nitrogenPercent")]))
  
}, error = function(e) {
  message(sprintf("Error in complete merge: %s", e$message))
  message("Full error:")
  print(e)
})

message("\nDebug complete.")
