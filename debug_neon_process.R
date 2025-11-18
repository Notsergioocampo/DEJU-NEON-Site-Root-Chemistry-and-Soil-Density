#!/usr/bin/env Rscript

# Debug the entire process_neon_data function
source("R/data_processing.R")

message("Debugging process_neon_data function...")

# Test with minimal parameters
data_dir <- "data/raw_data"
site_id <- "DEJU"
config <- list(
  depth_breaks = c(0, 10, 30, 60, 100, 200),
  root_size_classes = list(fine = "≤4mm", coarse = ">4mm")
)

message("Step 1: Checking file existence...")
expected_files <- c(
  root_chemistry    = "megapit_carbon_nitrogen.csv",
  root_samples      = "megapit_root_samples.csv",
  soil_bulk_density = "soil_bulk_density.csv",
  soil_chemistry    = "soil_chemistry.csv",
  biomass           = "megapit_biomass.csv"
)

file_paths <- file.path(data_dir, expected_files)
available_files <- file.exists(file_paths)
names(available_files) <- names(expected_files)

message("Available files:")
for (file in names(available_files)) {
  message(sprintf("  %s: %s", file, if (available_files[file]) "YES" else "NO"))
}

message("\nStep 2: Testing individual processing functions...")

# Test root chemistry
tryCatch({
  message("Processing root chemistry...")
  root_chemistry <- process_root_chemistry(
    file.path(data_dir, expected_files["root_chemistry"]),
    site_id = site_id,
    config = config,
    validate = FALSE
  )
  message(sprintf("Root chemistry: %d rows", nrow(root_chemistry)))
}, error = function(e) {
  message(sprintf("Error in root chemistry: %s", e$message))
})

# Test soil bulk density
tryCatch({
  message("Processing soil bulk density...")
  soil_bulk_density <- process_soil_bulk_density(
    file.path(data_dir, expected_files["soil_bulk_density"]),
    site_id = site_id,
    config = config,
    validate = FALSE
  )
  message(sprintf("Soil bulk density: %d rows", nrow(soil_bulk_density)))
}, error = function(e) {
  message(sprintf("Error in soil bulk density: %s", e$message))
})

message("\nStep 3: Testing merge function...")
if (exists("root_chemistry") && exists("soil_bulk_density")) {
  tryCatch({
    merged_data <- merge_root_soil_data(root_chemistry, soil_bulk_density, site_id, config)
    message(sprintf("Merged data: %d rows", nrow(merged_data)))
  }, error = function(e) {
    message(sprintf("Error in merge: %s", e$message))
  })
}

message("\nDebug complete.")
