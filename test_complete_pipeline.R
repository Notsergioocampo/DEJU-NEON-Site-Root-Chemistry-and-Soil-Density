#!/usr/bin/env Rscript

# Test the complete pipeline with detailed error reporting
source("R/data_processing.R")
source("R/analysis_models.R")
source("R/main_analysis.R")

message("Testing complete pipeline...")

tryCatch({
  results <- run_deju_pipeline(
    site_id = "DEJU",
    data_dir = "data/raw_data",
    output_dir = "data_processed",
    figures_dir = "figures",
    download_data = FALSE,
    run_models = TRUE,
    create_figures = TRUE,
    render_report = FALSE
  )
  
  message("Pipeline completed successfully!")
  message(sprintf("Processed %d root chemistry samples", nrow(results$data$root_chemistry)))
  if (!is.null(results$data$soil_bulk_density)) {
    message(sprintf("Processed %d soil bulk density samples", nrow(results$data$soil_bulk_density)))
  }
  if (!is.null(results$data$merged_data)) {
    message(sprintf("Merged %d observations", nrow(results$data$merged_data)))
  }
  
}, error = function(e) {
  message(sprintf("Pipeline failed with error: %s", e$message))
  message("Error traceback:")
  print(traceback())
  message("Attempting to continue with basic processing...")
  
  # Try basic processing without merge
  tryCatch({
    data_list <- process_neon_data("data/raw_data", "DEJU", validate = FALSE)
    
    if (!is.null(data_list$root_chemistry)) {
      readr::write_csv(data_list$root_chemistry, "data_processed/root_chemistry_DEJU.csv")
      message("Saved root chemistry data")
    }
    if (!is.null(data_list$soil_bulk_density)) {
      readr::write_csv(data_list$soil_bulk_density, "data_processed/soil_bulk_density_DEJU.csv")
      message("Saved soil bulk density data")
    }
    
    message("Basic processing completed successfully!")
    
  }, error = function(e2) {
    message(sprintf("Basic processing also failed: %s", e2$message))
  })
})

message("Test complete.")
