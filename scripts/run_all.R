#!/usr/bin/env Rscript

cat("Loading R functions...\n")

# Source all required functions
source("R/data_processing.R")
source("R/analysis_models.R")
source("R/visualization.R")
source("R/main_analysis.R")

cat("\n=== NEON DEJU Root Chemistry Analysis ===\n\n")

tryCatch({
  run_deju_pipeline(
    site_id      = "DEJU",
    data_dir     = "data/raw_data",
    output_dir   = "data_processed",
    figures_dir  = "figures",
    download_data = FALSE,   # if you add neonUtilities hooks later
    run_models    = TRUE,
    create_figures = TRUE,
    render_report  = FALSE
  )
}, error = function(e) {
  cat("\n❌ Error during analysis:\n", e$message, "\n\n")
  cat("Please check your data files and try again.\n")
  quit(status = 1)
})
