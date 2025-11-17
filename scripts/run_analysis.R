#!/usr/bin/env Rscript

#' Main Analysis Script for NEON DEJU Root Chemistry and Soil Density
#'
#' This script runs the complete analysis pipeline for NEON DEJU site data,
#' including data processing, statistical analysis, and visualization.
#'
#' @author Sergio Ocampo
#' @date November 2025

# Load required packages
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(stringr)
  library(broom)
  library(ggthemes)
})

# Source all R functions
message("Loading R functions...")
source("R/data_processing.R")
source("R/visualization.R")
source("R/analysis.R")

# Set up output directories
output_dirs <- c("data/processed", "figures", "output")
for (dir in output_dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}

# Function to ensure Pandoc is available
ensure_pandoc <- function() {
  # Check if pandoc is available
  pandoc_available <- tryCatch({
    rmarkdown::find_pandoc()
    TRUE
  }, error = function(e) FALSE)
  
  if (!pandoc_available) {
    message("Pandoc not found. Attempting to install...")
    
    # Try to install pandoc via R
    tryCatch({
      if (requireNamespace("installr", quietly = TRUE)) {
        installr::install.pandoc()
      } else {
        # Try alternative installation methods
        if (Sys.info()["sysname"] == "Darwin") {
          system("brew install pandoc")
        } else if (Sys.info()["sysname"] == "Linux") {
          system("sudo apt-get install pandoc")
        } else if (Sys.info()["sysname"] == "Windows") {
          message("Please install Pandoc manually from: https://pandoc.org/installing.html")
        }
      }
      
      # Check again after installation attempt
      Sys.sleep(5)  # Wait for installation to complete
      pandoc_available <- tryCatch({
        rmarkdown::find_pandoc()
        TRUE
      }, error = function(e) FALSE)
    }, error = function(e) {
      message("Automatic Pandoc installation failed.")
      FALSE
    })
  }
  
  if (!pandoc_available) {
    stop(paste(
      "Pandoc is required but not available. Please install Pandoc manually:\n",
      "• macOS: brew install pandoc\n",
      "• Ubuntu/Debian: sudo apt-get install pandoc\n", 
      "• Windows: Download from https://pandoc.org/installing.html\n",
      "• Or install R package: install.packages('installr'); installr::install.pandoc()"
    ))
  }
  
  message("✓ Pandoc is available")
  return(TRUE)
}

# Main analysis function
run_complete_analysis <- function() {
  message("\n=== NEON DEJU Root Chemistry Analysis ===\n")
  
  # Step 1: Load and process data
  message("Step 1: Processing NEON data...")
  processed_data <- process_neon_data("data/raw_data")
  
  # Extract processed datasets
  root_chemistry <- processed_data$root_chemistry
  soil_bulk_density <- processed_data$soil_bulk_density
  
  # Save processed data
  write_csv(root_chemistry, "data/processed/root_chemistry_clean.csv")
  write_csv(soil_bulk_density, "data/processed/soil_bulk_density_clean.csv")
  message("✓ Processed data saved to data/processed/")
  
  # Step 2: Merge datasets for combined analysis
  message("\nStep 2: Merging root and soil data...")
  
  # Extract depth information from root sample IDs if not already present
  if (!"depth_cm" %in% names(root_chemistry)) {
    if ("cnSampleID" %in% names(root_chemistry)) {
      depth_info <- extract_depth_info(root_chemistry$cnSampleID)
      root_chemistry <- root_chemistry %>%
        left_join(depth_info, by = c("cnSampleID" = "sample_id"))
    }
  }
  
  # Merge on siteID - use approximate depth matching since depths may not align perfectly
  merged_data <- root_chemistry %>%
    left_join(
      soil_bulk_density %>% 
        select(siteID, depth_cm, bulk_density, horizonName),
      by = "siteID"
    )
  
  # Add depth categories if not already present
  if (!"depth_category" %in% names(merged_data) && "depth_cm" %in% names(merged_data)) {
    merged_data <- merged_data %>%
      mutate(depth_category = create_depth_categories(depth_cm))
  }
  
  write_csv(merged_data, "data/processed/merged_root_soil_data.csv")
  message("✓ Merged data saved to data/processed/")
  
  # Step 3: Generate visualizations
  message("\nStep 3: Creating visualizations...")
  
  # Create individual plots
  plots <- list(
    nitrogen_distribution = plot_nitrogen_distribution(root_chemistry),
    carbon_nitrogen_relationship = plot_carbon_nitrogen_relationship(root_chemistry),
    cn_ratio_distribution = plot_cn_ratio_distribution(root_chemistry)
  )
  
  # Add size comparison if size data available
  if ("sizeCategory" %in% names(root_chemistry)) {
    plots$root_size_comparison <- plot_root_size_comparison(root_chemistry)
  }
  
  # Add soil-root relationship plots if merged data available
  if (nrow(merged_data) > 0 && all(c("bulk_density", "nitrogenPercent") %in% names(merged_data))) {
    plots$soil_nitrogen_relationship <- plot_soil_root_relationship(merged_data, "nitrogenPercent")
    plots$soil_carbon_relationship <- plot_soil_root_relationship(merged_data, "carbonPercent")
    plots$soil_cn_ratio_relationship <- plot_soil_root_relationship(merged_data, "cn_ratio")
  }
  
  # Add depth profile if depth data available
  if (all(c("depth_cm", "cn_ratio") %in% names(root_chemistry)) && 
      all(c("depth_cm", "bulk_density") %in% names(soil_bulk_density))) {
    plots$depth_profiles <- plot_depth_profiles(root_chemistry, soil_bulk_density)
  }
  
  # Save plots
  save_plots(plots, output_dir = "figures")
  message("✓ All plots saved to figures/")
  
  # Step 4: Perform statistical analyses
  message("\nStep 4: Performing statistical analyses...")
  
  analysis_results <- perform_comprehensive_analysis(
    root_chemistry,
    soil_bulk_density,
    merged_data
  )
  
  # Print results to console
  print_analysis_results(analysis_results)
  
  # Save summary tables
  for (table_name in names(analysis_results$summary_tables)) {
    if (!is.null(analysis_results$summary_tables[[table_name]])) {
      write_csv(
        analysis_results$summary_tables[[table_name]],
        paste0("output/", table_name, ".csv")
      )
    }
  }
  
  message("\n✓ Summary tables saved to output/")
  
  # Step 5: Create final research paper
  message("\nStep 5: Creating final research paper...")
  
  create_final_report(
    root_chemistry,
    soil_bulk_density,
    merged_data,
    analysis_results,
    plots
  )
  
  message("\n=== Analysis Complete! ===\n")
  message("Results saved to:")
  message("  - Processed data: data/processed/")
  message("  - Figures: figures/")
  message("  - Summary tables: output/")
  message("  - Research paper: output/project_report.html")
  message("  - Research paper: output/project_report.docx")
  if (file.exists("output/project_report.pdf")) {
    message("  - Research paper: output/project_report.pdf")
  }
  
  return(list(
    data = list(
      root_chemistry = root_chemistry,
      soil_bulk_density = soil_bulk_density,
      merged_data = merged_data
    ),
    plots = plots,
    analysis_results = analysis_results
  ))
}

#' Create final research paper
#'
#' @param root_data Root chemistry data
#' @param soil_data Soil bulk density data
#' @param merged_data Merged data
#' @param analysis_results Statistical analysis results
#' @param plots List of plots
create_final_report <- function(root_data, soil_data, merged_data, analysis_results, plots) {
  
  message("Creating research paper...")
  
  # Ensure Pandoc is available
  ensure_pandoc()
  
  # Copy the research paper template to output directory
  if (file.exists("output/research_paper.Rmd")) {
    file.copy("output/research_paper.Rmd", "output/project_report.Rmd", overwrite = TRUE)
  } else {
    stop("Research paper template not found at output/research_paper.Rmd")
  }
  
  # Copy references file if it exists
  if (file.exists("output/references.bib")) {
    file.copy("output/references.bib", "output/references_used.bib", overwrite = TRUE)
  }
  
  # Copy ecology CSL file if it exists
  if (file.exists("ecology.csl")) {
    file.copy("ecology.csl", "output/ecology.csl", overwrite = TRUE)
  }
  
  # Render the research paper in multiple formats
  message("Rendering research paper...")
  
  tryCatch({
    # HTML version
    message("  - Generating HTML version...")
    rmarkdown::render(
      input = "output/project_report.Rmd",
      output_format = "html_document",
      output_file = "project_report.html",
      output_dir = "output",
      clean = TRUE
    )
    
    # Word version
    message("  - Generating Word version...")
    rmarkdown::render(
      input = "output/project_report.Rmd",
      output_format = "word_document",
      output_file = "project_report.docx",
      output_dir = "output",
      clean = TRUE
    )
    
    # Try PDF version (may fail if LaTeX not available)
    message("  - Attempting PDF version...")
    try({
      rmarkdown::render(
        input = "output/project_report.Rmd",
        output_format = "pdf_document",
        output_file = "project_report.pdf",
        output_dir = "output",
        clean = TRUE
      )
      message("  ✓ PDF version created successfully")
    }, silent = TRUE)
    
    message("✓ Research paper rendered in multiple formats")
    
  }, error = function(e) {
    warning("Failed to render research paper: ", e$message)
    warning("Creating fallback summary report...")
    
    # Create a simple fallback report
    create_fallback_report(root_data, soil_data, merged_data, analysis_results, plots)
  })
}

#' Create fallback report if main rendering fails
#'
#' @param root_data Root chemistry data
#' @param soil_data Soil bulk density data
#' @param merged_data Merged data
#' @param analysis_results Statistical analysis results
#' @param plots List of plots
create_fallback_report <- function(root_data, soil_data, merged_data, analysis_results, plots) {
  
  message("Creating fallback HTML report...")
  
  fallback_content <- paste0('
<!DOCTYPE html>
<html>
<head>
  <title>NEON DEJU Site Analysis Results</title>
  <style>
    body { font-family: Arial, sans-serif; margin: 40px; line-height: 1.6; }
    h1 { color: #2c5530; border-bottom: 2px solid #4CAF50; }
    h2 { color: #4CAF50; }
    .figure { text-align: center; margin: 20px 0; }
    .stats { background: #f0f8f0; padding: 15px; border-radius: 5px; }
    table { border-collapse: collapse; width: 100%; margin: 20px 0; }
    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
    th { background-color: #4CAF50; color: white; }
  </style>
</head>
<body>
  <h1>NEON DEJU Site Root Chemistry and Soil Density Analysis</h1>
  
  <div class="stats">
    <h3>Key Statistics</h3>
    <ul>
      <li>Root chemistry samples: ', nrow(root_data), '</li>
      <li>Soil bulk density samples: ', nrow(soil_data), '</li>
      <li>Mean root carbon: ', round(mean(root_data$carbonPercent, na.rm = TRUE), 1), '%</li>
      <li>Mean root nitrogen: ', round(mean(root_data$nitrogenPercent, na.rm = TRUE), 2), '%</li>
      <li>Mean C:N ratio: ', round(mean(root_data$cn_ratio, na.rm = TRUE), 1), '</li>
    </ul>
  </div>
  
  <h2>Generated Figures</h2>
  <p>The following figures have been generated and are available in the figures/ directory:</p>
  <ul>
    <li>nitrogen_distribution.png - Distribution of root nitrogen content</li>
    <li>carbon_nitrogen_relationship.png - Carbon vs nitrogen scatter plot</li>
    <li>cn_ratio_distribution.png - C:N ratio distribution</li>
    <li>root_size_comparison.png - Comparison by root size category</li>
    <li>soil_nitrogen_relationship.png - Soil vs nitrogen relationship</li>
    <li>soil_carbon_relationship.png - Soil vs carbon relationship</li>
    <li>soil_cn_ratio_relationship.png - Soil vs C:N ratio relationship</li>
    <li>depth_profiles.png - Vertical distribution patterns</li>
  </ul>
  
  <h2>Summary Tables</h2>
  <p>Statistical summary tables are available in the output/ directory as CSV files.</p>
  
  <h2>Analysis Complete</h2>
  <p>The analysis pipeline has completed successfully. For a full research paper, please ensure Pandoc is installed and run the analysis again.</p>
  
  <p><em>Generated on: ', Sys.Date(), '</em></p>
</body>
</html>
  ')
  
  writeLines(fallback_content, "output/project_report.html")
  message("✓ Fallback HTML report created")
}

# Run the analysis if this script is executed directly
if (sys.nframe() == 0) {
  tryCatch({
    results <- run_complete_analysis()
    message("\n🎉 Analysis completed successfully!")
  }, error = function(e) {
    message("\n❌ Error during analysis:")
    message(e$message)
    message("\nPlease check your data files and try again.")
    quit(status = 1)
  })
}
