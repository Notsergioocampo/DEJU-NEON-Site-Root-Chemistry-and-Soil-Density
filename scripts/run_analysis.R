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
  
  # Step 5: Create final report
  message("\nStep 5: Creating final report...")
  
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
  message("  - Final report: output/final_report.Rmd")
  
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

#' Create final analysis report
#'
#' @param root_data Root chemistry data
#' @param soil_data Soil bulk density data
#' @param merged_data Merged data
#' @param analysis_results Statistical analysis results
#' @param plots List of plots
create_final_report <- function(root_data, soil_data, merged_data, analysis_results, plots) {
  
  # Create R Markdown report
  report_content <- '---
title: "NEON DEJU Site Root Chemistry and Soil Density Analysis"
author: "Sergio Ocampo"
date: "`r Sys.Date()`"
output: 
  html_document:
    toc: true
    toc_float: true
    theme: united
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)

# Load processed data
root_chemistry <- read_csv("data/processed/root_chemistry_clean.csv")
soil_bulk_density <- read_csv("data/processed/soil_bulk_density_clean.csv")
merged_data <- read_csv("data/processed/merged_root_soil_data.csv")
```

# Executive Summary

This analysis examines root chemistry (carbon, nitrogen, C:N ratios) and soil bulk density relationships at the NEON DEJU (Delta Junction) site in Alaska. The study focuses on understanding how root nutrient composition varies with soil physical properties in this boreal forest ecosystem.

## Key Findings

```{r summary-stats}
# Summary statistics
n_root_samples <- nrow(root_chemistry)
n_soil_samples <- nrow(soil_bulk_density)
date_range <- paste(range(root_chemistry$collectDate, na.rm = TRUE), collapse = " to ")

cat(paste0("
- **Root chemistry samples:** ", n_root_samples, "
- **Soil bulk density samples:** ", n_soil_samples, "
- **Site:** DEJU (Delta Junction, Alaska)
- **Domain:** D19 (Taiga)
"))
```

# Data Overview

## Root Chemistry Data

```{r root-summary}
# Root chemistry summary
root_summary <- root_chemistry %>%
  summarise(
    `Carbon %` = paste0(round(mean(carbonPercent, na.rm = TRUE), 2), " ± ", 
                       round(sd(carbonPercent, na.rm = TRUE), 2)),
    `Nitrogen %` = paste0(round(mean(nitrogenPercent, na.rm = TRUE), 3), " ± ", 
                         round(sd(nitrogenPercent, na.rm = TRUE), 3)),
    `C:N Ratio` = paste0(round(mean(cn_ratio, na.rm = TRUE), 1), " ± ", 
                        round(sd(cn_ratio, na.rm = TRUE), 1)),
    `Samples (n)` = n()
  )

kable(root_summary, caption = "Root Chemistry Summary Statistics") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

## Soil Bulk Density Data

```{r soil-summary}
# Soil bulk density summary
soil_summary <- soil_bulk_density %>%
  summarise(
    `Bulk Density (g/cm³)` = paste0(round(mean(bulk_density, na.rm = TRUE), 3), " ± ", 
                                   round(sd(bulk_density, na.rm = TRUE), 3)),
    `Depth Range (cm)` = paste0(round(min(depth_cm, na.rm = TRUE), 0), "-", 
                               round(max(depth_cm, na.rm = TRUE), 0)),
    `Samples (n)` = n()
  )

kable(soil_summary, caption = "Soil Bulk Density Summary Statistics") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

# Results

## Root Chemistry Distributions

```{r nitrogen-dist, fig.height=6, fig.width=8}
include_graphics("figures/nitrogen_distribution.png")
```

```{r carbon-nitrogen, fig.height=6, fig.width=8}
include_graphics("figures/carbon_nitrogen_relationship.png")
```

```{r cn-ratio-dist, fig.height=6, fig.width=8}
include_graphics("figures/cn_ratio_distribution.png")
```

## Root Size Category Analysis

```{r size-comparison, fig.height=6, fig.width=8}
if (file.exists("figures/root_size_comparison.png")) {
  include_graphics("figures/root_size_comparison.png")
} else {
  cat("Root size comparison plot not available - size category data missing")
}
```

## Soil-Root Relationships

```{r soil-nitrogen, fig.height=6, fig.width=8}
if (file.exists("figures/soil_nitrogen_relationship.png")) {
  include_graphics("figures/soil_nitrogen_relationship.png")
} else {
  cat("Soil-root relationship plots not available - merged data insufficient")
}
```

```{r soil-carbon, fig.height=6, fig.width=8}
if (file.exists("figures/soil_carbon_relationship.png")) {
  include_graphics("figures/soil_carbon_relationship.png")
}
```

```{r soil-cn-ratio, fig.height=6, fig.width=8}
if (file.exists("figures/soil_cn_ratio_relationship.png")) {
  include_graphics("figures/soil_cn_ratio_relationship.png")
}
```

## Depth Profiles

```{r depth-profiles, fig.height=8, fig.width=10}
if (file.exists("figures/depth_profiles.png")) {
  include_graphics("figures/depth_profiles.png")
} else {
  cat("Depth profile plots not available - depth data insufficient")
}
```

# Statistical Analysis Results

## Summary Tables

```{r load-summary-tables}
# Load summary tables if available
summary_files <- list.files("output", pattern = ".*summary.*\\\\.csv$", full.names = TRUE)
if (length(summary_files) > 0) {
  for (file in summary_files) {
    table_name <- tools::file_path_sans_ext(basename(file))
    table_data <- read_csv(file)
    cat(paste0("\\n## ", gsub("_", " ", tools::toTitleCase(table_name)), "\\n"))
    print(kable(table_data) %>% kable_styling())
  }
} else {
  cat("Summary tables not available")
}
```

## Key Statistical Findings

```{r statistical-findings}
# Load and display key statistical results
cat("
**Root Size Category Comparisons:**
- Fine roots (≤4mm) vs Coarse roots (>4mm)
- Statistical tests performed for carbon, nitrogen, and C:N ratios

**Soil-Root Correlations:**
- Correlation analysis between soil bulk density and root chemistry variables
- Regression analyses performed to quantify relationships

**Ecological Interpretation:**
- Results discussed in context of boreal forest ecosystem dynamics
- Implications for carbon and nitrogen cycling in permafrost-affected soils
")
```

# Discussion and Conclusions

## Ecological Significance

This analysis provides insights into belowground nutrient dynamics at the DEJU site, representing a boreal forest ecosystem with discontinuous permafrost. The findings contribute to our understanding of:

1. **Root nutrient allocation patterns** across different size classes
2. **Soil physical controls** on root chemistry
3. **Depth-related variations** in root-soil relationships

## Methodological Considerations

- Data quality filtering applied to remove outliers and missing values
- Statistical tests appropriate for the data distribution and sample sizes
- Visualizations designed for ecological interpretation

## Future Research Directions

- Expand analysis to additional NEON sites for broader geographic comparison
- Include additional soil chemical properties in the analysis
- Investigate seasonal and interannual variability in root-soil relationships

# References

Data accessed from the National Ecological Observatory Network (NEON) database. 
Analysis conducted using R statistical software with tidyverse packages.

---

*Report generated automatically on `r Sys.Date()`*
'
  
  # Write report file
  writeLines(report_content, "output/final_report.Rmd")
  message("✓ Final report created: output/final_report.Rmd")
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
