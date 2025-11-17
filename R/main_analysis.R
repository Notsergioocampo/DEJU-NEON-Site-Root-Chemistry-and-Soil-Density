#' Main Analysis Pipeline for NEON Root-Soil Analysis
#'
#' This module provides the high-level API for running complete NEON root-soil
#' analyses across multiple sites with statistical modeling.
#'
#' @name main_analysis
#' @docType package
NULL

#' Run complete NEON root-soil analysis pipeline
#'
#' Executes the complete analysis workflow for a specified NEON site,
#' including data processing, statistical modeling, and visualization.
#'
#' @param site_id Character string specifying NEON site ID (e.g., "DEJU", "HARV")
#' @param data_dir Character string specifying data directory
#' @param output_dir Character string specifying output directory
#' @param download_data Logical indicating whether to download data
#' @param run_models Logical indicating whether to run statistical models
#' @param create_figures Logical indicating whether to create figures
#' @param render_report Logical indicating whether to render final report
#' @param overwrite Logical indicating whether to overwrite existing files
#' @param config List containing site-specific configuration (optional)
#'
#' @return List containing all analysis results and outputs
#' @export
run_deju_pipeline <- function(site_id = "DEJU",
                              data_dir = "data/raw_data",
                              output_dir = "output",
                              download_data = FALSE,
                              run_models = TRUE,
                              create_figures = TRUE,
                              render_report = TRUE,
                              overwrite = FALSE,
                              config = NULL) {
  
  message(sprintf("Starting NEON root-soil analysis pipeline for site: %s", site_id))
  
  # Record start time
  start_time <- Sys.time()
  
  # Load site configuration
  if (is.null(config)) {
    config <- load_site_config(site_id)
  }
  
  # Set up directories
  setup_directories(data_dir, output_dir)
  
  # Step 1: Data processing
  message("\nStep 1: Processing NEON data...")
  processed_data <- process_neon_data(
    data_dir = data_dir,
    site_id = site_id,
    config = config,
    validate = TRUE
  )
  
  # Check if critical data is available
  if (is.null(processed_data$root_chemistry) || is.null(processed_data$soil_bulk_density)) {
    stop(sprintf("Insufficient data for analysis at site %s", site_id))
  }
  
  # Step 2: Statistical analysis and modeling
  analysis_results <- list()
  
  if (run_models) {
    message("\nStep 2: Performing statistical analysis...")
    
    # Basic descriptive statistics
    analysis_results$descriptive_stats <- create_comprehensive_summary(processed_data, site_id)
    
    # Root size class comparisons
    if ("sizeCategory" %in% names(processed_data$root_chemistry)) {
      message("  Analyzing root size class differences...")
      analysis_results$size_class_analysis <- analyze_root_size_classes(
        processed_data$root_chemistry,
        site_id,
        config
      )
    }
    
    # Depth-stratified analysis
    message("  Performing depth-stratified analysis...")
    analysis_results$depth_analysis <- analyze_depth_stratified(
      processed_data$root_chemistry,
      site_id = site_id,
      config = config
    )
    
    # Soil-root relationship models (if merged data available)
    if (!is.null(processed_data$merged_data)) {
      message("  Fitting soil-root relationship models...")
      analysis_results$soil_root_models <- fit_soil_root_models(
        processed_data$merged_data,
        site_id = site_id
      )
    }
  }
  
  # Step 3: Visualization
  figure_paths <- list()
  
  if (create_figures) {
    message("\nStep 3: Creating publication-quality figures...")
    figure_paths <- create_publication_figures(
      processed_data,
      site_id = site_id,
      output_dir = file.path(output_dir, "figures"),
      dpi = 300,
      width = 8,
      height = 6
    )
  }
  
  # Step 4: Summary tables and reports
  message("\nStep 4: Generating summary outputs...")
  
  # Create summary tables
  summary_tables <- create_analysis_summary_tables(
    analysis_results,
    processed_data,
    site_id
  )
  
  # Save summary tables
  save_summary_tables(summary_tables, output_dir)
  
  # Step 5: Final report (if Pandoc available)
  if (render_report) {
    message("\nStep 5: Rendering final report...")
    render_analysis_report(site_id, output_dir)
  }
  
  # Record completion time
  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  message(sprintf("\n✓ Analysis pipeline completed successfully in %.1f minutes", duration))
  
  # Compile final results
  final_results <- list(
    site_id = site_id,
    config = config,
    processed_data = processed_data,
    analysis_results = analysis_results,
    figure_paths = figure_paths,
    summary_tables = summary_tables,
    processing_time = duration,
    timestamp = end_time
  )
  
  return(final_results)
}

#' Load processed data for a specific site
#'
#' Loads previously processed data for analysis or visualization.
#'
#' @param site_id Character string specifying NEON site ID
#' @param data_dir Character string specifying data directory
#' @return List containing processed data
#' @export
load_processed_data <- function(site_id = "DEJU", data_dir = "data/processed") {
  
  message(sprintf("Loading processed data for site: %s", site_id))
  
  # Define expected file names
  expected_files <- c(
    root_chemistry = "root_chemistry_clean.csv",
    soil_bulk_density = "soil_bulk_density_clean.csv",
    merged_data = "merged_root_soil_data.csv"
  )
  
  file_paths <- file.path(data_dir, expected_files)
  available_files <- file.exists(file_paths)
  
  if (!any(available_files)) {
    warning(sprintf("No processed data found for site %s", site_id))
    return(NULL)
  }
  
  processed_data <- list()
  
  # Load available datasets
  if (available_files["root_chemistry"]) {
    processed_data$root_chemistry <- readr::read_csv(file_paths["root_chemistry"], show_col_types = FALSE)
    message(sprintf("  Loaded %d root chemistry samples", nrow(processed_data$root_chemistry)))
  }
  
  if (available_files["soil_bulk_density"]) {
    processed_data$soil_bulk_density <- readr::read_csv(file_paths["soil_bulk_density"], show_col_types = FALSE)
    message(sprintf("  Loaded %d soil bulk density samples", nrow(processed_data$soil_bulk_density)))
  }
  
  if (available_files["merged_data"]) {
    processed_data$merged_data <- readr::read_csv(file_paths["merged_data"], show_col_types = FALSE)
    message(sprintf("  Loaded %d merged observations", nrow(processed_data$merged_data)))
  }
  
  return(processed_data)
}

#' Analyze root size class differences
#'
#' Performs statistical analysis comparing root chemistry
#' between fine and coarse root size classes.
#'
#' @param root_data Data frame containing root chemistry data
#' @param site_id Character string specifying site ID
#' @param config List containing site configuration
#'
#' @return List containing analysis results
#' @export
analyze_root_size_classes <- function(root_data, site_id, config) {
  
  message(sprintf("  Analyzing root size class differences for site %s", site_id))
  
  # Check if size category data is available
  if (!"sizeCategory" %in% names(root_data)) {
    warning("Size category data not available for analysis")
    return(NULL)
  }
  
  # Filter for valid size categories
  size_data <- root_data %>%
    filter(!is.na(sizeCategory), sizeCategory %in% c("Fine (≤4mm)", "Coarse (>4mm)"))
  
  if (nrow(size_data) < 10) {
    warning("Insufficient data for size class analysis")
    return(NULL)
  }
  
  # Variables to analyze
  variables <- c("carbonPercent", "nitrogenPercent", "cn_ratio")
  
  # Perform statistical tests
  test_results <- perform_size_class_tests(size_data, variables)
  
  # Calculate effect sizes
  effect_sizes <- calculate_size_class_effects(size_data, variables)
  
  # Create summary statistics
  summary_stats <- size_data %>%
    group_by(sizeCategory) %>%
    summarise(
      across(all_of(variables), list(
        n = n(),
        mean = mean(., na.rm = TRUE),
        sd = sd(., na.rm = TRUE),
        se = sd(., na.rm = TRUE) / sqrt(n()),
        min = min(., na.rm = TRUE),
        max = max(., na.rm = TRUE)
      ), .groups = "drop")
    ) %>%
    pivot_longer(cols = -sizeCategory, names_to = c("variable", "statistic"), 
                 names_pattern = "(.*)_(.*)", values_to = "value") %>%
    pivot_wider(names_from = statistic, values_from = value)
  
  return(list(
    test_results = test_results,
    effect_sizes = effect_sizes,
    summary_stats = summary_stats,
    site_id = site_id
  ))
}

#' Setup analysis directories
#'
#' Creates necessary directories for analysis outputs.
#'
#' @param data_dir Character string specifying data directory
#' @param output_dir Character string specifying output directory
#' @export
setup_directories <- function(data_dir, output_dir) {
  
  # Create data directories if they don't exist
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
    message(sprintf("Created data directory: %s", data_dir))
  }
  
  # Create output directories
  output_subdirs <- c(
    file.path(output_dir, "figures"),
    file.path(output_dir, "tables"),
    file.path(output_dir, "models")
  )
  
  for (dir in output_subdirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      message(sprintf("Created output directory: %s", dir))
    }
  }
}

#' Create analysis summary tables
#'
#' Generates comprehensive summary tables from analysis results.
#'
#' @param analysis_results List containing analysis results
#' @param processed_data List containing processed data
#' @param site_id Character string specifying site ID
#' @return List containing summary tables
#' @export
create_analysis_summary_tables <- function(analysis_results, processed_data, site_id) {
  
  summary_tables <- list()
  
  # Model summary table
  if (!is.null(analysis_results$soil_root_models)) {
    if (!is.null(analysis_results$soil_root_models$summary)) {
      summary_tables$model_summary <- analysis_results$soil_root_models$summary$fixed_effects
    }
  }
  
  # Depth category summary
  if (!is.null(analysis_results$depth_analysis)) {
    depth_summary <- analysis_results$depth_analysis$depth_stratified %>%
      purrr::map_df(~ .x$summary_stats) %>%
      mutate(site_id = site_id)
    summary_tables$depth_category_summary <- depth_summary
  }
  
  # Size class summary
  if (!is.null(analysis_results$size_class_analysis)) {
    size_summary <- analysis_results$size_class_analysis$summary_stats %>%
      mutate(site_id = site_id)
    summary_tables$root_chemistry_summary <- size_summary
  }
  
  return(summary_tables)
}

#' Save summary tables to files
#'
#' Writes summary tables to CSV files for external use.
#'
#' @param summary_tables List containing summary tables
#' @param output_dir Character string specifying output directory
#' @export
save_summary_tables <- function(summary_tables, output_dir) {
  
  for (table_name in names(summary_tables)) {
    if (!is.null(summary_tables[[table_name]])) {
      file_path <- file.path(output_dir, paste0(table_name, ".csv"))
      readr::write_csv(summary_tables[[table_name]], file_path)
      message(sprintf("  Saved summary table: %s", basename(file_path)))
    }
  }
}

#' Render final analysis report
#'
#' Renders the RMarkdown research paper if Pandoc is available.
#'
#' @param site_id Character string specifying site ID
#' @param output_dir Character string specifying output directory
#' @export
render_analysis_report <- function(site_id, output_dir) {
  
  # Check if Pandoc is available
  if (!check_pandoc_available()) {
    warning("Pandoc not available. Skipping research paper render.")
    return(NULL)
  }
  
  message("  Rendering research paper...")
  
  tryCatch({
    render_research_paper()
    message("  ✓ Research paper rendered successfully")
  }, error = function(e) {
    warning(sprintf("Failed to render research paper: %s", e$message))
  })
}
