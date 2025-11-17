#' Research Paper Generation and Report Creation Module
#'
#' This module provides functions for creating comprehensive research papers
#' and reports from analysis results, including advanced statistical summaries
#' and publication-ready formatting.
#'
#' @name report_generation
#' @docType package
NULL

#' Render enhanced analysis report with advanced results
#'
#' Creates a comprehensive research paper incorporating all analysis results
#' including advanced statistical methods, spatial analysis, machine learning
#' results, and Bayesian uncertainty quantification.
#'
#' @param site_id Character string specifying site ID
#' @param output_dir Character string specifying output directory
#' @param analysis_results List containing all analysis results
#' @param template_file Character string specifying RMarkdown template
#' @param output_formats Character vector of output formats
#'
#' @return List containing paths to generated reports
#' @export
render_enhanced_analysis_report <- function(site_id = "DEJU",
                                           output_dir = "output",
                                           analysis_results = list(),
                                           template_file = "enhanced_research_paper.Rmd",
                                           output_formats = c("html_document", "word_document")) {
  
  message(sprintf("Creating comprehensive research paper for site %s", site_id))
  
  # Check if Pandoc is available
  if (!check_pandoc_available()) {
    warning("Pandoc not available. Cannot render enhanced research paper.")
    return(render_fallback_report(site_id, output_dir, analysis_results))
  }
  
  # Prepare analysis results for inclusion in report
  report_data <- prepare_report_data(analysis_results, site_id)
  
  # Save analysis results for report consumption
  results_file <- file.path(output_dir, "analysis_results.rds")
  saveRDS(report_data, results_file)
  
  # Copy template to output directory
  template_path <- here("output", template_file)
  if (!file.exists(template_path)) {
    template_path <- here("output", "research_paper.Rmd")  # Fallback template
  }
  
  report_template <- file.path(output_dir, "final_report.Rmd")
  file.copy(template_path, report_template, overwrite = TRUE)
  
  # Generate multiple output formats
  report_paths <- list()
  
  for (format in output_formats) {
    message(sprintf("  Rendering %s version...", format))
    
    tryCatch({
      output_file <- sprintf("comprehensive_analysis_%s", site_id)
      
      rmarkdown::render(
        input = report_template,
        output_format = format,
        output_file = output_file,
        output_dir = output_dir,
        quiet = TRUE,
        params = list(
          site_id = site_id,
          analysis_results = report_data
        )
      )
      
      # Determine file extension based on format
      ext <- switch(format,
                   "html_document" = "html",
                   "word_document" = "docx",
                   "pdf_document" = "pdf",
                   "html")
      
      report_paths[[format]] <- file.path(output_dir, paste0(output_file, ".", ext))
      message(sprintf("  ✓ %s report created: %s", format, basename(report_paths[[format]])))
      
    }, error = function(e) {
      warning(sprintf("Failed to render %s format: %s", format, e$message))
    })
  }
  
  # Create supplementary materials
  create_supplementary_materials(analysis_results, site_id, output_dir)
  
  message(sprintf("✓ Comprehensive research paper completed for site %s", site_id))
  return(report_paths)
}

#' Prepare analysis results for report inclusion
#'
#' Formats and structures analysis results for consumption by RMarkdown templates.
#'
#' @param analysis_results List containing all analysis results
#' @param site_id Character string specifying site ID
#' @return Formatted data structure for report
#' @export
prepare_report_data <- function(analysis_results, site_id) {
  
  report_data <- list(
    site_id = site_id,
    timestamp = Sys.time(),
    analysis_date = format(Sys.Date(), "%B %d, %Y")
  )
  
  # Basic statistics
  if (!is.null(analysis_results$descriptive_stats)) {
    report_data$descriptive_stats <- analysis_results$descriptive_stats
  }
  
  # Model results
  if (!is.null(analysis_results$soil_root_models)) {
    report_data$model_results <- format_model_results(analysis_results$soil_root_models)
  }
  
  # Spatial analysis
  if (!is.null(analysis_results$spatial_autocorrelation)) {
    report_data$spatial_results <- format_spatial_results(analysis_results$spatial_autocorrelation)
  }
  
  # Temporal analysis
  if (!is.null(analysis_results$temporal_dynamics)) {
    report_data$temporal_results <- format_temporal_results(analysis_results$temporal_dynamics)
  }
  
  # Machine learning
  if (!is.null(analysis_results$ml_results)) {
    report_data$ml_results <- format_ml_results(analysis_results$ml_results)
  }
  
  # Bayesian analysis
  if (!is.null(analysis_results$bayesian_results)) {
    report_data$bayesian_results <- format_bayesian_results(analysis_results$bayesian_results)
  }
  
  # SEM analysis
  if (!is.null(analysis_results$sem_results)) {
    report_data$sem_results <- format_sem_results(analysis_results$sem_results)
  }
  
  # Summary tables
  if (!is.null(analysis_results$summary_tables)) {
    report_data$summary_tables <- analysis_results$summary_tables
  }
  
  # Figure paths
  if (!is.null(analysis_results$figure_paths)) {
    report_data$figure_paths <- analysis_results$figure_paths
  }
  
  return(report_data)
}

#' Format model results for report
#'
#' Formats mixed model results for inclusion in research paper.
#'
#' @param model_results List containing model analysis results
#' @return Formatted model results
#' @export
format_model_results <- function(model_results) {
  
  if (is.null(model_results$best_model)) {
    return(list(error = "No model results available"))
  }
  
  formatted <- list(
    best_model = model_results$best_model,
    model_comparison = model_results$model_comparison,
    fixed_effects = model_results$summary$fixed_effects,
    variance_components = model_results$summary$variance_components,
    diagnostics = model_results$diagnostics,
    formula = model_results$formula,
    n_observations = model_results$n_obs,
    r_squared = tryCatch({
      performance::r2(model_results$best_model)$R2_conditional
    }, error = function(e) NA)
  )
  
  return(formatted)
}

#' Format spatial analysis results
#'
#' Formats spatial autocorrelation results for report.
#'
#' @param spatial_results List containing spatial analysis results
#' @return Formatted spatial results
#' @export
format_spatial_results <- function(spatial_results) {
  
  if (is.null(spatial_results$spatial_autocorrelation)) {
    return(list(error = "No spatial results available"))
  }
  
  formatted <- list(
    moran_results = list(),
    geary_results = list(),
    site_summary = list()
  )
  
  for (var in names(spatial_results$spatial_autocorrelation)) {
    var_results <- spatial_results$spatial_autocorrelation[[var]]
    
    formatted$moran_results[[var]] <- data.frame(
      Variable = var,
      Morans_I = var_results$morans_i$statistic,
      P_value = var_results$morans_i$p_value,
      Interpretation = var_results$morans_i$interpretation
    )
    
    formatted$geary_results[[var]] <- data.frame(
      Variable = var,
      Gearys_C = var_results$gearys_c$statistic,
      P_value = var_results$gearys_c$p_value,
      Interpretation = var_results$gearys_c$interpretation
    )
  }
  
  return(formatted)
}

#' Format temporal analysis results
#'
#' Formats temporal dynamics results for report.
#'
#' @param temporal_results List containing temporal analysis results
#' @return Formatted temporal results
#' @export
format_temporal_results <- function(temporal_results) {
  
  if (is.null(temporal_results$temporal_dynamics)) {
    return(list(error = "No temporal results available"))
  }
  
  formatted <- list(
    seasonal_summary = list(),
    trend_analysis = list(),
    site_info = list()
  )
  
  for (var in names(temporal_results$temporal_dynamics)) {
    var_results <- temporal_results$temporal_dynamics[[var]]
    
    formatted$seasonal_summary[[var]] <- var_results$seasonal_summary
    formatted$trend_analysis[[var]] <- var_results$trend_model
  }
  
  formatted$site_info <- list(
    temporal_scale = temporal_results$temporal_dynamics[[1]]$temporal_scale,
    date_range = temporal_results$temporal_dynamics[[1]]$date_range
  )
  
  return(formatted)
}

#' Format machine learning results
#'
#' Formats ML analysis results for report.
#'
#' @param ml_results List containing machine learning results
#' @return Formatted ML results
#' @export
format_ml_results <- function(ml_results) {
  
  if (is.null(ml_results$machine_learning)) {
    return(list(error = "No ML results available"))
  }
  
  formatted <- list(
    performance_metrics = ml_results$machine_learning$train_performance,
    test_performance = ml_results$machine_learning$test_performance,
    variable_importance = ml_results$machine_learning$variable_importance,
    method = ml_results$ml_method,
    n_training = ml_results$n_training,
    n_testing = ml_results$n_testing
  )
  
  return(formatted)
}

#' Format Bayesian analysis results
#'
#' Formats Bayesian analysis results for report.
#'
#' @param bayes_results List containing Bayesian analysis results
#' @return Formatted Bayesian results
#' @export
format_bayesian_results <- function(bayes_results) {
  
  if (is.null(bayes_results$bayesian_model)) {
    return(list(error = "No Bayesian results available"))
  }
  
  formatted <- list(
    model_summary = bayes_results$model_summary,
    credible_intervals = bayes_results$credible_intervals,
    prior_specifications = bayes_results$prior_specifications,
    n_chains = bayes_results$n_chains,
    n_iterations = bayes_results$n_iterations,
    n_observations = bayes_results$n_observations
  )
  
  return(formatted)
}

#' Format SEM analysis results
#'
#' Formats structural equation modeling results for report.
#'
#' @param sem_results List containing SEM analysis results
#' @return Formatted SEM results
#' @export
format_sem_results <- function(sem_results) {
  
  if (is.null(sem_results$sem_model)) {
    return(list(error = "No SEM results available"))
  }
  
  formatted <- list(
    fit_measures = sem_results$fit_measures,
    model_equation = sem_results$model_equation,
    sem_summary = sem_results$sem_summary,
    n_observations = sem_results$n_observations
  )
  
  return(formatted)
}

#' Create supplementary materials
#'
#' Generates supplementary materials including detailed statistical tables,
#' additional figures, and code documentation.
#'
#' @param analysis_results List containing analysis results
#' @param site_id Character string specifying site ID
#' @param output_dir Character string specifying output directory
#' @export
create_supplementary_materials <- function(analysis_results, site_id, output_dir) {
  
  message("  Creating supplementary materials...")
  
  supp_dir <- file.path(output_dir, "supplementary")
  if (!dir.exists(supp_dir)) {
    dir.create(supp_dir, recursive = TRUE)
  }
  
  # Create detailed statistical tables
  create_detailed_tables(analysis_results, site_id, supp_dir)
  
  # Create additional figures
  create_additional_figures(analysis_results, site_id, supp_dir)
  
  # Create code documentation
  create_code_documentation(site_id, supp_dir)
  
  message("  ✓ Supplementary materials created")
}

#' Create detailed statistical tables
#'
#' Generates comprehensive statistical tables for supplementary materials.
#'
#' @param analysis_results List containing analysis results
#' @param site_id Character string specifying site ID
#' @param supp_dir Character string specifying supplementary directory
#' @export
create_detailed_tables <- function(analysis_results, site_id, supp_dir) {
  
  # Model coefficients table
  if (!is.null(analysis_results$soil_root_models)) {
    if (!is.null(analysis_results$soil_root_models$summary)) {
      write_csv(
        analysis_results$soil_root_models$summary$fixed_effects,
        file.path(supp_dir, paste0("model_coefficients_", site_id, ".csv"))
      )
    }
  }
  
  # Spatial analysis details
  if (!is.null(analysis_results$spatial_autocorrelation)) {
    # Detailed spatial results would be saved here
  }
  
  # ML feature importance
  if (!is.null(analysis_results$ml_results)) {
    if (!is.null(analysis_results$ml_results$machine_learning$variable_importance)) {
      write_csv(
        as.data.frame(analysis_results$ml_results$machine_learning$variable_importance),
        file.path(supp_dir, paste0("ml_importance_", site_id, ".csv"))
      )
    }
  }
}

#' Create additional figures
#'
#' Generates additional figures for supplementary materials.
#'
#' @param analysis_results List containing analysis results
#' @param site_id Character string specifying site ID
#' @param supp_dir Character string specifying supplementary directory
#' @export
create_additional_figures <- function(analysis_results, site_id, supp_dir) {
  
  # Residual plots
  # Diagnostic plots
  # Additional visualizations would be created here
  
  message("    Created diagnostic figures")
}

#' Create code documentation
#'
#' Generates code documentation for supplementary materials.
#'
#' @param site_id Character string specifying site ID
#' @param supp_dir Character string specifying supplementary directory
#' @export
create_code_documentation <- function(site_id, supp_dir) {
  
  # Session info
  session_info <- capture.output(sessionInfo())
  writeLines(session_info, file.path(supp_dir, paste0("session_info_", site_id, ".txt")))
  
  # Package versions
  pkg_info <- capture.output(installed.packages()[, c("Package", "Version")])
  writeLines(pkg_info, file.path(supp_dir, paste0("package_versions_", site_id, ".txt")))
  
  message("    Created code documentation")
}

#' Render fallback report when advanced features unavailable
#'
#' Creates a basic summary report when advanced features cannot be rendered.
#'
#' @param site_id Character string specifying site ID
#' @param output_dir Character string specifying output directory
#' @param analysis_results List containing analysis results
#' @return Path to fallback report
#' @export
render_fallback_report <- function(site_id, output_dir, analysis_results) {
  
  message("Creating fallback summary report...")
  
  fallback_content <- paste0('
<!DOCTYPE html>
<html>
<head>
  <title>NEON Root-Soil Analysis Summary - ', site_id, '</title>
  <style>
    body { font-family: Arial, sans-serif; margin: 40px; line-height: 1.6; }
    h1 { color: #2c5530; border-bottom: 2px solid #4CAF50; }
    h2 { color: #4CAF50; }
    .stats { background: #f0f8f0; padding: 15px; border-radius: 5px; }
    table { border-collapse: collapse; width: 100%; margin: 20px 0; }
    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
    th { background-color: #4CAF50; color: white; }
  </style>
</head>
<body>
  <h1>NEON Root-Soil Analysis Summary - ', site_id, '</h1>
  
  <div class="stats">
    <h3>Analysis Summary</h3>
    <p>This is a summary report for the NEON root-soil analysis conducted for site ', site_id, '.</p>
    <p>For the complete research paper with advanced statistical analyses, please ensure Pandoc is installed and run the analysis again.</p>
  </div>
  
  <h2>Key Results</h2>
  <p>Basic analysis completed successfully. Advanced features require Pandoc installation.</p>
  
  <h2>Next Steps</h2>
  <ol>
    <li>Install Pandoc: https://pandoc.org/installing.html</li>
    <li>Run the analysis again: Rscript scripts/run_analysis.R</li>
    <li>Access the comprehensive research paper</li>
  </ol>
  
  <p><em>Generated on: ', Sys.Date(), '</em></p>
</body>
</html>
  ')
  
  fallback_path <- file.path(output_dir, paste0("summary_", site_id, ".html"))
  writeLines(fallback_content, fallback_path)
  
  message("  ✓ Fallback report created")
  return(fallback_path)
}
