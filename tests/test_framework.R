#' Test Suite for NEON Root-Soil Analysis Framework
#'
#' Comprehensive tests for the sophisticated ecological analysis framework,
#' ensuring robustness across different NEON sites and edge cases.
#'
#' @name test_framework
#' @docType package
NULL

context("NEON Root-Soil Analysis Framework Tests")

test_that("Site configuration loading works correctly", {
  
  # Test DEJU site configuration
  config_deju <- load_site_config("DEJU")
  expect_type(config_deju, "list")
  expect_equal(config_deju$domain, "D19")
  expect_true("depth_breaks" %in% names(config_deju))
  expect_true("root_size_classes" %in% names(config_deju))
  
  # Test HARV site configuration
  config_harv <- load_site_config("HARV")
  expect_type(config_harv, "list")
  expect_equal(config_harv$domain, "D01")
  
  # Test error handling for invalid site
  expect_error(load_site_config("INVALID"))
})

test_that("Data processing handles multiple sites correctly", {
  
  # Test with existing DEJU data
  test_data <- process_neon_data(
    data_dir = "data/raw_data",
    site_id = "DEJU",
    validate = FALSE
  )
  
  expect_type(test_data, "list")
  expect_true("root_chemistry" %in% names(test_data))
  expect_true("soil_bulk_density" %in% names(test_data))
  
  if (!is.null(test_data$root_chemistry)) {
    expect_true(nrow(test_data$root_chemistry) > 0)
    expect_true("carbonPercent" %in% names(test_data$root_chemistry))
    expect_true("nitrogenPercent" %in% names(test_data$root_chemistry))
    expect_true("cn_ratio" %in% names(test_data$root_chemistry))
  }
})

test_that("Mixed model fitting works with ecological data", {
  
  # Load test data
  processed_data <- process_neon_data("data/raw_data", "DEJU", validate = FALSE)
  
  if (!is.null(processed_data$root_chemistry) && nrow(processed_data$root_chemistry) > 20) {
    
    # Test linear mixed model
    model_results <- fit_root_chemistry_model(
      data = processed_data$root_chemistry,
      response_var = "cn_ratio",
      fixed_effects = c("carbonPercent"),
      random_effects = c("pitID"),
      site_id = "DEJU",
      model_type = "lmer"
    )
    
    expect_type(model_results, "list")
    expect_true("model" %in% names(model_results))
    expect_true("diagnostics" %in% names(model_results))
    expect_true("summary" %in% names(model_results))
    
    if (!is.null(model_results$model)) {
      expect_s3_class(model_results$model, "lmerMod")
    }
  }
})

test_that("Depth-stratified analysis produces valid results", {
  
  processed_data <- process_neon_data("data/raw_data", "DEJU", validate = FALSE)
  
  if (!is.null(processed_data$root_chemistry) && nrow(processed_data$root_chemistry) > 10) {
    
    depth_results <- analyze_depth_stratified(
      data = processed_data$root_chemistry,
      response_vars = c("cn_ratio", "carbonPercent"),
      site_id = "DEJU"
    )
    
    expect_type(depth_results, "list")
    expect_true("depth_stratified" %in% names(depth_results))
    expect_equal(depth_results$site_id, "DEJU")
    
    if (!is.null(depth_results$depth_stratified)) {
      expect_true("cn_ratio" %in% names(depth_results$depth_stratified))
    }
  }
})

test_that("Visualization functions produce valid ggplot objects", {
  
  processed_data <- process_neon_data("data/raw_data", "DEJU", validate = FALSE)
  
  if (!is.null(processed_data$root_chemistry)) {
    
    # Test basic visualization
    p1 <- plot_nitrogen_distribution(processed_data$root_chemistry)
    expect_s3_class(p1, "gg")
    
    p2 <- plot_carbon_nitrogen_relationship(processed_data$root_chemistry)
    expect_s3_class(p2, "gg")
    
    p3 <- plot_cn_ratio_distribution(processed_data$root_chemistry)
    expect_s3_class(p3, "gg")
  }
  
  if (!is.null(processed_data$merged_data) && nrow(processed_data$merged_data) > 10) {
    
    p4 <- plot_soil_root_relationship(
      processed_data$merged_data,
      response_var = "cn_ratio",
      soil_predictor = "bulk_density",
      site_id = "DEJU"
    )
    
    if (!is.null(p4)) {
      expect_s3_class(p4, "gg")
    }
  }
})

test_that("Data validation catches problematic inputs", {
  
  # Test with empty data frame
  empty_data <- data.frame(siteID = character())
  validation <- validate_root_chemistry(empty_data, "DEJU")
  expect_false(validation$valid)
  expect_match(validation$message, "Insufficient sample size")
  
  # Test with invalid values
  invalid_data <- data.frame(
    siteID = "DEJU",
    carbonPercent = c(150, -10, 50),  # Out of range
    nitrogenPercent = c(1, 2, 3),
    cn_ratio = c(20, 30, 40)
  )
  
  validation <- validate_root_chemistry(invalid_data, "DEJU")
  expect_false(validation$valid)
  expect_match(validation$message, "outside valid range")
})

test_that("Multi-site analysis framework is extensible", {
  
  # Test configuration for multiple sites
  site_configs <- map(c("DEJU", "HARV", "BART"), load_site_config)
  
  expect_type(site_configs, "list")
  expect_equal(length(site_configs), 3)
  
  # Verify each site has different parameters
  depth_breaks <- map(site_configs, "depth_breaks")
  expect_true(length(unique(depth_breaks)) > 1)
})

test_that("Error handling is robust", {
  
  # Test with missing files
  expect_error(
    process_neon_data("nonexistent_directory", "DEJU"),
    "Critical data files missing"
  )
  
  # Test with insufficient data
  small_data <- data.frame(
    siteID = "DEJU",
    carbonPercent = c(45, 46),
    nitrogenPercent = c(0.8, 0.9),
    cn_ratio = c(56, 57)
  )
  
  # This should handle gracefully without crashing
  expect_warning(
    fit_root_chemistry_model(small_data, response_var = "cn_ratio"),
    "Insufficient sample size"
  )
})

test_that("Publication figure generation works correctly", {
  
  processed_data <- process_neon_data("data/raw_data", "DEJU", validate = FALSE)
  
  if (!is.null(processed_data$root_chemistry)) {
    
    figure_paths <- create_publication_figures(
      processed_data,
      site_id = "DEJU",
      output_dir = tempdir(),
      dpi = 150  # Lower DPI for testing
    )
    
    expect_type(figure_paths, "list")
    expect_true(length(figure_paths) > 0)
    
    # Check that files were created
    for (fig_path in figure_paths) {
      if (!is.null(fig_path)) {
        expect_true(file.exists(fig_path))
      }
    }
  }
})

test_that("Main pipeline API functions correctly", {
  
  # Test high-level API
  results <- run_deju_pipeline(
    site_id = "DEJU",
    data_dir = "data/raw_data",
    output_dir = tempdir(),
    download_data = FALSE,
    run_models = TRUE,
    create_figures = FALSE,
    render_report = FALSE
  )
  
  expect_type(results, "list")
  expect_true("site_id" %in% names(results))
  expect_true("processed_data" %in% names(results))
  expect_true("analysis_results" %in% names(results))
  expect_equal(results$site_id, "DEJU")
})

test_that("Configuration system is flexible", {
  
  # Test custom configuration
  custom_config <- list(
    depth_breaks = c(0, 5, 15, 30, 60, 100),
    root_size_classes = list(fine = "≤2mm", coarse = ">2mm"),
    significance_level = 0.01
  )
  
  processed_data <- process_neon_data(
    "data/raw_data",
    "DEJU",
    config = custom_config,
    validate = FALSE
  )
  
  # Should use custom configuration
  expect_type(processed_data, "list")
})

test_that("Statistical assumptions are checked", {
  
  processed_data <- process_neon_data("data/raw_data", "DEJU", validate = FALSE)
  
  if (!is.null(processed_data$root_chemistry) && nrow(processed_data$root_chemistry) > 30) {
    
    # Test model diagnostics
    model_results <- fit_root_chemistry_model(
      processed_data$root_chemistry,
      response_var = "cn_ratio",
      fixed_effects = c("carbonPercent"),
      random_effects = c("pitID"),
      site_id = "DEJU"
    )
    
    if (!is.null(model_results$diagnostics)) {
      expect_true("residual_normality" %in% names(model_results$diagnostics) ||
                  "error" %in% names(model_results$diagnostics))
    }
  }
})

test_that("Performance is acceptable for ecological datasets", {
  
  start_time <- Sys.time()
  
  # Run a complete analysis
  results <- run_deju_pipeline(
    site_id = "DEJU",
    data_dir = "data/raw_data",
    output_dir = tempdir(),
    download_data = FALSE,
    run_models = TRUE,
    create_figures = FALSE,
    render_report = FALSE
  )
  
  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Should complete within reasonable time (5 minutes for full analysis)
  expect_true(duration < 300)
  
  message(sprintf("Full analysis completed in %.1f seconds", duration))
})

# Integration test for complete workflow
test_that("Complete workflow produces expected outputs", {
  
  withr::local_dir(tempdir())
  
  # Run complete pipeline
  results <- run_deju_pipeline(
    site_id = "DEJU",
    data_dir = file.path("..", "data", "raw_data"),
    output_dir = "test_output",
    download_data = FALSE,
    run_models = TRUE,
    create_figures = TRUE,
    render_report = FALSE
  )
  
  # Check expected outputs exist
  expect_true(file.exists("test_output/figures/nitrogen_distribution.png"))
  expect_true(file.exists("test_output/figures/carbon_nitrogen_relationship.png"))
  expect_true(file.exists("test_output/root_chemistry_summary.csv"))
  expect_true(file.exists("test_output/depth_category_summary.csv"))
  
  # Check that results contain expected components
  expect_true(!is.null(results$processed_data))
  expect_true(!is.null(results$analysis_results))
  expect_true(!is.null(results$figure_paths))
})

# Cleanup function for tests
cleanup_test_files <- function() {
  # Remove any test files created during testing
  test_dirs <- c("test_output", "temp_test")
  for (dir in test_dirs) {
    if (dir.exists(dir)) {
      unlink(dir, recursive = TRUE)
    }
  }
}

# Run cleanup after tests
withr::defer(cleanup_test_files())
