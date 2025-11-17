#' Statistical Analysis Functions for NEON Root Chemistry Data
#'
#' This script contains functions for performing statistical analyses
#' of root chemistry and soil bulk density relationships.
#'
#' @author Sergio Ocampo
#' @date November 2025

library(dplyr)
library(tidyr)
library(broom)
library(stats)

#' Calculate summary statistics by group
#'
#' @param data Data frame containing the data
#' @param group_var Grouping variable name
#' @param numeric_vars Numeric variables to summarize
#' @return Data frame with summary statistics
#' @export
calculate_summary_stats <- function(data, group_var, numeric_vars) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  if (!group_var %in% names(data)) {
    stop(paste("Group variable", group_var, "not found in data"))
  }
  
  missing_vars <- setdiff(numeric_vars, names(data))
  if (length(missing_vars) > 0) {
    stop(paste("Missing numeric variables:", paste(missing_vars, collapse = ", ")))
  }
  
  summary_stats <- data %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      across(
        all_of(numeric_vars),
        list(
          n = ~sum(!is.na(.)),
          mean = ~mean(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE),
          se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))),
          min = ~min(.x, na.rm = TRUE),
          max = ~max(.x, na.rm = TRUE),
          median = ~median(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
  
  return(summary_stats)
}

#' Perform t-test comparison between root size categories
#'
#' @param data Data frame containing root chemistry data
#' @param var Variable to test
#' @param group_var Grouping variable (should have two levels)
#' @return List containing test results and interpretation
#' @export
compare_root_sizes <- function(data, var, group_var = "sizeCategory") {
  if (!var %in% names(data)) {
    stop(paste("Variable", var, "not found in data"))
  }
  
  if (!group_var %in% names(data)) {
    stop(paste("Group variable", group_var, "not found in data"))
  }
  
  # Filter out missing values
  test_data <- data %>%
    filter(!is.na(!!sym(var)), !is.na(!!sym(group_var)))
  
  # Check if we have enough data
  group_counts <- table(test_data[[group_var]])
  if (length(group_counts) < 2) {
    stop("Need at least two groups for comparison")
  }
  
  if (any(group_counts < 3)) {
    warning("Some groups have very few observations (< 3)")
  }
  
  # Perform t-test
  formula <- as.formula(paste(var, "~", group_var))
  t_test_result <- t.test(formula, data = test_data)
  
  # Calculate effect size (Cohen's d)
  group_means <- test_data %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      mean = mean(!!sym(var), na.rm = TRUE),
      sd = sd(!!sym(var), na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  pooled_sd <- sqrt(
    ((group_means$n[1] - 1) * group_means$sd[1]^2 + 
     (group_means$n[2] - 1) * group_means$sd[2]^2) / 
    (group_means$n[1] + group_means$n[2] - 2)
  )
  
  cohens_d <- abs(diff(group_means$mean)) / pooled_sd
  
  # Interpret effect size
  effect_interpretation <- ifelse(
    cohens_d < 0.2, "negligible",
    ifelse(cohens_d < 0.5, "small",
           ifelse(cohens_d < 0.8, "medium", "large"))
  )
  
  return(list(
    test_type = "Two-sample t-test",
    statistic = t_test_result$statistic,
    df = t_test_result$parameter,
    p_value = t_test_result$p.value,
    mean_difference = diff(group_means$mean),
    cohens_d = cohens_d,
    effect_size = effect_interpretation,
    confidence_interval = t_test_result$conf.int,
    group_means = group_means,
    interpretation = ifelse(
      t_test_result$p.value < 0.05,
      "Significant difference between groups",
      "No significant difference between groups"
    )
  ))
}

#' Perform correlation analysis between soil and root variables
#'
#' @param data Data frame containing both soil and root data
#' @param soil_var Soil variable name
#' @param root_var Root chemistry variable name
#' @param method Correlation method ("pearson", "spearman", "kendall")
#' @return List containing correlation results and interpretation
#' @export
analyze_soil_root_correlation <- function(data, soil_var, root_var, method = "pearson") {
  valid_methods <- c("pearson", "spearman", "kendall")
  if (!method %in% valid_methods) {
    stop(paste("Method must be one of:", paste(valid_methods, collapse = ", ")))
  }
  
  if (!soil_var %in% names(data)) {
    stop(paste("Soil variable", soil_var, "not found in data"))
  }
  
  if (!root_var %in% names(data)) {
    stop(paste("Root variable", root_var, "not found in data"))
  }
  
  # Filter out missing values
  test_data <- data %>%
    filter(!is.na(!!sym(soil_var)), !is.na(!!sym(root_var)))
  
  n_obs <- nrow(test_data)
  if (n_obs < 3) {
    stop("Need at least 3 observations for correlation analysis")
  }
  
  # Calculate correlation
  cor_result <- cor.test(
    test_data[[soil_var]], 
    test_data[[root_var]], 
    method = method
  )
  
  # Interpret correlation strength
  cor_value <- cor_result$estimate
  abs_cor <- abs(cor_value)
  
  strength <- ifelse(
    abs_cor < 0.1, "negligible",
    ifelse(abs_cor < 0.3, "weak",
           ifelse(abs_cor < 0.5, "moderate",
                  ifelse(abs_cor < 0.7, "strong", "very strong")))
  )
  
  direction <- ifelse(cor_value > 0, "positive", "negative")
  
  return(list(
    method = paste0(toupper(substring(method, 1, 1)), substring(method, 2), " correlation"),
    correlation = cor_value,
    p_value = cor_result$p.value,
    confidence_interval = cor_result$conf.int,
    n_observations = n_obs,
    strength = strength,
    direction = direction,
    interpretation = paste(
      strength, "and", direction, "correlation",
      ifelse(cor_result$p.value < 0.05, "(significant)", "(not significant)")
    )
  ))
}

#' Perform linear regression analysis
#'
#' @param data Data frame containing the data
#' @param formula Regression formula
#' @return List containing regression results
#' @export
perform_regression <- function(data, formula) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # Perform regression
  model <- lm(formula, data = data)
  model_summary <- summary(model)
  
  # Extract key statistics
  r_squared <- model_summary$r.squared
  adj_r_squared <- model_summary$adj.r.squared
  p_value <- pf(model_summary$fstatistic[1], 
                model_summary$fstatistic[2], 
                model_summary$fstatistic[3], 
                lower.tail = FALSE)
  
  # Create model diagnostics
  diagnostics <- list(
    residuals = residuals(model),
    fitted_values = fitted(model),
    leverage = hatvalues(model),
    cooks_distance = cooks.distance(model)
  )
  
  return(list(
    model = model,
    formula = formula,
    coefficients = tidy(model),
    r_squared = r_squared,
    adj_r_squared = adj_r_squared,
    f_statistic = model_summary$fstatistic,
    p_value = p_value,
    diagnostics = diagnostics,
    summary = model_summary
  ))
}

#' Create comprehensive analysis summary table
#'
#' @param root_data Root chemistry data frame
#' @param soil_data Soil bulk density data frame
#' @param merged_data Merged root and soil data frame
#' @return List of summary tables
#' @export
create_analysis_summary <- function(root_data, soil_data, merged_data) {
  
  # Root chemistry summary by size category
  if ("sizeCategory" %in% names(root_data)) {
    root_summary <- calculate_summary_stats(
      root_data,
      group_var = "sizeCategory",
      numeric_vars = c("carbonPercent", "nitrogenPercent", "cn_ratio")
    )
  } else {
    root_summary <- root_data %>%
      summarise(
        across(
          c("carbonPercent", "nitrogenPercent", "cn_ratio"),
          list(
            n = ~sum(!is.na(.)),
            mean = ~mean(.x, na.rm = TRUE),
            sd = ~sd(.x, na.rm = TRUE),
            se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
          ),
          .names = "{.col}_{.fn}"
        )
      )
  }
  
  # Soil bulk density summary
  soil_summary <- soil_data %>%
    summarise(
      across(
        "bulk_density",
        list(
          n = ~sum(!is.na(.)),
          mean = ~mean(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE),
          se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))),
          min = ~min(.x, na.rm = TRUE),
          max = ~max(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      )
    )
  
  # Depth category summary if available
  if ("depth_category" %in% names(root_data)) {
    depth_summary <- calculate_summary_stats(
      root_data,
      group_var = "depth_category",
      numeric_vars = c("carbonPercent", "nitrogenPercent", "cn_ratio")
    )
  } else {
    depth_summary <- NULL
  }
  
  return(list(
    root_chemistry_summary = root_summary,
    soil_bulk_density_summary = soil_summary,
    depth_category_summary = depth_summary
  ))
}

#' Perform all statistical tests
#'
#' @param root_data Root chemistry data frame
#' @param soil_data Soil bulk density data frame
#' @param merged_data Merged root and soil data frame
#' @return List containing all test results
#' @export
perform_comprehensive_analysis <- function(root_data, soil_data, merged_data) {
  
  results <- list()
  
  # 1. Root size comparisons
  if ("sizeCategory" %in% names(root_data)) {
    message("Comparing root size categories...")
    
    results$size_comparisons <- list(
      nitrogen = compare_root_sizes(root_data, "nitrogenPercent"),
      carbon = compare_root_sizes(root_data, "carbonPercent"),
      cn_ratio = compare_root_sizes(root_data, "cn_ratio")
    )
  }
  
  # 2. Soil-root correlations
  if (all(c("bulk_density", "nitrogenPercent") %in% names(merged_data))) {
    message("Analyzing soil-root correlations...")
    
    results$soil_root_correlations <- list(
      nitrogen_bulk_density = analyze_soil_root_correlation(
        merged_data, "bulk_density", "nitrogenPercent"
      ),
      carbon_bulk_density = analyze_soil_root_correlation(
        merged_data, "bulk_density", "carbonPercent"
      ),
      cn_ratio_bulk_density = analyze_soil_root_correlation(
        merged_data, "bulk_density", "cn_ratio"
      )
    )
  }
  
  # 3. Regression analyses
  if (all(c("bulk_density", "nitrogenPercent") %in% names(merged_data))) {
    message("Performing regression analyses...")
    
    results$regression_analyses <- list(
      nitrogen_vs_bulk_density = perform_regression(
        merged_data,
        nitrogenPercent ~ bulk_density
      ),
      carbon_vs_bulk_density = perform_regression(
        merged_data,
        carbonPercent ~ bulk_density
      ),
      cn_ratio_vs_bulk_density = perform_regression(
        merged_data,
        cn_ratio ~ bulk_density
      )
    )
  }
  
  # 4. Summary statistics
  message("Creating summary tables...")
  results$summary_tables <- create_analysis_summary(root_data, soil_data, merged_data)
  
  return(results)
}

#' Print analysis results in a readable format
#'
#' @param analysis_results Results from perform_comprehensive_analysis
#' @export
print_analysis_results <- function(analysis_results) {
  
  cat("\n=== NEON DEJU Site Root Chemistry Analysis Results ===\n\n")
  
  # Root size comparisons
  if (!is.null(analysis_results$size_comparisons)) {
    cat("1. ROOT SIZE CATEGORY COMPARISONS\n")
    cat("---------------------------------\n")
    
    for (var_name in names(analysis_results$size_comparisons)) {
      result <- analysis_results$size_comparisons[[var_name]]
      cat(paste0("\n", toupper(var_name), ":\n"))
      cat(paste("  Mean difference:", round(result$mean_difference, 3), "\n"))
      cat(paste("  t-statistic:", round(result$statistic, 3), "\n"))
      cat(paste("  p-value:", format.pval(result$p_value), "\n"))
      cat(paste("  Cohen's d:", round(result$cohens_d, 3), 
                paste0(" (", result$effect_size, " effect)\n")))
      cat(paste("  Interpretation:", result$interpretation, "\n"))
    }
  }
  
  # Soil-root correlations
  if (!is.null(analysis_results$soil_root_correlations)) {
    cat("\n\n2. SOIL-ROOT CORRELATIONS\n")
    cat("------------------------\n")
    
    for (var_name in names(analysis_results$soil_root_correlations)) {
      result <- analysis_results$soil_root_correlations[[var_name]]
      cat(paste0("\n", toupper(gsub("_", " ", var_name)), ":\n"))
      cat(paste("  Correlation coefficient:", round(result$correlation, 3), "\n"))
      cat(paste("  p-value:", format.pval(result$p_value), "\n"))
      cat(paste("  n observations:", result$n_observations, "\n"))
      cat(paste("  Interpretation:", result$interpretation, "\n"))
    }
  }
  
  # Regression results
  if (!is.null(analysis_results$regression_analyses)) {
    cat("\n\n3. REGRESSION ANALYSES\n")
    cat("---------------------\n")
    
    for (var_name in names(analysis_results$regression_analyses)) {
      result <- analysis_results$regression_analyses[[var_name]]
      cat(paste0("\n", toupper(gsub("_", " ", var_name)), ":\n"))
      cat(paste("  R-squared:", round(result$r_squared, 3), "\n"))
      cat(paste("  Adjusted R-squared:", round(result$adj_r_squared, 3), "\n"))
      cat(paste("  F-statistic p-value:", format.pval(result$p_value), "\n"))
    }
  }
  
  cat("\n\n=== Analysis Complete ===\n")
}
