#' Statistical Analysis and Modeling Module for NEON Root-Soil Analysis
#'
#' This module provides statistical modeling functions for analyzing root-soil
#' relationships, including linear models, basic mixed models, and hypothesis testing.
#'
#' @name analysis_models
#' @docType package
NULL

# Load required packages
library(dplyr)
library(broom)

#' Fit root chemistry model
#'
#' Analyzes root chemistry variables using linear models or basic mixed models.
#'
#' @param data Data frame containing root chemistry data
#' @param response_var Character string specifying response variable
#' @param fixed_effects Character vector of fixed effect variables
#' @param random_effects Character vector of random effect variables (optional)
#' @param site_id Character string specifying site ID
#' @param model_type Character string specifying model type ("lm" or "lmer")
#'
#' @return List containing model results and diagnostics
#' @export
fit_root_chemistry_model <- function(data,
                                     response_var = "cn_ratio",
                                     fixed_effects = c("depth_cm"),
                                     random_effects = NULL,
                                     site_id = "DEJU",
                                     model_type = "lm") {
  
  message(sprintf("Fitting %s model for %s at site %s", model_type, response_var, site_id))
  
  # Validate inputs
  required_vars <- c(response_var, fixed_effects)
  if (!is.null(random_effects)) {
    required_vars <- c(required_vars, random_effects)
  }
  
  missing_vars <- setdiff(required_vars, names(data))
  if (length(missing_vars) > 0) {
    stop(sprintf("Missing required variables: %s", paste(missing_vars, collapse = ", ")))
  }
  
  # Check sample size
  if (nrow(data) < 10) {
    warning("Insufficient sample size for reliable model fitting")
    return(list(
      model = NULL,
      diagnostics = list(error = "Insufficient sample size"),
      summary = NULL,
      site_id = site_id
    ))
  }
  
  # Prepare data for modeling
  model_data <- data %>%
    select(all_of(required_vars)) %>%
    filter(if_all(everything(), ~ !is.na(.)))
  
  if (nrow(model_data) < 10) {
    warning("Insufficient complete cases for model fitting")
    return(list(
      model = NULL,
      diagnostics = list(error = "Insufficient complete cases"),
      summary = NULL,
      site_id = site_id
    ))
  }
  
  tryCatch({
    if (model_type == "lmer" && !is.null(random_effects) && length(random_effects) > 0) {
      # Try to use lme4 if available
      if (requireNamespace("lme4", quietly = TRUE)) {
        # Build formula
        fixed_formula <- paste(response_var, "~", paste(fixed_effects, collapse = " + "))
        random_formula <- paste("(", paste(random_effects, collapse = " + "), ")")
        full_formula <- paste(fixed_formula, "+", random_formula)
        
        # Fit mixed model
        model <- lme4::lmer(as.formula(full_formula), data = model_data, REML = TRUE)
        
        # Extract summary
        model_summary <- summary(model)
        
        return(list(
          model = model,
          diagnostics = list(method = "lme4 mixed model"),
          summary = list(
            fixed_effects = broom::tidy(model),
            variance_components = as.data.frame(lme4::VarCorr(model))
          ),
          formula = full_formula,
          model_type = "lmer",
          site_id = site_id,
          n_obs = nrow(model_data)
        ))
        
      } else {
        warning("lme4 package not available, falling back to linear model")
        model_type <- "lm"
      }
    }
    
    if (model_type == "lm") {
      # Build linear model formula
      formula <- paste(response_var, "~", paste(fixed_effects, collapse = " + "))
      
      # Fit linear model
      model <- stats::lm(as.formula(formula), data = model_data)
      
      # Extract summary
      model_summary <- summary(model)
      
      return(list(
        model = model,
        diagnostics = list(
          r_squared = model_summary$r.squared,
          adj_r_squared = model_summary$adj.r.squared,
          f_statistic = model_summary$fstatistic[1],
          f_p_value = pf(model_summary$fstatistic[1], 
                        model_summary$fstatistic[2], 
                        model_summary$fstatistic[3], 
                        lower.tail = FALSE)
        ),
        summary = list(
          fixed_effects = broom::tidy(model),
          model_summary = broom::glance(model)
        ),
        formula = formula,
        model_type = "lm",
        site_id = site_id,
        n_obs = nrow(model_data)
      ))
    }
    
  }, error = function(e) {
    warning(sprintf("Model fitting failed: %s", e$message))
    return(list(
      model = NULL,
      diagnostics = list(error = e$message),
      summary = NULL,
      site_id = site_id
    ))
  })
}

#' Validate root chemistry data
#'
#' Performs validation checks on root chemistry data.
#'
#' @param data Data frame to validate
#' @param site_id Character string specifying site ID
#' @return List containing validation results
#' @export
validate_root_chemistry <- function(data, site_id) {
  
  validation <- list(valid = TRUE, message = "Validation passed")
  
  # Check for required variables
  required_vars <- c("siteID", "carbonPercent", "nitrogenPercent", "cn_ratio")
  missing_vars <- setdiff(required_vars, names(data))
  
  if (length(missing_vars) > 0) {
    validation$valid <- FALSE
    validation$message <- sprintf("Missing required variables: %s", paste(missing_vars, collapse = ", "))
    return(validation)
  }
  
  # Check for reasonable value ranges
  if (any(data$carbonPercent < 0 | data$carbonPercent > 100, na.rm = TRUE)) {
    validation$valid <- FALSE
    validation$message <- "Carbon percentages outside valid range (0-100%)"
    return(validation)
  }
  
  if (any(data$nitrogenPercent < 0 | data$nitrogenPercent > 50, na.rm = TRUE)) {
    validation$valid <- FALSE
    validation$message <- "Nitrogen percentages outside valid range (0-50%)"
    return(validation)
  }
  
  if (any(data$cn_ratio < 1 | data$cn_ratio > 1000, na.rm = TRUE)) {
    validation$valid <- FALSE
    validation$message <- "C:N ratios outside reasonable range (1-1000)"
    return(validation)
  }
  
  # Check minimum sample size
  if (nrow(data) < 10) {
    validation$valid <- FALSE
    validation$message <- "Insufficient sample size for reliable analysis"
    return(validation)
  }
  
  # Check for site-specific data
  if (all(is.na(data$siteID)) || !any(data$siteID == site_id, na.rm = TRUE)) {
    validation$valid <- FALSE
    validation$message <- sprintf("No data found for site %s", site_id)
    return(validation)
  }
  
  return(validation)
}

#' Perform depth-stratified analysis
#'
#' Analyzes root chemistry variables across depth intervals.
#'
#' @param data Data frame containing root chemistry and depth data
#' @param depth_breaks Numeric vector specifying depth interval breaks
#' @param response_vars Character vector of response variables to analyze
#' @param site_id Character string specifying site ID
#' @param config Site configuration list
#'
#' @return List containing depth-stratified analysis results
#' @export
analyze_depth_stratified <- function(data,
                                     depth_breaks = c(0, 10, 25, 50, 100, 200),
                                     response_vars = c("carbonPercent", "nitrogenPercent", "cn_ratio"),
                                     site_id = "DEJU",
                                     config = NULL) {
  
  message(sprintf("Performing depth-stratified analysis for site %s", site_id))
  
  # Load site configuration if not provided
  if (is.null(config)) {
    config <- load_site_config(site_id)
  }
  
  # Use config depth breaks if available
  if (!is.null(config$depth_breaks)) {
    depth_breaks <- config$depth_breaks
  }
  
  # Create depth categories
  data <- data %>%
    mutate(
      depth_category = cut(
        depth_cm,
        breaks = depth_breaks,
        labels = sprintf("Depth_%d-%d", depth_breaks[-length(depth_breaks)], depth_breaks[-1]),
        include.lowest = TRUE
      )
    )
  
  # Analyze each response variable
  depth_results <- list()
  
  for (response_var in response_vars) {
    if (!(response_var %in% names(data))) {
      warning(sprintf("Response variable %s not found in data", response_var))
      next
    }
    
    message(sprintf("  Analyzing %s across depth categories", response_var))
    
    # Summary statistics by depth
    depth_summary <- data %>%
      group_by(depth_category) %>%
      summarise(
        n = n(),
        mean = mean(.data[[response_var]], na.rm = TRUE),
        sd = sd(.data[[response_var]], na.rm = TRUE),
        se = sd / sqrt(n),
        min = min(.data[[response_var]], na.rm = TRUE),
        max = max(.data[[response_var]], na.rm = TRUE),
        median = median(.data[[response_var]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        response_var = response_var,
        site_id = site_id
      )
    
    # Statistical tests across depth categories
    if (n_distinct(data$depth_category, na.rm = TRUE) >= 3) {
      # ANOVA across depth categories
      anova_formula <- as.formula(paste(response_var, "~ depth_category"))
      anova_result <- tryCatch({
        anova_model <- stats::aov(anova_formula, data = data)
        anova_summary <- summary(anova_model)
        
        list(
          f_statistic = anova_summary[[1]]$`F value`[1],
          p_value = anova_summary[[1]]$`Pr(>F)`[1],
          df_between = anova_summary[[1]]$Df[1],
          df_within = anova_summary[[1]]$Df[2]
        )
      }, error = function(e) {
        list(error = e$message)
      })
      
      # Post-hoc tests if ANOVA is significant
      post_hoc <- NULL
      if (!is.null(anova_result$p_value) && anova_result$p_value < 0.05) {
        post_hoc <- tryCatch({
          tukey_result <- stats::TukeyHSD(anova_model)
          list(
            comparisons = names(tukey_result$depth_category),
            differences = tukey_result$depth_category[, "diff"],
            p_values = tukey_result$depth_category[, "p adj"],
            confidence_intervals = tukey_result$depth_category[, c("lwr", "upr")]
          )
        }, error = function(e) {
          list(error = e$message)
        })
      }
    } else {
      anova_result <- list(error = "Insufficient depth categories for ANOVA")
      post_hoc <- NULL
    }
    
    # Linear trend analysis
    trend_analysis <- analyze_depth_trend(data, response_var, depth_breaks)
    
    depth_results[[response_var]] <- list(
      summary_stats = depth_summary,
      anova = anova_result,
      post_hoc = post_hoc,
      trend_analysis = trend_analysis,
      depth_breaks = depth_breaks
    )
  }
  
  return(list(
    depth_stratified = depth_results,
    site_id = site_id,
    n_total = nrow(data),
    depth_range = range(data$depth_cm, na.rm = TRUE)
  ))
}

#' Analyze linear trends with depth
#'
#' Fits linear models to examine trends in root chemistry variables with depth.
#'
#' @param data Data frame containing depth and chemistry data
#' @param response_var Character string specifying response variable
#' @param depth_breaks Numeric vector of depth breaks for analysis
#' @return List containing trend analysis results
#' @export
analyze_depth_trend <- function(data, response_var, depth_breaks) {
  
  # Remove missing values
  clean_data <- data %>%
    select(depth_cm, all_of(response_var)) %>%
    filter(!is.na(depth_cm), !is.na(.data[[response_var]]))
  
  if (nrow(clean_data) < 10) {
    return(list(
      error = "Insufficient data for trend analysis",
      n_obs = nrow(clean_data)
    ))
  }
  
  # Fit linear model
  trend_formula <- as.formula(paste(response_var, "~ depth_cm"))
  trend_model <- tryCatch({
    stats::lm(trend_formula, data = clean_data)
  }, error = function(e) {
    NULL
  })
  
  if (is.null(trend_model)) {
    return(list(
      error = "Failed to fit trend model",
      n_obs = nrow(clean_data)
    ))
  }
  
  # Extract model summary
  model_summary <- summary(trend_model)
  coefficients <- coef(trend_model)
  
  # Calculate confidence intervals
  conf_int <- stats::confint(trend_model, level = 0.95)
  
  # Test for non-linearity using quadratic term
  quad_formula <- as.formula(paste(response_var, "~ depth_cm + I(depth_cm^2)"))
  quad_model <- tryCatch({
    stats::lm(quad_formula, data = clean_data)
  }, error = function(e) {
    NULL
  })
  
  nonlinearity_test <- NULL
  if (!is.null(quad_model)) {
    anova_comparison <- stats::anova(trend_model, quad_model)
    nonlinearity_test <- list(
      f_statistic = anova_comparison$F[2],
      p_value = anova_comparison$`Pr(>F)`[2],
      significant = anova_comparison$`Pr(>F)`[2] < 0.05
    )
  }
  
  return(list(
    model = trend_model,
    slope = coefficients[2],
    intercept = coefficients[1],
    r_squared = model_summary$r.squared,
    p_value = coef(model_summary)[2, 4],
    confidence_interval = conf_int[2, ],
    nonlinearity_test = nonlinearity_test,
    n_obs = nrow(clean_data),
    depth_range = range(clean_data$depth_cm, na.rm = TRUE)
  ))
}

#' Perform size class tests
#'
#' Conducts statistical tests comparing root chemistry between size classes.
#'
#' @param data Data frame containing root chemistry and size class data
#' @param variables Character vector of variables to test
#' @return Data frame containing test results
#' @export
perform_size_class_tests <- function(data, variables) {
  
  results <- list()
  
  for (var in variables) {
    if (!(var %in% names(data))) next
    
    # Two-sample t-test
    fine_data <- data %>% filter(sizeCategory == "Fine (≤4mm)") %>% pull(!!sym(var))
    coarse_data <- data %>% filter(sizeCategory == "Coarse (>4mm)") %>% pull(!!sym(var))
    
    if (length(fine_data) > 2 && length(coarse_data) > 2) {
      test_result <- t.test(fine_data, coarse_data)
      
      results[[var]] <- data.frame(
        variable = var,
        statistic = test_result$statistic,
        p_value = test_result$p.value,
        mean_difference = test_result$estimate[1] - test_result$estimate[2],
        ci_lower = test_result$conf.int[1],
        ci_upper = test_result$conf.int[2],
        significant = test_result$p.value < 0.05,
        interpretation = ifelse(test_result$p.value < 0.05, "Significant", "Not significant")
      )
    }
  }
  
  if (length(results) > 0) {
    return(bind_rows(results))
  } else {
    return(NULL)
  }
}

#' Calculate size class effects
#'
#' Calculates effect sizes for size class comparisons.
#'
#' @param data Data frame containing root chemistry and size class data
#' @param variables Character vector of variables to analyze
#' @return Data frame containing effect sizes
#' @export
calculate_size_class_effects <- function(data, variables) {
  
  effects <- list()
  
  for (var in variables) {
    if (!(var %in% names(data))) next
    
    fine_data <- data %>% filter(sizeCategory == "Fine (≤4mm)") %>% pull(!!sym(var))
    coarse_data <- data %>% filter(sizeCategory == "Coarse (>4mm)") %>% pull(!!sym(var))
    
    if (length(fine_data) > 2 && length(coarse_data) > 2) {
      # Calculate Cohen's d
      pooled_sd <- sqrt(((length(fine_data) - 1) * var(fine_data, na.rm = TRUE) + 
                        (length(coarse_data) - 1) * var(coarse_data, na.rm = TRUE)) / 
                       (length(fine_data) + length(coarse_data) - 2))
      
      cohens_d <- (mean(fine_data, na.rm = TRUE) - mean(coarse_data, na.rm = TRUE)) / pooled_sd
      
      effects[[var]] <- data.frame(
        variable = var,
        cohens_d = cohens_d,
        effect_size = interpret_effect_size(abs(cohens_d))
      )
    }
  }
  
  if (length(effects) > 0) {
    return(bind_rows(effects))
  } else {
    return(NULL)
  }
}

#' Interpret effect sizes in ecological context
#'
#' Provides ecological interpretation of standardized effect sizes.
#'
#' @param cohens_d Numeric Cohen's d value
#' @return Character string with interpretation
#' @export
interpret_effect_size <- function(cohens_d) {
  
  if (cohens_d < 0.2) {
    return("negligible effect")
  } else if (cohens_d < 0.5) {
    return("small effect")
  } else if (cohens_d < 0.8) {
    return("medium effect")
  } else if (cohens_d < 1.2) {
    return("large effect")
  } else {
    return("very large effect")
  }
}

#' Validate soil bulk density data
#'
#' Performs validation checks on soil bulk density data.
#'
#' @param data Data frame to validate
#' @param site_id Character string specifying site ID
#' @return List containing validation results
#' @export
validate_soil_bulk_density <- function(data, site_id) {
  
  validation <- list(valid = TRUE, message = "Validation passed")
  
  # Check for required variables
  required_vars <- c("siteID", "bulk_density", "depth_cm")
  missing_vars <- setdiff(required_vars, names(data))
  
  if (length(missing_vars) > 0) {
    validation$valid <- FALSE
    validation$message <- sprintf("Missing required variables: %s", paste(missing_vars, collapse = ", "))
    return(validation)
  }
  
  # Check for reasonable value ranges
  if (any(data$bulk_density < 0.1 | data$bulk_density > 3.0, na.rm = TRUE)) {
    validation$valid <- FALSE
    validation$message <- "Bulk density values outside reasonable range (0.1-3.0 g/cm³)"
    return(validation)
  }
  
  if (any(data$depth_cm < 0 | data$depth_cm > 500, na.rm = TRUE)) {
    validation$valid <- FALSE
    validation$message <- "Depth values outside reasonable range (0-500 cm)"
    return(validation)
  }
  
  # Check minimum sample size
  if (nrow(data) < 3) {
    validation$valid <- FALSE
    validation$message <- "Insufficient sample size for reliable analysis"
    return(validation)
  }
  
  # Check for site-specific data
  if (all(is.na(data$siteID)) || !any(data$siteID == site_id, na.rm = TRUE)) {
    validation$valid <- FALSE
    validation$message <- sprintf("No data found for site %s", site_id)
    return(validation)
  }
  
  return(validation)
}

#' Analyze root size classes
#'
#' Performs comprehensive statistical analysis comparing root chemistry
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
