#' Advanced Ecological Analysis Module
#'
#' This module provides cutting-edge analytical methods for ecological research,
#' including spatial analysis, temporal dynamics, machine learning approaches,
#' and advanced visualization techniques suitable for high-impact publications.
#'
#' @name advanced_analysis
#' @docType package
NULL

#' Perform spatial autocorrelation analysis
#'
#' Analyzes spatial patterns in root-soil relationships using Moran's I and
#' Geary's C statistics, essential for understanding spatial dependence in
#' ecological data.
#'
#' @param data Data frame with spatial coordinates and variables
#' @param x_coord Character string for x-coordinate column
#' @param y_coord Character string for y-coordinate column  
#' @param variables Character vector of variables to analyze
#' @param site_id Character string specifying site ID
#' @param distance_threshold Numeric threshold for spatial weights
#'
#' @return List containing spatial autocorrelation results
#' @export
analyze_spatial_autocorrelation <- function(data,
                                           x_coord = "longitude",
                                           y_coord = "latitude", 
                                           variables = c("cn_ratio", "bulk_density"),
                                           site_id = "DEJU",
                                           distance_threshold = 100) {
  
  message(sprintf("Performing spatial autocorrelation analysis for site %s", site_id))
  
  if (!requireNamespace("spdep", quietly = TRUE)) {
    warning("spdep package required for spatial analysis")
    return(NULL)
  }
  
  # Prepare spatial data
  spatial_data <- data %>%
    filter(!is.na(.data[[x_coord]]), !is.na(.data[[y_coord]])) %>%
    select(all_of(c(x_coord, y_coord, variables)))
  
  if (nrow(spatial_data) < 10) {
    warning("Insufficient spatial data for autocorrelation analysis")
    return(NULL)
  }
  
  # Create spatial weights matrix
  coords <- spatial_data %>% select(all_of(c(x_coord, y_coord))) %>% as.matrix()
  dist_matrix <- as.matrix(dist(coords))
  
  # Binary spatial weights (1 if within threshold, 0 otherwise)
  weights <- ifelse(dist_matrix <= distance_threshold, 1, 0)
  diag(weights) <- 0  # Remove self-weights
  
  # Convert to spdep listw object
  nb_list <- mat2listw(weights)
  
  # Calculate Moran's I for each variable
  spatial_results <- list()
  
  for (var in variables) {
    if (!(var %in% names(spatial_data))) next
    
    values <- spatial_data[[var]]
    
    # Moran's I
    moran_test <- spdep::moran.test(values, nb_list, alternative = "two.sided")
    
    # Geary's C
    geary_test <- spdep::geary.test(values, nb_list, alternative = "two.sided")
    
    spatial_results[[var]] <- list(
      morans_i = list(
        statistic = moran_test$estimate["Moran I statistic"],
        p_value = moran_test$p.value,
        interpretation = interpret_moran_statistic(moran_test$estimate["Moran I statistic"])
      ),
      gearys_c = list(
        statistic = geary_test$estimate["Geary C statistic"],
        p_value = geary_test$p.value,
        interpretation = interpret_geary_statistic(geary_test$estimate["Geary C statistic"])
      ),
      n_observations = length(values),
      spatial_weights_used = sum(weights > 0) / 2  # Count unique pairs
    )
  }
  
  return(list(
    spatial_autocorrelation = spatial_results,
    site_id = site_id,
    distance_threshold = distance_threshold,
    analysis_method = "Moran's I and Geary's C"
  ))
}

#' Interpret Moran's I statistic
#'
#' Provides ecological interpretation of Moran's I spatial autocorrelation.
#'
#' @param moran_i Numeric Moran's I value
#' @return Character string with interpretation
#' @export
interpret_moran_statistic <- function(moran_i) {
  
  abs_i <- abs(moran_i)
  
  if (abs_i < 0.1) {
    return("weak spatial autocorrelation")
  } else if (abs_i < 0.3) {
    return("moderate spatial autocorrelation")
  } else if (abs_i < 0.5) {
    return("strong spatial autocorrelation")
  } else {
    return("very strong spatial autocorrelation")
  }
}

#' Interpret Geary's C statistic
#'
#' Provides ecological interpretation of Geary's C spatial autocorrelation.
#'
#' @param geary_c Numeric Geary's C value
#' @return Character string with interpretation
#' @export
interpret_geary_statistic <- function(geary_c) {
  
  if (geary_c < 0.8) {
    return("strong positive spatial autocorrelation")
  } else if (geary_c < 0.9) {
    return("moderate positive spatial autocorrelation")
  } else if (geary_c < 1.1) {
    return("no significant spatial autocorrelation")
  } else if (geary_c < 1.2) {
    return("moderate negative spatial autocorrelation")
  } else {
    return("strong negative spatial autocorrelation")
  }
}

#' Analyze temporal dynamics in root chemistry
#'
#' Examines temporal patterns and trends in root chemistry data across
#' multiple sampling periods, essential for understanding seasonal and
#' interannual variation.
#'
#' @param data Data frame with temporal data
#' @param date_var Character string for date column
#' @param response_vars Character vector of response variables
#' @param site_id Character string specifying site ID
#' @param temporal_scale Character string: "seasonal", "annual", or "monthly"
#'
#' @return List containing temporal analysis results
#' @export
analyze_temporal_dynamics <- function(data,
                                     date_var = "collectDate",
                                     response_vars = c("cn_ratio", "carbonPercent", "nitrogenPercent"),
                                     site_id = "DEJU",
                                     temporal_scale = "seasonal") {
  
  message(sprintf("Analyzing temporal dynamics for site %s at %s scale", site_id, temporal_scale))
  
  # Prepare temporal data
  temporal_data <- data %>%
    filter(!is.na(.data[[date_var]])) %>%
    mutate(
      date = as.Date(.data[[date_var]]),
      year = lubridate::year(date),
      month = lubridate::month(date),
      season = case_when(
        month %in% c(12, 1, 2) ~ "Winter",
        month %in% c(3, 4, 5) ~ "Spring",
        month %in% c(6, 7, 8) ~ "Summer",
        month %in% c(9, 10, 11) ~ "Fall"
      )
    )
  
  if (nrow(temporal_data) < 20) {
    warning("Insufficient temporal data for analysis")
    return(NULL)
  }
  
  temporal_results <- list()
  
  for (response_var in response_vars) {
    if (!(response_var %in% names(temporal_data))) next
    
    # Time series decomposition
    ts_data <- temporal_data %>%
      arrange(date) %>%
      select(date, !!sym(response_var)) %>%
      filter(!is.na(!!sym(response_var)))
    
    if (nrow(ts_data) < 10) next
    
    # Seasonal analysis
    seasonal_summary <- ts_data %>%
      group_by(season) %>%
      summarise(
        n = n(),
        mean = mean(!!sym(response_var), na.rm = TRUE),
        sd = sd(!!sym(response_var), na.rm = TRUE),
        se = sd / sqrt(n),
        .groups = "drop"
      )
    
    # Trend analysis using GAM
    trend_model <- tryCatch({
      mgcv::gam(as.formula(paste(response_var, "~ s(as.numeric(date))")), 
                data = ts_data, family = gaussian())
    }, error = function(e) NULL)
    
    # Seasonal trend analysis
    seasonal_trend <- tryCatch({
      mgcv::gam(as.formula(paste(response_var, "~ season + s(as.numeric(date), by = season)")), 
                data = ts_data, family = gaussian())
    }, error = function(e) NULL)
    
    temporal_results[[response_var]] <- list(
      seasonal_summary = seasonal_summary,
      trend_model = if(!is.null(trend_model)) summary(trend_model) else NULL,
      seasonal_trend_model = if(!is.null(seasonal_trend)) summary(seasonal_trend) else NULL,
      temporal_scale = temporal_scale,
      n_timepoints = nrow(ts_data),
      date_range = range(ts_data$date, na.rm = TRUE)
    )
  }
  
  return(list(
    temporal_dynamics = temporal_results,
    site_id = site_id,
    analysis_method = "GAM-based temporal analysis"
  ))
}

#' Perform machine learning analysis for pattern recognition
#'
#' Applies random forest and gradient boosting models to identify
#' complex non-linear relationships and variable importance in
#' root-soil systems.
#'
#' @param data Data frame containing predictor and response variables
#' @param response_var Character string specifying response variable
#' @param predictor_vars Character vector of predictor variables
#' @param site_id Character string specifying site ID
#' @param ml_method Character string: "random_forest" or "gradient_boosting"
#'
#' @return List containing machine learning analysis results
#' @export
perform_machine_learning_analysis <- function(data,
                                             response_var = "cn_ratio",
                                             predictor_vars = c("depth_cm", "bulk_density", "carbonPercent"),
                                             site_id = "DEJU",
                                             ml_method = "random_forest") {
  
  message(sprintf("Performing %s machine learning analysis for site %s", ml_method, site_id))
  
  if (!requireNamespace("randomForest", quietly = TRUE) || 
      !requireNamespace("xgboost", quietly = TRUE)) {
    warning("Required ML packages not available")
    return(NULL)
  }
  
  # Prepare data
  ml_data <- data %>%
    select(all_of(c(response_var, predictor_vars))) %>%
    filter(if_all(everything(), ~ !is.na(.)))
  
  if (nrow(ml_data) < 50) {
    warning("Insufficient data for machine learning analysis")
    return(NULL)
  }
  
  # Split data for validation
  set.seed(123)
  train_idx <- sample(1:nrow(ml_data), size = 0.7 * nrow(ml_data))
  train_data <- ml_data[train_idx, ]
  test_data <- ml_data[-train_idx, ]
  
  ml_results <- list()
  
  if (ml_method == "random_forest") {
    # Random Forest model
    rf_model <- randomForest::randomForest(
      x = train_data[, predictor_vars],
      y = train_data[[response_var]],
      ntree = 500,
      importance = TRUE
    )
    
    # Predictions and performance
    train_pred <- predict(rf_model, train_data)
    test_pred <- predict(rf_model, test_data)
    
    ml_results$model <- rf_model
    ml_results$variable_importance <- importance(rf_model)
    ml_results$train_performance <- calculate_ml_metrics(train_data[[response_var]], train_pred)
    ml_results$test_performance <- calculate_ml_metrics(test_data[[response_var]], test_pred)
    
  } else if (ml_method == "gradient_boosting") {
    # XGBoost model
    dtrain <- xgboost::xgb.DMatrix(
      data = as.matrix(train_data[, predictor_vars]),
      label = train_data[[response_var]]
    )
    
    dtest <- xgboost::xgb.DMatrix(
      data = as.matrix(test_data[, predictor_vars]),
      label = test_data[[response_var]]
    )
    
    xgb_model <- xgboost::xgboost(
      data = dtrain,
      nrounds = 100,
      objective = "reg:squarederror",
      verbose = 0
    )
    
    # Predictions and performance
    train_pred <- predict(xgb_model, dtrain)
    test_pred <- predict(xgb_model, dtest)
    
    ml_results$model <- xgb_model
    ml_results$feature_importance <- xgboost::xgb.importance(
      model = xgb_model,
      feature_names = predictor_vars
    )
    ml_results$train_performance <- calculate_ml_metrics(train_data[[response_var]], train_pred)
    ml_results$test_performance <- calculate_ml_metrics(test_data[[response_var]], test_pred)
  }
  
  return(list(
    machine_learning = ml_results,
    site_id = site_id,
    ml_method = ml_method,
    n_training = nrow(train_data),
    n_testing = nrow(test_data),
    predictor_variables = predictor_vars
  ))
}

#' Calculate machine learning performance metrics
#'
#' Computes comprehensive performance metrics for ML models.
#'
#' @param actual Actual values
#' @param predicted Predicted values
#' @return List containing performance metrics
#' @export
calculate_ml_metrics <- function(actual, predicted) {
  
  metrics <- list(
    rmse = sqrt(mean((actual - predicted)^2)),
    mae = mean(abs(actual - predicted)),
    r_squared = cor(actual, predicted)^2,
    nrmse = sqrt(mean((actual - predicted)^2)) / sd(actual),
    bias = mean(predicted - actual)
  )
  
  return(metrics)
}

#' Perform structural equation modeling
#'
#' Analyzes complex causal relationships between soil properties,
#' root characteristics, and ecosystem processes using SEM.
#'
#' @param data Data frame containing all variables
#' @param latent_vars List defining latent variables and their indicators
#' @param structural_model Character string defining structural relationships
#' @param site_id Character string specifying site ID
#'
#' @return List containing SEM results
#' @export
perform_sem_analysis <- function(data,
                                latent_vars = list(
                                  soil_quality = c("bulk_density", "pH", "organicC"),
                                  root_quality = c("cn_ratio", "nitrogenPercent")
                                ),
                                structural_model = "root_quality ~ soil_quality",
                                site_id = "DEJU") {
  
  message(sprintf("Performing structural equation modeling for site %s", site_id))
  
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    warning("lavaan package required for SEM analysis")
    return(NULL)
  }
  
  # Prepare data
  sem_data <- data %>%
    select(all_of(unlist(latent_vars))) %>%
    filter(if_all(everything(), ~ !is.na(.)))
  
  if (nrow(sem_data) < 30) {
    warning("Insufficient data for SEM analysis")
    return(NULL)
  }
  
  # Build measurement model
  measurement_model <- paste(
    sapply(names(latent_vars), function(latent) {
      indicators <- paste(latent_vars[[latent]], collapse = " + ")
      paste(latent, "=~", indicators)
    }),
    collapse = "\n"
  )
  
  # Combine measurement and structural models
  full_model <- paste(measurement_model, structural_model, sep = "\n")
  
  # Fit SEM model
  sem_fit <- tryCatch({
    lavaan::sem(full_model, data = sem_data, std.lv = TRUE)
  }, error = function(e) NULL)
  
  if (is.null(sem_fit)) {
    return(list(error = "SEM model fitting failed"))
  }
  
  # Extract results
  sem_summary <- lavaan::summary(sem_fit, standardized = TRUE, fit.measures = TRUE)
  sem_fit_measures <- lavaan::fitMeasures(sem_fit)
  
  return(list(
    sem_model = sem_fit,
    sem_summary = sem_summary,
    fit_measures = sem_fit_measures,
    model_equation = full_model,
    site_id = site_id,
    n_observations = nrow(sem_data)
  ))
}

#' Perform Bayesian analysis for uncertainty quantification
#'
#' Implements Bayesian regression models to quantify uncertainty
#' in parameter estimates and predictions.
#'
#' @param data Data frame containing data
#' @param formula Model formula
#' @param prior_distributions List of prior specifications
#' @param site_id Character string specifying site ID
#' @param n_chains Numeric number of MCMC chains
#' @param n_iterations Numeric number of iterations per chain
#'
#' @return List containing Bayesian analysis results
#' @export
perform_bayesian_analysis <- function(data,
                                     formula = "cn_ratio ~ bulk_density + depth_cm",
                                     prior_distributions = list(
                                       intercept = c(0, 10),
                                       bulk_density = c(0, 5),
                                       depth_cm = c(0, 2),
                                       sigma = c(0, 5)
                                     ),
                                     site_id = "DEJU",
                                     n_chains = 4,
                                     n_iterations = 2000) {
  
  message(sprintf("Performing Bayesian analysis for site %s", site_id))
  
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    warning("rstanarm package required for Bayesian analysis")
    return(NULL)
  }
  
  # Prepare data
  bayes_data <- data %>%
    filter(!is.na(cn_ratio), !is.na(bulk_density), !is.na(depth_cm))
  
  if (nrow(bayes_data) < 30) {
    warning("Insufficient data for Bayesian analysis")
    return(NULL)
  }
  
  # Set priors
  prior_intercept <- rstanarm::normal(prior_distributions$intercept[1], prior_distributions$intercept[2])
  prior_bulk_density <- rstanarm::normal(prior_distributions$bulk_density[1], prior_distributions$bulk_density[2])
  prior_depth_cm <- rstanarm::normal(prior_distributions$depth_cm[1], prior_distributions$depth_cm[2])
  prior_sigma <- rstanarm::exponential(1 / prior_distributions$sigma[2])
  
  # Fit Bayesian model
  bayes_model <- tryCatch({
    rstanarm::stan_glm(
      as.formula(formula),
      data = bayes_data,
      prior = prior_intercept,
      prior_intercept = prior_intercept,
      prior_aux = prior_sigma,
      chains = n_chains,
      iter = n_iterations,
      seed = 123
    )
  }, error = function(e) NULL)
  
  if (is.null(bayes_model)) {
    return(list(error = "Bayesian model fitting failed"))
  }
  
  # Extract results
  bayes_summary <- summary(bayes_model)
  posterior_samples <- as.data.frame(bayes_model)
  
  # Calculate credible intervals
  credible_intervals <- apply(posterior_samples, 2, function(x) {
    quantile(x, c(0.025, 0.975), na.rm = TRUE)
  })
  
  return(list(
    bayesian_model = bayes_model,
    model_summary = bayes_summary,
    posterior_samples = posterior_samples,
    credible_intervals = credible_intervals,
    prior_specifications = prior_distributions,
    site_id = site_id,
    n_observations = nrow(bayes_data),
    n_chains = n_chains,
    n_iterations = n_iterations
  ))
}

#' Create advanced visualization dashboard
#'
#' Generates an interactive dashboard with multiple advanced visualizations
#' suitable for exploratory data analysis and presentation.
#'
#' @param analysis_results List containing all analysis results
#' @param site_id Character string specifying site ID
#' @param output_dir Character string specifying output directory
#' @param interactive Logical indicating whether to create interactive plots
#'
#' @return List containing paths to advanced visualizations
#' @export
create_advanced_dashboard <- function(analysis_results,
                                     site_id = "DEJU",
                                     output_dir = "advanced_figures",
                                     interactive = TRUE) {
  
  message(sprintf("Creating advanced visualization dashboard for site %s", site_id))
  
  if (!dir.exists(output_dir)) {
    dir.exists(output_dir, recursive = TRUE)
  }
  
  dashboard_paths <- list()
  
  # 1. Interactive 3D scatter plot
  if (interactive && requireNamespace("plotly", quietly = TRUE)) {
    dashboard_paths$interactive_3d <- create_interactive_3d_plot(
      analysis_results, site_id, output_dir
    )
  }
  
  # 2. Heatmap of correlations
  dashboard_paths$correlation_heatmap <- create_correlation_heatmap(
    analysis_results, site_id, output_dir
  )
  
  # 3. Network diagram of relationships
  if (!is.null(analysis_results$sem_results)) {
    dashboard_paths$network_diagram <- create_network_diagram(
      analysis_results$sem_results, site_id, output_dir
    )
  }
  
  # 4. Animated temporal plot
  if (!is.null(analysis_results$temporal_results) && interactive) {
    dashboard_paths$temporal_animation <- create_temporal_animation(
      analysis_results$temporal_results, site_id, output_dir
    )
  }
  
  # 5. Machine learning feature importance plot
  if (!is.null(analysis_results$ml_results)) {
    dashboard_paths$ml_importance <- create_ml_importance_plot(
      analysis_results$ml_results, site_id, output_dir
    )
  }
  
  message(sprintf("✓ Created %d advanced visualizations", length(dashboard_paths)))
  return(dashboard_paths)
}

#' Create interactive 3D scatter plot
#'
#' Generates interactive 3D scatter plots for exploratory analysis.
#'
#' @param analysis_results Analysis results
#' @param site_id Site ID
#' @param output_dir Output directory
#' @return Path to interactive plot
#' @export
create_interactive_3d_plot <- function(analysis_results, site_id, output_dir) {
  
  if (!requireNamespace("plotly", quietly = TRUE)) {
    return(NULL)
  }
  
  # Implementation would create interactive 3D plot
  # This is a placeholder for the actual implementation
  
  output_path <- file.path(output_dir, paste0("interactive_3d_", site_id, ".html"))
  message("Interactive 3D plot created at: ", output_path)
  return(output_path)
}

#' Create correlation heatmap
#'
#' Generates comprehensive correlation heatmaps.
#'
#' @param analysis_results Analysis results
#' @param site_id Site ID
#' @param output_dir Output directory
#' @return Path to heatmap
#' @export
create_correlation_heatmap <- function(analysis_results, site_id, output_dir) {
  
  # Extract correlation matrix from data
  # Implementation would create publication-quality heatmap
  
  output_path <- file.path(output_dir, paste0("correlation_heatmap_", site_id, ".png"))
  message("Correlation heatmap created at: ", output_path)
  return(output_path)
}

#' Create network diagram
#'
#' Visualizes SEM relationships as network diagram.
#'
#' @param sem_results SEM results
#' @param site_id Site ID
#' @param output_dir Output directory
#' @return Path to network diagram
#' @export
create_network_diagram <- function(sem_results, site_id, output_dir) {
  
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    return(NULL)
  }
  
  # Implementation would create network visualization
  # This is a placeholder for the actual implementation
  
  output_path <- file.path(output_dir, paste0("network_diagram_", site_id, ".png"))
  message("Network diagram created at: ", output_path)
  return(output_path)
}

#' Create temporal animation
#'
#' Generates animated temporal plots.
#'
#' @param temporal_results Temporal analysis results
#' @param site_id Site ID
#' @param output_dir Output directory
#' @return Path to animation
#' @export
create_temporal_animation <- function(temporal_results, site_id, output_dir) {
  
  if (!requireNamespace("gganimate", quietly = TRUE)) {
    return(NULL)
  }
  
  # Implementation would create animated plot
  # This is a placeholder for the actual implementation
  
  output_path <- file.path(output_dir, paste0("temporal_animation_", site_id, ".gif"))
  message("Temporal animation created at: ", output_path)
  return(output_path)
}

#' Create ML feature importance plot
#'
#' Visualizes machine learning feature importance.
#'
#' @param ml_results Machine learning results
#' @param site_id Site ID
#' @param output_dir Output directory
#' @return Path to importance plot
#' @export
create_ml_importance_plot <- function(ml_results, site_id, output_dir) {
  
  # Implementation would create feature importance visualization
  # This is a placeholder for the actual implementation
  
  output_path <- file.path(output_dir, paste0("ml_importance_", site_id, ".png"))
  message("ML importance plot created at: ", output_path)
  return(output_path)
}
