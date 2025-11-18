#' Advanced Visualization Module for NEON Root-Soil Analysis
#'
#' This module provides publication-quality visualization functions for
#' ecological data analysis, with consistent themes, proper error representation,
#' and support for multi-site comparisons.
#'
#' @name visualization
#' @docType package
NULL

# Load required packages
library(tidyr)
library(ggplot2)

#' Create depth-stratified visualization of root chemistry
#'
#' Generates publication-quality plots showing root chemistry variables
#' across depth intervals with confidence intervals and proper ecological
#' formatting.
#'
#' @param data Data frame containing root chemistry and depth data
#' @param response_var Character string specifying response variable
#' @param depth_breaks Numeric vector of depth interval breaks
#' @param site_id Character string specifying site ID
#' @param group_var Character string specifying grouping variable (optional)
#' @param show_ci Logical indicating whether to show confidence intervals
#' @param theme_name Character string specifying ggplot theme
#'
#' @return ggplot object
#' @export
plot_depth_stratified <- function(data,
                                  response_var = "cn_ratio",
                                  depth_breaks = c(0, 10, 25, 50, 100, 200),
                                  site_id = "DEJU",
                                  group_var = NULL,
                                  show_ci = TRUE,
                                  theme_name = "theme_bw") {
  
  message(sprintf("Creating depth-stratified plot for %s at site %s", response_var, site_id))
  
  # Create depth categories
  plot_data <- data %>%
    mutate(
      depth_category = cut(
        depth_cm,
        breaks = depth_breaks,
        labels = sprintf("Depth_%d-%d", depth_breaks[-length(depth_breaks)], depth_breaks[-1]),
        include.lowest = TRUE
      ),
      depth_mid = (depth_breaks[-length(depth_breaks)] + depth_breaks[-1]) / 2
    )
  
  # Calculate summary statistics by depth and group
  if (!is.null(group_var) && group_var %in% names(plot_data)) {
    summary_data <- plot_data %>%
      group_by(depth_category, depth_mid, !!sym(group_var)) %>%
      summarise(
        mean_val = mean(.data[[response_var]], na.rm = TRUE),
        se = sd(.data[[response_var]], na.rm = TRUE) / sqrt(n()),
        n = n(),
        .groups = "drop"
      )
    
    if (show_ci) {
      summary_data <- summary_data %>%
        mutate(
          lower = mean_val - 1.96 * se,
          upper = mean_val + 1.96 * se
        )
    }
    
    # Create plot with grouping
    p <- ggplot(summary_data, aes(x = depth_mid, y = mean_val, color = !!sym(group_var))) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      {
        if (show_ci) geom_ribbon(aes(ymin = lower, ymax = upper, fill = !!sym(group_var)), 
                                 alpha = 0.2, color = NA)
      }
    
  } else {
    summary_data <- plot_data %>%
      group_by(depth_category, depth_mid) %>%
      summarise(
        mean_val = mean(.data[[response_var]], na.rm = TRUE),
        se = sd(.data[[response_var]], na.rm = TRUE) / sqrt(n()),
        n = n(),
        .groups = "drop"
      )
    
    if (show_ci) {
      summary_data <- summary_data %>%
        mutate(
          lower = mean_val - 1.96 * se,
          upper = mean_val + 1.96 * se
        )
    }
    
    # Create simple plot
    p <- ggplot(summary_data, aes(x = depth_mid, y = mean_val)) +
      geom_line(size = 1.2, color = "#2E7D32") +
      geom_point(size = 4, color = "#2E7D32") +
      {
        if (show_ci) geom_ribbon(aes(ymin = lower, ymax = upper), 
                                 alpha = 0.3, fill = "#2E7D32")
      }
  }
  
  # Add labels and formatting
  p <- p +
    labs(
      title = sprintf("Root %s Across Depth Intervals - %s", 
                      format_variable_name(response_var), site_id),
      subtitle = sprintf("Mean ± 95%% CI (n = %d samples)", sum(summary_data$n, na.rm = TRUE)),
      x = "Soil Depth (cm)",
      y = format_variable_name(response_var),
      color = if (!is.null(group_var)) format_variable_name(group_var),
      fill = if (!is.null(group_var)) format_variable_name(group_var)
    ) +
    scale_x_continuous(
      breaks = depth_breaks,
      labels = depth_breaks,
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    apply_ecological_theme(theme_name) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray40"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 11),
      legend.position = if (is.null(group_var)) "none" else "right",
      legend.title = element_text(size = 11, face = "bold")
    )
  
  return(p)
}

#' Create soil-root relationship visualization
#'
#' Generates scatter plots and regression lines showing relationships between
#' soil bulk density and root chemistry variables, with optional stratification
#' by depth or other factors.
#'
#' @param merged_data Data frame containing merged root and soil data
#' @param response_var Character string specifying root chemistry response
#' @param soil_predictor Character string specifying soil predictor variable
#' @param site_id Character string specifying site ID
#' @param stratify_var Character string specifying stratification variable (optional)
#' @param add_smooth Logical indicating whether to add smooth regression line
#' @param show_ci Logical indicating whether to show confidence intervals
#' @param theme_name Character string specifying ggplot theme
#'
#' @return ggplot object
#' @export
plot_soil_root_relationship <- function(merged_data,
                                        response_var = "cn_ratio",
                                        soil_predictor = "bulk_density",
                                        site_id = "DEJU",
                                        stratify_var = NULL,
                                        add_smooth = TRUE,
                                        show_ci = TRUE,
                                        theme_name = "theme_bw") {
  
  message(sprintf("Creating soil-root relationship plot for %s", site_id))
  
  # Validate data
  required_vars <- c(response_var, soil_predictor, "siteID")
  missing_vars <- setdiff(required_vars, names(merged_data))
  
  if (length(missing_vars) > 0) {
    stop(sprintf("Missing required variables: %s", paste(missing_vars, collapse = ", ")))
  }
  
  # Filter for current site and remove missing values
  plot_data <- merged_data %>%
    filter(siteID == site_id) %>%
    filter(!is.na(.data[[response_var]]), !is.na(.data[[soil_predictor]]))
  
  if (nrow(plot_data) < 10) {
    warning("Insufficient data for soil-root relationship visualization")
    return(NULL)
  }
  
  # Create base plot
  p <- ggplot(plot_data, aes(x = .data[[soil_predictor]], y = .data[[response_var]])) +
    geom_point(alpha = 0.7, size = 2) +
    labs(
      title = sprintf("Root %s vs Soil %s - %s", 
                      format_variable_name(response_var), 
                      format_variable_name(soil_predictor), 
                      site_id),
      subtitle = sprintf("n = %d paired observations", nrow(plot_data)),
      x = format_variable_name(soil_predictor, with_units = TRUE),
      y = format_variable_name(response_var, with_units = TRUE)
    )
  
  # Add stratification if specified
  if (!is.null(stratify_var) && stratify_var %in% names(plot_data)) {
    p <- p +
      aes(color = .data[[stratify_var]]) +
      scale_color_brewer(palette = "Set2", name = format_variable_name(stratify_var))
  }
  
  # Add regression line and confidence interval
  if (add_smooth) {
    if (show_ci) {
      p <- p + 
        geom_smooth(method = "lm", se = TRUE, alpha = 0.2, size = 1) +
        geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "gray40", size = 0.8)
    } else {
      p <- p + geom_smooth(method = "lm", se = FALSE, size = 1)
    }
  }
  
  # Add correlation coefficient
  correlation <- cor(plot_data[[soil_predictor]], plot_data[[response_var]], use = "complete.obs")
  if (!is.na(correlation)) {
    p <- p + 
      annotate("text", 
               x = Inf, y = Inf, 
               label = sprintf("r = %.3f", correlation),
               hjust = 1.1, vjust = 1.1,
               size = 4, color = "gray40")
  }
  
  # Apply theme and formatting
  p <- p +
    apply_ecological_theme(theme_name) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray40"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 11),
      legend.position = if (is.null(stratify_var)) "none" else "right",
      legend.title = element_text(size = 11, face = "bold")
    )
  
  return(p)
}

#' Create multi-site comparison visualization
#'
#' Generates faceted plots comparing root-soil relationships across multiple
#' NEON sites, suitable for cross-site ecological analysis.
#'
#' @param multi_site_data Data frame containing data from multiple sites
#' @param response_var Character string specifying response variable
#' @param predictor_var Character string specifying predictor variable
#' @param site_var Character string specifying site ID variable
#' @param facet_type Character string specifying facet type ("wrap" or "grid")
#' @param common_scale Logical indicating whether to use common y-axis scale
#' @param theme_name Character string specifying ggplot theme
#'
#' @return ggplot object
#' @export
plot_multi_site_comparison <- function(multi_site_data,
                                       response_var = "cn_ratio",
                                       predictor_var = "bulk_density",
                                       site_var = "siteID",
                                       facet_type = "wrap",
                                       common_scale = TRUE,
                                       theme_name = "theme_bw") {
  
  message("Creating multi-site comparison visualization")
  
  # Validate data
  required_vars <- c(response_var, predictor_var, site_var)
  missing_vars <- setdiff(required_vars, names(multi_site_data))
  
  if (length(missing_vars) > 0) {
    stop(sprintf("Missing required variables: %s", paste(missing_vars, collapse = ", ")))
  }
  
  # Get unique sites
  sites <- unique(multi_site_data[[site_var]])
  n_sites <- length(sites)
  
  if (n_sites < 2) {
    warning("Insufficient sites for multi-site comparison")
    return(NULL)
  }
  
  # Create base plot
  p <- ggplot(multi_site_data, aes(x = .data[[predictor_var]], y = .data[[response_var]])) +
    geom_point(alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.2, size = 1) +
    labs(
      title = sprintf("Root %s vs Soil %s Across NEON Sites", 
                      format_variable_name(response_var), 
                      format_variable_name(predictor_var)),
      subtitle = sprintf("%d sites analyzed", n_sites),
      x = format_variable_name(predictor_var, with_units = TRUE),
      y = format_variable_name(response_var, with_units = TRUE)
    )
  
  # Add faceting
  if (facet_type == "wrap") {
    p <- p + facet_wrap(vars(.data[[site_var]]), scales = if (common_scale) "fixed" else "free_y")
  } else {
    p <- p + facet_grid(vars(.data[[site_var]]), scales = if (common_scale) "fixed" else "free_y")
  }
  
  # Add site-specific correlation coefficients
  correlations <- multi_site_data %>%
    group_by(.data[[site_var]]) %>%
    summarise(
      correlation = cor(.data[[response_var]], .data[[predictor_var]], use = "complete.obs"),
      n = n(),
      .groups = "drop"
    ) %>%
    filter(!is.na(correlation))
  
  if (nrow(correlations) > 0) {
    p <- p + 
      geom_text(
        data = correlations,
        aes(x = Inf, y = Inf, label = sprintf("r = %.3f\\nn = %d", correlation, n)),
        hjust = 1.1, vjust = 1.1,
        size = 3, color = "gray40"
      )
  }
  
  # Apply theme and formatting
  p <- p +
    apply_ecological_theme(theme_name) +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray40"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      strip.text = element_text(size = 11, face = "bold"),
      panel.spacing = unit(1, "lines")
    )
  
  return(p)
}

#' Create depth profile visualization
#'
#' Generates comprehensive depth profiles showing multiple variables
#' across soil depth, suitable for publication.
#'
#' @param root_data Data frame containing root chemistry data
#' @param soil_data Data frame containing soil bulk density data
#' @param site_id Character string specifying site ID
#' @param variables Character vector of variables to plot
#' @param depth_range Numeric vector specifying depth range to display
#' @param theme_name Character string specifying ggplot theme
#'
#' @return ggplot object
#' @export
plot_depth_profiles <- function(root_data,
                                soil_data,
                                site_id = "DEJU",
                                variables = c("cn_ratio", "carbonPercent", "nitrogenPercent"),
                                depth_range = c(0, 200),
                                theme_name = "theme_bw") {
  
  message(sprintf("Creating depth profiles for site %s", site_id))
  
  # Prepare root data
  # Check if we have depth range columns, otherwise use depth_cm directly
  if (all(c("depth_cm_top", "depth_cm_bottom") %in% names(root_data))) {
    root_summary <- root_data %>%
      mutate(depth_mid = (depth_cm_top + depth_cm_bottom) / 2) %>%
      group_by(depth_mid) %>%
      summarise(
        across(all_of(variables), list(mean = ~mean(., na.rm = TRUE), 
                                       se = ~sd(., na.rm = TRUE) / sqrt(n()),
                                       n = ~n()), 
               .groups = "drop")
      ) %>%
      pivot_longer(cols = -depth_mid, names_to = c("variable", "statistic"), 
                   names_pattern = "(.*)_(.*)", values_to = "value") %>%
      pivot_wider(names_from = statistic, values_from = value) %>%
      mutate(
        lower = mean - 1.96 * se,
        upper = mean + 1.96 * se
      )
  } else {
    # Use depth_cm directly if no range columns available
    root_summary <- root_data %>%
      group_by(depth_cm) %>%
      summarise(
        across(all_of(variables), list(mean = ~mean(., na.rm = TRUE), 
                                       se = ~sd(., na.rm = TRUE) / sqrt(n()),
                                       n = ~n())), 
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(cols = -depth_cm, names_to = c("variable", "statistic"), 
                          names_pattern = "(.*)_(.*)", values_to = "value") %>%
      tidyr::pivot_wider(names_from = statistic, values_from = value) %>%
      mutate(
        depth_mid = depth_cm,
        lower = mean - 1.96 * se,
        upper = mean + 1.96 * se
      )
  }
  
  # Prepare soil data
  soil_summary <- soil_data %>%
    mutate(depth_mid = (depth_cm_top + depth_cm_bottom) / 2) %>%
    group_by(depth_mid) %>%
    summarise(
      bulk_density_mean = mean(bulk_density, na.rm = TRUE),
      bulk_density_se = sd(bulk_density, na.rm = TRUE) / sqrt(n()),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(
      bulk_density_lower = bulk_density_mean - 1.96 * bulk_density_se,
      bulk_density_upper = bulk_density_mean + 1.96 * bulk_density_se
    )
  
  # Create combined plot
  p <- ggplot() +
    # Root chemistry variables
    geom_ribbon(
      data = root_summary %>% filter(variable == "cn_ratio"),
      aes(x = depth_mid, ymin = lower, ymax = upper),
      fill = "#9C27B0", alpha = 0.3
    ) +
    geom_line(
      data = root_summary %>% filter(variable == "cn_ratio"),
      aes(x = depth_mid, y = mean),
      color = "#9C27B0", size = 1.2
    ) +
    geom_point(
      data = root_summary %>% filter(variable == "cn_ratio"),
      aes(x = depth_mid, y = mean),
      color = "#9C27B0", size = 3
    ) +
    # Bulk density
    geom_ribbon(
      data = soil_summary,
      aes(x = depth_mid, ymin = bulk_density_lower, ymax = bulk_density_upper),
      fill = "#795548", alpha = 0.3
    ) +
    geom_line(
      data = soil_summary,
      aes(x = depth_mid, y = bulk_density_mean),
      color = "#795548", size = 1.2, linetype = "dashed"
    ) +
    geom_point(
      data = soil_summary,
      aes(x = depth_mid, y = bulk_density_mean),
      color = "#795548", size = 3, shape = 17
    ) +
    labs(
      title = sprintf("Depth Profiles of Root Chemistry and Soil Properties - %s", site_id),
      subtitle = "Mean ± 95% CI",
      x = "Soil Depth (cm)",
      y = "Value"
    ) +
    scale_x_continuous(
      limits = depth_range,
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(
      name = "Root C:N Ratio",
      sec.axis = sec_axis(~ . * 0.02 + 0.5, name = "Bulk Density (g/cm³)")
    ) +
    apply_ecological_theme(theme_name) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray40"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 11),
      legend.position = "right"
    )
  
  return(p)
}

#' Create root size comparison visualization
#'
#' Generates bar plots comparing root chemistry between size classes
#' with proper error representation and statistical significance indicators.
#'
#' @param data Data frame containing root chemistry data
#' @param variables Character vector of variables to compare
#' @param site_id Character string specifying site ID
#' @param size_break Numeric value specifying size class breakpoint
#' @param show_stats Logical indicating whether to show statistical tests
#' @param theme_name Character string specifying ggplot theme
#'
#' @return ggplot object
#' @export
plot_root_size_comparison <- function(data,
                                      variables = c("carbonPercent", "nitrogenPercent", "cn_ratio"),
                                      site_id = "DEJU",
                                      size_break = 4,
                                      show_stats = TRUE,
                                      theme_name = "theme_bw") {
  
  message(sprintf("Creating root size comparison for site %s", site_id))
  
  # Create size categories
  plot_data <- data %>%
    mutate(
      size_category = case_when(
        carbonPercent <= size_break ~ "Fine (≤4mm)",
        carbonPercent > size_break ~ "Coarse (>4mm)",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(size_category))
  
  if (nrow(plot_data) < 10) {
    warning("Insufficient data for root size comparison")
    return(NULL)
  }
  
  # Calculate summary statistics
  summary_data <- plot_data %>%
    group_by(size_category) %>%
    summarise(
      across(all_of(variables), list(
        mean = ~mean(., na.rm = TRUE),
        se = ~sd(., na.rm = TRUE) / sqrt(n()),
        n = ~n()
      ), .groups = "drop")
    ) %>%
    pivot_longer(cols = -size_category, names_to = c("variable", "statistic"), 
                 names_pattern = "(.*)_(.*)", values_to = "value") %>%
    pivot_wider(names_from = statistic, values_from = value) %>%
    mutate(
      lower = mean - 1.96 * se,
      upper = mean + 1.96 * se
    )
  
  # Perform statistical tests
  stats_results <- NULL
  if (show_stats) {
    stats_results <- perform_size_class_tests(plot_data, variables)
  }
  
  # Create plot
  p <- ggplot(summary_data, aes(x = size_category, y = mean, fill = variable)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
    geom_errorbar(
      aes(ymin = lower, ymax = upper),
      position = position_dodge(width = 0.9),
      width = 0.2,
      size = 0.5
    ) +
    geom_text(
      aes(label = sprintf("%.1f", mean), y = mean + se + 0.5),
      position = position_dodge(width = 0.9),
      vjust = -0.5,
      size = 3
    ) +
    scale_fill_brewer(palette = "Set2", name = "Variable") +
    labs(
      title = sprintf("Root Chemistry by Size Class - %s", site_id),
      subtitle = sprintf("Mean ± 95%% CI (n = %d total samples)", sum(summary_data$n, na.rm = TRUE)),
      x = "Root Size Class",
      y = "Mean Value"
    ) +
    apply_ecological_theme(theme_name) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray40"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 11),
      legend.position = "right",
      legend.title = element_text(size = 11, face = "bold")
    )
  
  # Add statistical annotations if available
  if (!is.null(stats_results) && nrow(stats_results) > 0) {
    p <- p + 
      geom_text(
        data = stats_results,
        aes(x = 1.5, y = Inf, label = significance_label),
        vjust = 1.5,
        size = 3,
        color = "red"
      )
  }
  
  return(p)
}

#' Apply consistent ecological theme to plots
#'
#' Applies a standardized theme suitable for ecological publications.
#'
#' @param theme_name Character string specifying base theme
#' @return Theme object
#' @export
apply_ecological_theme <- function(theme_name = "theme_bw") {
  
  base_theme <- switch(theme_name,
                       "theme_bw" = theme_bw(),
                       "theme_minimal" = theme_minimal(),
                       "theme_classic" = theme_classic(),
                       "theme_gray" = theme_gray(),
                       theme_bw())
  
  ecological_theme <- base_theme +
    theme(
      text = element_text(family = "sans", size = 12),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, color = "gray40", hjust = 0.5),
      axis.title = element_text(size = 12, face = "plain"),
      axis.text = element_text(size = 11, color = "black"),
      axis.line = element_line(color = "black", size = 0.5),
      panel.grid.major = element_line(color = "gray90", size = 0.2),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white", color = NA),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      strip.background = element_rect(fill = "gray95", color = "gray70"),
      strip.text = element_text(size = 11, face = "bold")
    )
  
  return(ecological_theme)
}

#' Format variable names for plot labels
#'
#' Converts variable names to human-readable format with proper units.
#'
#' @param var_name Character string specifying variable name
#' @param with_units Logical indicating whether to include units
#' @return Formatted variable name
#' @export
format_variable_name <- function(var_name, with_units = FALSE) {
  
  name_map <- list(
    "carbonPercent" = "Carbon Content",
    "nitrogenPercent" = "Nitrogen Content",
    "cn_ratio" = "C:N Ratio",
    "bulk_density" = "Bulk Density",
    "depth_cm" = "Depth",
    "sizeCategory" = "Root Size Class",
    "siteID" = "Site",
    "horizonName" = "Soil Horizon",
    "pitID" = "Sampling Pit"
  )
  
  unit_map <- list(
    "carbonPercent" = "(% dry mass)",
    "nitrogenPercent" = "(% dry mass)",
    "cn_ratio" = "(mass ratio)",
    "bulk_density" = "(g/cm³)",
    "depth_cm" = "(cm)"
  )
  
  formatted_name <- name_map[[var_name]] %||% var_name
  
  if (with_units && var_name %in% names(unit_map)) {
    formatted_name <- paste(formatted_name, unit_map[[var_name]])
  }
  
  return(formatted_name)
}

#' Perform statistical tests for root size class comparisons
#'
#' Conducts formal statistical tests comparing root chemistry between size classes.
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
    fine_data <- data %>% filter(size_category == "Fine (≤4mm)") %>% pull(!!sym(var))
    coarse_data <- data %>% filter(size_category == "Coarse (>4mm)") %>% pull(!!sym(var))
    
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

#' Create publication-ready figure set
#'
#' Generates a complete set of publication-quality figures for the analysis.
#'
#' @param data_list List containing all processed data
#' @param site_id Character string specifying site ID
#' @param output_dir Character string specifying output directory
#' @param dpi Numeric specifying figure resolution
#' @param width Numeric specifying figure width
#' @param height Numeric specifying figure height
#' @return List containing paths to generated figures
#' @export
create_publication_figures <- function(data_list,
                                       site_id = "DEJU",
                                       output_dir = "figures",
                                       dpi = 300,
                                       width = 8,
                                       height = 6) {
  
  message(sprintf("Creating publication figure set for site %s", site_id))
  
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  figure_paths <- list()
  
  # Figure 1: Root chemistry distributions
  figure_paths$nitrogen_distribution <- file.path(output_dir, "nitrogen_distribution.png")
  ggsave(figure_paths$nitrogen_distribution,
         plot = plot_nitrogen_distribution(data_list$root_chemistry, site_id = site_id),
         width = width, height = height, dpi = dpi, bg = "white")
  
  figure_paths$carbon_nitrogen_relationship <- file.path(output_dir, "carbon_nitrogen_relationship.png")
  ggsave(figure_paths$carbon_nitrogen_relationship,
         plot = plot_carbon_nitrogen_relationship(data_list$root_chemistry, site_id = site_id),
         width = width, height = height, dpi = dpi, bg = "white")
  
  figure_paths$cn_ratio_distribution <- file.path(output_dir, "cn_ratio_distribution.png")
  ggsave(figure_paths$cn_ratio_distribution,
         plot = plot_cn_ratio_distribution(data_list$root_chemistry, site_id = site_id),
         width = width, height = height, dpi = dpi, bg = "white")
  
  # Figure 2: Root size comparison
  figure_paths$root_size_comparison <- file.path(output_dir, "root_size_comparison.png")
  ggsave(figure_paths$root_size_comparison,
         plot = plot_root_size_comparison(data_list$root_chemistry, site_id = site_id),
         width = width, height = height, dpi = dpi, bg = "white")
  
  # Figure 3: Soil-root relationships
  if (!is.null(data_list$merged_data)) {
    figure_paths$soil_nitrogen_relationship <- file.path(output_dir, "soil_nitrogen_relationship.png")
    ggsave(figure_paths$soil_nitrogen_relationship,
           plot = plot_soil_root_relationship(data_list$merged_data, site_id = site_id),
           width = width, height = height, dpi = dpi, bg = "white")
    
    figure_paths$soil_carbon_relationship <- file.path(output_dir, "soil_carbon_relationship.png")
    ggsave(figure_paths$soil_carbon_relationship,
           plot = plot_soil_root_relationship(data_list$merged_data, response_var = "carbonPercent", site_id = site_id),
           width = width, height = height, dpi = dpi, bg = "white")
    
    figure_paths$soil_cn_ratio_relationship <- file.path(output_dir, "soil_cn_ratio_relationship.png")
    ggsave(figure_paths$soil_cn_ratio_relationship,
           plot = plot_soil_root_relationship(data_list$merged_data, response_var = "cn_ratio", site_id = site_id),
           width = width, height = height, dpi = dpi, bg = "white")
  }
  
  # Figure 4: Depth profiles
  if (!is.null(data_list$root_chemistry) && !is.null(data_list$soil_bulk_density)) {
    figure_paths$depth_profiles <- file.path(output_dir, "depth_profiles.png")
    ggsave(figure_paths$depth_profiles,
           plot = plot_depth_profiles(data_list$root_chemistry, data_list$soil_bulk_density, site_id = site_id),
           width = width, height = height * 1.2, dpi = dpi, bg = "white")
  }
  
  message(sprintf("✓ Generated %d publication-quality figures", length(figure_paths)))
  return(figure_paths)
}

#' Helper function for NULL-coalescing
#'
#' @param x Value to check
#' @param y Default value
#' @return x if not NULL, otherwise y
`%||%` <- function(x, y) if (is.null(x)) y else x
