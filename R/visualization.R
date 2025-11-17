#' Visualization Functions for NEON Root Chemistry Analysis
#'
#' This script contains functions for creating high-quality visualizations
#' of root chemistry and soil bulk density relationships.
#'
#' @author Sergio Ocampo
#' @date November 2025

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(RColorBrewer)

#' Create nitrogen distribution histogram
#'
#' @param root_data Root chemistry data frame
#' @return ggplot object
#' @export
plot_nitrogen_distribution <- function(root_data) {
  if (!"nitrogenPercent" %in% names(root_data)) {
    stop("Root data must contain nitrogenPercent column")
  }
  
  p <- ggplot(root_data, aes(x = nitrogenPercent)) +
    geom_histogram(
      binwidth = 0.05,
      fill = "#2E8B57",
      color = "black",
      alpha = 0.8
    ) +
    labs(
      title = "Distribution of Root Nitrogen Content",
      subtitle = "NEON DEJU Site",
      x = "Nitrogen Content (%)",
      y = "Number of Samples"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 12),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

#' Create carbon vs nitrogen scatter plot
#'
#' @param root_data Root chemistry data frame
#' @return ggplot object
#' @export
plot_carbon_nitrogen_relationship <- function(root_data) {
  required_cols <- c("carbonPercent", "nitrogenPercent")
  missing_cols <- setdiff(required_cols, names(root_data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Calculate correlation
  cor_value <- cor(root_data$carbonPercent, root_data$nitrogenPercent, 
                   use = "complete.obs", method = "pearson")
  
  p <- ggplot(root_data, aes(x = carbonPercent, y = nitrogenPercent)) +
    geom_point(
      alpha = 0.6,
      color = "#D2691E",
      size = 3
    ) +
    geom_smooth(
      method = "lm",
      se = TRUE,
      color = "black",
      linetype = "dashed"
    ) +
    labs(
      title = "Root Carbon vs Nitrogen Relationship",
      subtitle = paste("NEON DEJU Site | r =", round(cor_value, 3)),
      x = "Carbon Content (%)",
      y = "Nitrogen Content (%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 12),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

#' Create C:N ratio distribution histogram
#'
#' @param root_data Root chemistry data frame
#' @return ggplot object
#' @export
plot_cn_ratio_distribution <- function(root_data) {
  if (!"cn_ratio" %in% names(root_data)) {
    stop("Root data must contain cn_ratio column")
  }
  
  p <- ggplot(root_data, aes(x = cn_ratio)) +
    geom_histogram(
      binwidth = 5,
      fill = "#9370DB",
      color = "black",
      alpha = 0.8
    ) +
    labs(
      title = "Distribution of Root C:N Ratios",
      subtitle = "NEON DEJU Site",
      x = "C:N Ratio",
      y = "Number of Samples"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 12),
      panel.grid.minor = element_blank()
    ) +
    xlim(0, 150)  # Focus on main distribution
  
  return(p)
}

#' Create comparison plot by root size category
#'
#' @param root_data Root chemistry data frame with sizeCategory
#' @return ggplot object
#' @export
plot_root_size_comparison <- function(root_data) {
  required_cols <- c("sizeCategory", "carbonPercent", "nitrogenPercent")
  missing_cols <- setdiff(required_cols, names(root_data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Prepare data for plotting
  plot_data <- root_data %>%
    select(sizeCategory, carbonPercent, nitrogenPercent) %>%
    pivot_longer(
      cols = c(carbonPercent, nitrogenPercent),
      names_to = "element",
      values_to = "content"
    ) %>%
    mutate(
      element = recode(element,
                       "carbonPercent" = "Carbon",
                       "nitrogenPercent" = "Nitrogen"),
      sizeCategory = recode(sizeCategory,
                            "<=4mm" = "Fine Roots (≤4mm)",
                            ">4mm" = "Coarse Roots (>4mm)")
    )
  
  # Calculate summary statistics
  summary_stats <- plot_data %>%
    group_by(sizeCategory, element) %>%
    summarise(
      mean_content = mean(content, na.rm = TRUE),
      se = sd(content, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  p <- ggplot(summary_stats, aes(x = sizeCategory, y = mean_content, fill = element)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
    geom_errorbar(
      aes(ymin = mean_content - se, ymax = mean_content + se),
      position = position_dodge(width = 0.8),
      width = 0.2,
      color = "black"
    ) +
    geom_text(
      aes(label = round(mean_content, 1)),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      size = 4
    ) +
    scale_fill_manual(
      values = c("Carbon" = "#4169E1", "Nitrogen" = "#DC143C"),
      name = "Element"
    ) +
    labs(
      title = "Root Chemistry by Size Category",
      subtitle = "NEON DEJU Site",
      x = "Root Size Category",
      y = "Mean Content (%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 12),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

#' Create soil bulk density vs root chemistry scatter plot
#'
#' @param merged_data Data frame containing both root and soil data
#' @param chemistry_var Chemistry variable to plot ("nitrogenPercent", "carbonPercent", or "cn_ratio")
#' @return ggplot object
#' @export
plot_soil_root_relationship <- function(merged_data, chemistry_var = "nitrogenPercent") {
  valid_vars <- c("nitrogenPercent", "carbonPercent", "cn_ratio")
  if (!chemistry_var %in% valid_vars) {
    stop(paste("chemistry_var must be one of:", paste(valid_vars, collapse = ", ")))
  }
  
  if (!"bulk_density" %in% names(merged_data)) {
    stop("Merged data must contain bulk_density column")
  }
  
  # Get appropriate labels
  var_labels <- list(
    nitrogenPercent = list(
      title = "Soil Bulk Density vs Root Nitrogen",
      y = "Root Nitrogen Content (%)"
    ),
    carbonPercent = list(
      title = "Soil Bulk Density vs Root Carbon", 
      y = "Root Carbon Content (%)"
    ),
    cn_ratio = list(
      title = "Soil Bulk Density vs Root C:N Ratio",
      y = "Root C:N Ratio"
    )
  )
  
  # Calculate correlation
  cor_value <- cor(merged_data$bulk_density, merged_data[[chemistry_var]], 
                   use = "complete.obs", method = "pearson")
  
  p <- ggplot(merged_data, aes(x = bulk_density, y = .data[[chemistry_var]])) +
    geom_point(
      alpha = 0.6,
      color = "#228B22",
      size = 3
    ) +
    geom_smooth(
      method = "lm",
      se = TRUE,
      color = "black",
      linetype = "dashed"
    ) +
    labs(
      title = var_labels[[chemistry_var]]$title,
      subtitle = paste("NEON DEJU Site | r =", round(cor_value, 3)),
      x = "Soil Bulk Density (g/cm³)",
      y = var_labels[[chemistry_var]]$y
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 12),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

#' Create depth profile plot
#'
#' @param root_data Root chemistry data frame with depth information
#' @param soil_data Soil bulk density data frame with depth information
#' @return ggplot object
#' @export
plot_depth_profiles <- function(root_data, soil_data) {
  required_root_cols <- c("depth_cm", "cn_ratio")
  required_soil_cols <- c("depth_cm", "bulk_density")
  
  missing_root_cols <- setdiff(required_root_cols, names(root_data))
  missing_soil_cols <- setdiff(required_soil_cols, names(soil_data))
  
  if (length(missing_root_cols) > 0) {
    stop(paste("Root data missing columns:", paste(missing_root_cols, collapse = ", ")))
  }
  if (length(missing_soil_cols) > 0) {
    stop(paste("Soil data missing columns:", paste(missing_soil_cols, collapse = ", ")))
  }
  
  # Prepare root data
  root_depth <- root_data %>%
    filter(!is.na(depth_cm), !is.na(cn_ratio)) %>%
    group_by(depth_cm) %>%
    summarise(
      mean_cn = mean(cn_ratio, na.rm = TRUE),
      se = sd(cn_ratio, na.rm = TRUE) / sqrt(n()),
      n = n(),
      .groups = "drop"
    )
  
  # Prepare soil data
  soil_depth <- soil_data %>%
    filter(!is.na(depth_cm), !is.na(bulk_density)) %>%
    group_by(depth_cm) %>%
    summarise(
      mean_bd = mean(bulk_density, na.rm = TRUE),
      se = sd(bulk_density, na.rm = TRUE) / sqrt(n()),
      n = n(),
      .groups = "drop"
    )
  
  # Create plot
  p <- ggplot() +
    # Root C:N ratio
    geom_line(
      data = root_depth,
      aes(x = mean_cn, y = depth_cm, color = "Root C:N Ratio"),
      size = 1.2
    ) +
    geom_point(
      data = root_depth,
      aes(x = mean_cn, y = depth_cm, color = "Root C:N Ratio"),
      size = 3
    ) +
    # Soil bulk density
    geom_line(
      data = soil_depth,
      aes(x = mean_bd * 20, y = depth_cm, color = "Soil Bulk Density"),
      size = 1.2,
      linetype = "dashed"
    ) +
    geom_point(
      data = soil_depth,
      aes(x = mean_bd * 20, y = depth_cm, color = "Soil Bulk Density"),
      size = 3,
      shape = 17
    ) +
    scale_color_manual(
      values = c("Root C:N Ratio" = "#8B4513", "Soil Bulk Density" = "#4682B4"),
      name = "Variable"
    ) +
    labs(
      title = "Depth Profiles: Root C:N Ratio and Soil Bulk Density",
      subtitle = "NEON DEJU Site",
      x = "Root C:N Ratio | Soil Bulk Density (×20 g/cm³)",
      y = "Depth (cm)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 12),
      legend.position = "top",
      panel.grid.minor = element_blank()
    ) +
    scale_y_reverse()  # Reverse y-axis to show depth increasing downward
  
  return(p)
}

#' Save multiple plots to files
#'
#' @param plot_list List of ggplot objects
#' @param output_dir Output directory for plots
#' @param width Plot width in inches
#' @param height Plot height in inches
#' @param dpi Resolution in dots per inch
#' @export
save_plots <- function(plot_list, output_dir = "figures", width = 8, height = 6, dpi = 300) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  for (plot_name in names(plot_list)) {
    output_file <- file.path(output_dir, paste0(plot_name, ".png"))
    ggsave(
      filename = output_file,
      plot = plot_list[[plot_name]],
      width = width,
      height = height,
      dpi = dpi,
      bg = "white"
    )
    message(paste("Saved plot:", output_file))
  }
}
