#' Process NEON Root Chemistry and Soil Data
#'
#' This script contains functions for processing and cleaning NEON data
#' related to root chemistry and soil bulk density measurements.
#'
#' @author Sergio Ocampo
#' @date November 2025

library(dplyr)
library(tidyr)
library(readr)
library(stringr)

#' Clean and filter root chemistry data
#'
#' Removes samples with missing or invalid carbon/nitrogen data
#' and calculates C:N ratios.
#'
#' @param cn_data Data frame containing carbon and nitrogen data
#' @return Cleaned data frame with calculated C:N ratios
#' @export
clean_root_chemistry <- function(cn_data) {
  # Input validation
  if (!is.data.frame(cn_data)) {
    stop("Input must be a data frame")
  }
  
  required_cols <- c("cnSampleID", "carbonPercent", "nitrogenPercent")
  missing_cols <- setdiff(required_cols, names(cn_data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Filter out samples with missing or zero values
  cleaned_data <- cn_data %>%
    filter(
      !is.na(carbonPercent),
      !is.na(nitrogenPercent),
      carbonPercent > 0,
      nitrogenPercent > 0,
      carbonPercent < 100,  # Remove obvious outliers
      nitrogenPercent < 10   # Remove obvious outliers
    ) %>%
    mutate(
      cn_ratio = carbonPercent / nitrogenPercent,
      sample_id = cnSampleID
    )
  
  return(cleaned_data)
}

#' Extract depth information from sample IDs
#'
#' Parses NEON sample IDs to extract depth information.
#'
#' @param sample_ids Character vector of sample IDs
#' @return Data frame with depth information
#' @export
extract_depth_info <- function(sample_ids) {
  if (!is.character(sample_ids)) {
    stop("Sample IDs must be character vector")
  }
  
  # Extract depth from sample ID pattern (e.g., "DEJU.1.20.LIVE.<4MM")
  depth_info <- data.frame(
    sample_id = sample_ids,
    depth_category = str_extract(sample_ids, "\\d+\\.\\d+"),
    root_status = str_extract(sample_ids, "LIVE|DEAD"),
    size_class = str_extract(sample_ids, "<4MM|>4MM")
  )
  
  # Convert depth to numeric - extract the second number (depth in cm)
  depth_info$depth_cm <- as.numeric(str_extract(depth_info$depth_category, "\\d+$"))
  
  # Handle cases where depth extraction fails
  depth_info$depth_cm[is.na(depth_info$depth_cm)] <- 0
  
  # Convert size class to standard format
  depth_info$size_class <- ifelse(depth_info$size_class == "<4MM", "<=4mm", ">4mm")
  
  return(depth_info)
}

#' Merge root chemistry with sample metadata
#'
#' Joins carbon/nitrogen data with root sample metadata including size categories.
#'
#' @param cn_data Root chemistry data frame
#' @param sample_data Root sample metadata data frame
#' @return Merged data frame
#' @export
merge_root_data <- function(cn_data, sample_data) {
  # Input validation
  if (!is.data.frame(cn_data) || !is.data.frame(sample_data)) {
    stop("Both inputs must be data frames")
  }
  
  # Standardize column names for joining
  if ("cnSampleID" %in% names(cn_data) && "sampleID" %in% names(sample_data)) {
    merged_data <- cn_data %>%
      left_join(sample_data, by = c("cnSampleID" = "sampleID"))
  } else {
    warning("Could not find matching columns for join. Attempting fuzzy join.")
    merged_data <- cn_data %>%
      left_join(sample_data, by = "sample_id")
  }
  
  return(merged_data)
}

#' Clean and process soil bulk density data
#'
#' Filters soil data for valid bulk density measurements.
#'
#' @param soil_data Soil bulk density data frame
#' @return Cleaned soil data frame
#' @export
clean_soil_data <- function(soil_data) {
  if (!is.data.frame(soil_data)) {
    stop("Input must be a data frame")
  }
  
  required_cols <- c("bulkDensExclCoarseFrag", "bulkDensCenterDepth")
  missing_cols <- setdiff(required_cols, names(soil_data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  cleaned_soil <- soil_data %>%
    filter(
      !is.na(bulkDensExclCoarseFrag),
      !is.na(bulkDensCenterDepth),
      bulkDensExclCoarseFrag > 0,
      bulkDensExclCoarseFrag < 3,  # Reasonable range for soil bulk density
      bulkDensCenterDepth >= 0
    ) %>%
    mutate(
      depth_cm = bulkDensCenterDepth,
      bulk_density = bulkDensExclCoarseFrag
    )
  
  return(cleaned_soil)
}

#' Create depth categories for analysis
#'
#' Creates meaningful depth categories for ecological analysis.
#'
#' @param depth_cm Numeric vector of depths in cm
#' @param breaks Optional custom depth breaks
#' @return Character vector of depth categories
#' @export
create_depth_categories <- function(depth_cm, breaks = NULL) {
  if (is.null(breaks)) {
    breaks <- c(0, 10, 30, 60, 100, 300)
  }
  
  # Create labels based on the breaks provided
  n_breaks <- length(breaks)
  if (n_breaks == 6) {
    labels <- c("0-10cm", "10-30cm", "30-60cm", "60-100cm", "100cm+")
  } else {
    # Generate labels for custom breaks
    labels <- sapply(1:(n_breaks-1), function(i) {
      paste0(breaks[i], "-", breaks[i+1], "cm")
    })
  }
  
  depth_categories <- cut(
    depth_cm,
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE
  )
  
  return(as.character(depth_categories))
}

#' Load and process all NEON data
#'
#' Main function to load raw data files and process them.
#'
#' @param data_dir Directory containing raw data files
#' @return List containing processed data frames
#' @export
process_neon_data <- function(data_dir = "data/raw_data") {
  # Load raw data
  message("Loading raw data files...")
  
  cn_data <- tryCatch({
    read_csv(file.path(data_dir, "megapit_carbon_nitrogen.csv"))
  }, error = function(e) {
    stop("Could not load carbon/nitrogen data: ", e$message)
  })
  
  sample_data <- tryCatch({
    read_csv(file.path(data_dir, "megapit_root_samples.csv"))
  }, error = function(e) {
    warning("Could not load root sample data: ", e$message)
    NULL
  })
  
  soil_data <- tryCatch({
    read_csv(file.path(data_dir, "soil_bulk_density.csv"))
  }, error = function(e) {
    stop("Could not load soil bulk density data: ", e$message)
  })
  
  # Process data
  message("Processing root chemistry data...")
  clean_cn <- clean_root_chemistry(cn_data)
  
  message("Processing soil data...")
  clean_soil <- clean_soil_data(soil_data)
  
  # Merge with sample metadata if available
  if (!is.null(sample_data)) {
    message("Merging root data with sample metadata...")
    merged_roots <- merge_root_data(clean_cn, sample_data)
  } else {
    merged_roots <- clean_cn
  }
  
  # Add depth categories
  if ("depth_cm" %in% names(merged_roots)) {
    merged_roots <- merged_roots %>%
      mutate(depth_category = create_depth_categories(depth_cm))
  }
  
  if ("depth_cm" %in% names(clean_soil)) {
    clean_soil <- clean_soil %>%
      mutate(depth_category = create_depth_categories(depth_cm))
  }
  
  message("Data processing complete!")
  
  return(list(
    root_chemistry = merged_roots,
    soil_bulk_density = clean_soil,
    original_cn = cn_data,
    original_soil = soil_data
  ))
}
