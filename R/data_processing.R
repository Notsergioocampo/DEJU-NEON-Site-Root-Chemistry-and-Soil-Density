#' Data Processing and Cleaning Module for NEON Root-Soil Analysis
#'
#' This module provides functions for processing and cleaning NEON data,
#' including depth parsing, data validation, and preparation for statistical analysis.
#'
#' @name data_processing
#' @docType package
NULL

# Load required packages
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)

#' Clean root chemistry data
#'
#' Removes invalid data and calculates C:N ratios from root chemistry data.
#'
#' @param data Data frame containing root chemistry data
#' @return Cleaned data frame with C:N ratios
#' @export
clean_root_chemistry <- function(data) {
  
  # Check for required columns
  required_cols <- c("carbonPercent", "nitrogenPercent")
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }
  
  # Remove rows with missing critical data
  cleaned <- data %>%
    filter(!is.na(carbonPercent), !is.na(nitrogenPercent)) %>%
    # Remove zero nitrogen values
    filter(nitrogenPercent > 0) %>%
    # Calculate C:N ratio
    mutate(cn_ratio = carbonPercent / nitrogenPercent)
  
  # Remove extreme outliers (beyond 4 standard deviations) if we have enough data
  if (nrow(cleaned) > 5) {
    cleaned <- cleaned %>%
      filter(
        abs(carbonPercent - mean(carbonPercent, na.rm = TRUE)) <= 4 * sd(carbonPercent, na.rm = TRUE),
        abs(nitrogenPercent - mean(nitrogenPercent, na.rm = TRUE)) <= 4 * sd(nitrogenPercent, na.rm = TRUE),
        abs(cn_ratio - mean(cn_ratio, na.rm = TRUE)) <= 4 * sd(cn_ratio, na.rm = TRUE)
      )
  }
  
  return(cleaned)
}

#' Clean soil bulk density data
#'
#' Filters invalid measurements and standardizes soil bulk density data.
#'
#' @param data Data frame containing soil bulk density data
#' @return Cleaned data frame
#' @export
clean_soil_data <- function(data) {
  
  # Remove rows with missing bulk density
  cleaned <- data %>%
    filter(!is.na(bulkDensExclCoarseFrag)) %>%
    # Remove negative depths
    filter(bulkDensCenterDepth >= 0) %>%
    # Rename columns for consistency
    rename(
      bulk_density = bulkDensExclCoarseFrag,
      depth_cm = bulkDensCenterDepth
    )
  
  # Remove extreme outliers if we have enough data
  if (nrow(cleaned) > 5) {
    cleaned <- cleaned %>%
      filter(
        abs(bulk_density - mean(bulk_density, na.rm = TRUE)) <= 4 * sd(bulk_density, na.rm = TRUE)
      )
  }
  
  return(cleaned)
}

#' Extract depth information from sample IDs
#'
#' Parses depth information from NEON sample ID format.
#'
#' @param sample_ids Character vector of sample IDs
#' @return Data frame with depth information
#' @export
extract_depth_info <- function(sample_ids) {
  
  if (length(sample_ids) == 0) {
    return(tibble(sample_id = character(), depth_cm = numeric()))
  }
  
  # Extract depth from sample ID format: SITE.X.XX.STATUS.SIZE
  depth_info <- tibble(sample_id = sample_ids) %>%
    mutate(
      # Split by dots and extract depth component
      parts = stringr::str_split(sample_id, "\\."),
      depth_part = purrr::map_chr(parts, ~ .x[3] %||% NA_character_)
    ) %>%
    # Convert depth part to numeric depth in cm
    mutate(
      depth_cm = case_when(
        !is.na(depth_part) & nchar(depth_part) >= 2 ~ as.numeric(stringr::str_sub(depth_part, 1, 2)),
        !is.na(depth_part) & nchar(depth_part) == 1 ~ as.numeric(depth_part) * 10,
        TRUE ~ NA_real_
      ),
      depth_cm = ifelse(depth_cm > 0, depth_cm, NA_real_)
    ) %>%
    select(sample_id, depth_cm)
  
  return(depth_info)
}

#' Merge root data with sample metadata
#'
#' Joins root chemistry data with sample metadata.
#'
#' @param cn_data Data frame containing root chemistry data
#' @param sample_data Data frame containing sample metadata
#' @return Merged data frame
#' @export
merge_root_data <- function(cn_data, sample_data) {
  
  # Join on sample ID
  merged <- cn_data %>%
    left_join(sample_data, by = c("cnSampleID" = "sampleID"))
  
  return(merged)
}

#' Create depth categories based on breaks
#'
#' Creates categorical depth variables for analysis.
#'
#' @param depths Numeric vector of depths in cm
#' @param breaks Numeric vector of depth interval breaks
#' @return Character vector of depth categories
#' @export
create_depth_categories <- function(depths, breaks = c(0, 10, 25, 50, 100, 200)) {
  
  if (all(is.na(depths))) {
    return(rep(NA_character_, length(depths)))
  }
  
  valid_depths <- depths[!is.na(depths)]
  
  if (length(valid_depths) == 0) {
    return(rep(NA_character_, length(depths)))
  }
  
  # Create categories
  categories <- cut(
    valid_depths,
    breaks = breaks,
    labels = sprintf("Depth_%d-%d", breaks[-length(breaks)], breaks[-1]),
    include.lowest = TRUE
  )
  
  # Handle NA values
  result <- rep(NA_character_, length(depths))
  result[!is.na(depths)] <- categories
  
  return(result)
}

#' Load site configuration
#'
#' Loads configuration parameters for a specific NEON site.
#'
#' @param site_id Character string specifying NEON site ID
#' @return List containing site configuration
#' @export
load_site_config <- function(site_id) {
  
  # Define site configurations
  site_configs <- list(
    DEJU = list(
      site_id = "DEJU",
      name = "Delta Junction",
      domain = "D19",
      depth_breaks = c(0, 10, 25, 50, 100, 200),
      root_size_classes = list(fine = "≤4mm", coarse = ">4mm"),
      significance_level = 0.05,
      min_samples_per_class = 5
    ),
    HARV = list(
      site_id = "HARV",
      name = "Harvard Forest",
      domain = "D01",
      depth_breaks = c(0, 10, 25, 50, 100, 150),
      root_size_classes = list(fine = "≤4mm", coarse = ">4mm"),
      significance_level = 0.05,
      min_samples_per_class = 5
    ),
    BART = list(
      site_id = "BART",
      name = "Bartlett Experimental Forest",
      domain = "D01",
      depth_breaks = c(0, 10, 25, 50, 100, 150),
      root_size_classes = list(fine = "≤4mm", coarse = ">4mm"),
      significance_level = 0.05,
      min_samples_per_class = 5
    ),
    NIWO = list(
      site_id = "NIWO",
      name = "Niwot Ridge",
      domain = "D13",
      depth_breaks = c(0, 10, 25, 50, 100),
      root_size_classes = list(fine = "≤4mm", coarse = ">4mm"),
      significance_level = 0.05,
      min_samples_per_class = 5
    )
  )
  
  if (!(site_id %in% names(site_configs))) {
    stop(sprintf("Site configuration not found for site: %s", site_id))
  }
  
  return(site_configs[[site_id]])
}

#' Process NEON root chemistry and soil bulk density data
#'
#' Loads and processes raw NEON data files, performing data cleaning,
#' validation, and preparation for analysis. Handles multiple NEON sites
#' through configuration-based processing.
#'
#' @param data_dir Character string specifying directory containing raw data files
#' @param site_id Character string specifying NEON site ID (default: "DEJU")
#' @param config List containing site configuration parameters (optional)
#' @param validate Logical indicating whether to perform data validation
#'
#' @return List containing processed data frames
#' @export
process_neon_data <- function(data_dir = "data/raw_data",
                              site_id = "DEJU",
                              config = NULL,
                              validate = TRUE) {
  
  message(sprintf("Processing NEON data for site: %s", site_id))
  
  # Load site configuration if not provided
  if (is.null(config)) {
    config <- load_site_config(site_id)
  }
  
  # Define expected file names
  expected_files <- c(
    root_chemistry = "megapit_carbon_nitrogen.csv",
    root_samples = "megapit_root_samples.csv",
    soil_bulk_density = "soil_bulk_density.csv",
    soil_chemistry = "soil_chemistry.csv",
    biomass = "megapit_biomass.csv"
  )
  
  # Check file availability
  file_paths <- file.path(data_dir, expected_files)
  available_files <- file.exists(file_paths)
  
  if (!all(available_files[c("root_chemistry", "soil_bulk_density")])) {
    stop(sprintf("Critical data files missing for site %s", site_id))
  }
  
  # Process each data type
  processed_data <- list()
  
  # Process root chemistry data
  if (available_files["root_chemistry"]) {
    message("  Processing root chemistry data...")
    processed_data$root_chemistry <- process_root_chemistry(
      file_paths["root_chemistry"],
      site_id,
      config,
      validate
    )
  }
  
  # Process soil bulk density data
  if (available_files["soil_bulk_density"]) {
    message("  Processing soil bulk density data...")
    processed_data$soil_bulk_density <- process_soil_bulk_density(
      file_paths["soil_bulk_density"],
      site_id,
      config,
      validate
    )
  }
  
  # Process additional data types if available
  if (available_files["root_samples"]) {
    message("  Processing root sample metadata...")
    processed_data$root_samples <- process_root_samples(
      file_paths["root_samples"],
      site_id,
      config,
      validate
    )
  }
  
  if (available_files["soil_chemistry"]) {
    message("  Processing soil chemistry data...")
    processed_data$soil_chemistry <- process_soil_chemistry(
      file_paths["soil_chemistry"],
      site_id,
      config,
      validate
    )
  }
  
  if (available_files["biomass"]) {
    message("  Processing biomass data...")
    processed_data$biomass <- process_biomass_data(
      file_paths["biomass"],
      site_id,
      config,
      validate
    )
  }
  
  # Merge datasets if both root and soil data are available
  if (!is.null(processed_data$root_chemistry) && !is.null(processed_data$soil_bulk_density)) {
    message("  Merging root and soil datasets...")
    processed_data$merged_data <- merge_root_soil_data(
      processed_data$root_chemistry,
      processed_data$soil_bulk_density,
      site_id,
      config
    )
  }
  
  message(sprintf("✓ Data processing complete for site %s", site_id))
  return(processed_data)
}

#' Process root chemistry data
#'
#' Cleans and validates root chemistry data from NEON megapit samples.
#'
#' @param file_path Character string specifying input file path
#' @param site_id Character string specifying site ID
#' @param config List containing site configuration
#' @param validate Logical indicating whether to perform validation
#'
#' @return Cleaned data frame
#' @export
process_root_chemistry <- function(file_path, site_id, config, validate = TRUE) {
  
  # Read data
  raw_data <- readr::read_csv(file_path, show_col_types = FALSE)
  
  # Filter for current site
  site_data <- raw_data %>%
    filter(siteID == site_id)
  
  if (nrow(site_data) == 0) {
    warning(sprintf("No data found for site %s in root chemistry file", site_id))
    return(NULL)
  }
  
  # Standard processing
  processed_data <- site_data %>%
    # Remove samples with missing critical variables or quality issues
    filter(!is.na(carbonPercent), !is.na(nitrogenPercent), 
           carbonPercent > 0, nitrogenPercent > 0,
           cnPercentQF == "OK", cnIsotopeQF == "OK") %>%
    # Calculate C:N ratio
    mutate(
      cn_ratio = carbonPercent / nitrogenPercent,
      # Extract depth information from sample IDs if available
      depth_info = extract_depth_info(cnSampleID)
    ) %>%
    # Join with depth information
    left_join(depth_info, by = c("cnSampleID" = "sample_id")) %>%
    # Create depth categories based on site configuration
    mutate(
      depth_category = create_depth_categories(depth_cm, config$depth_breaks)
    ) %>%
    # Create size categories based on site configuration
    mutate(
      sizeCategory = create_size_categories(cnSampleID, config$root_size_classes)
    )
  
  # Remove extreme outliers if we have enough data
  if (nrow(processed_data) > 5) {
    processed_data <- processed_data %>%
      filter(
        abs(carbonPercent - mean(carbonPercent, na.rm = TRUE)) <= 4 * sd(carbonPercent, na.rm = TRUE),
        abs(nitrogenPercent - mean(nitrogenPercent, na.rm = TRUE)) <= 4 * sd(nitrogenPercent, na.rm = TRUE),
        abs(cn_ratio - mean(cn_ratio, na.rm = TRUE)) <= 4 * sd(cn_ratio, na.rm = TRUE)
      )
  }
  
  # Validate processed data
  if (validate) {
    validation_results <- validate_root_chemistry(processed_data, site_id)
    if (!validation_results$valid) {
      warning(sprintf("Root chemistry validation failed for site %s: %s", 
                      site_id, validation_results$message))
    }
  }
  
  message(sprintf("  Processed %d root chemistry samples", nrow(processed_data)))
  return(processed_data)
}

#' Process soil bulk density data
#'
#' Cleans and validates soil bulk density data from NEON megapit samples.
#'
#' @param file_path Character string specifying input file path
#' @param site_id Character string specifying site ID
#' @param config List containing site configuration
#' @param validate Logical indicating whether to perform validation
#'
#' @return Cleaned data frame
#' @export
process_soil_bulk_density <- function(file_path, site_id, config, validate = TRUE) {
  
  raw_data <- readr::read_csv(file_path, show_col_types = FALSE)
  
  # Filter for current site
  site_data <- raw_data %>%
    filter(siteID == site_id)
  
  if (nrow(site_data) == 0) {
    warning(sprintf("No data found for site %s in soil bulk density file", site_id))
    return(NULL)
  }
  
  # Standard processing
  processed_data <- site_data %>%
    # Remove samples with missing bulk density
    filter(!is.na(bulk_density)) %>%
    # Use corrected bulk density (excluding coarse fragments) if available
    mutate(
      bulk_density_corrected = coalesce(bulkDensExclCoarseFrag, bulk_density)
    ) %>%
    # Create depth categories
    mutate(
      depth_category = create_depth_categories(bulkDensCenterDepth, config$depth_breaks)
    ) %>%
    # Remove extreme outliers
    filter(
      abs(bulk_density_corrected - mean(bulk_density_corrected, na.rm = TRUE)) <= 
        4 * sd(bulk_density_corrected, na.rm = TRUE)
    ) %>%
    # Standardize column names
    select(
      siteID, domainID, pitID, horizonName,
      depth_cm = bulkDensCenterDepth,
      depth_cm_top = bulkDensTopDepth,
      depth_cm_bottom = bulkDensBottomDepth,
      bulk_density = bulk_density_corrected,
      bulk_density_raw = bulk_density,
      depth_category,
      everything()
    )
  
  # Validate processed data
  if (validate) {
    validation_results <- validate_soil_bulk_density(processed_data, site_id)
    if (!validation_results$valid) {
      warning(sprintf("Soil bulk density validation failed for site %s: %s", 
                      site_id, validation_results$message))
    }
  }
  
  message(sprintf("  Processed %d soil bulk density samples", nrow(processed_data)))
  return(processed_data)
}

#' Merge root and soil datasets
#'
#' Merges processed root chemistry and soil bulk density data based on
#' spatial proximity and depth alignment.
#'
#' @param root_data Data frame containing processed root chemistry data
#' @param soil_data Data frame containing processed soil bulk density data
#' @param site_id Character string specifying site ID
#' @param config List containing site configuration
#'
#' @return Merged data frame
#' @export
merge_root_soil_data <- function(root_data, soil_data, site_id, config) {
  
  if (is.null(root_data) || is.null(soil_data)) {
    warning("Cannot merge data: one or both datasets are NULL")
    return(NULL)
  }
  
  message("  Merging root and soil datasets...")
  
  # Perform spatial merge based on site and approximate depth
  merged_data <- root_data %>%
    left_join(
      soil_data %>% 
        select(siteID, depth_cm, depth_category, horizonName, bulk_density, everything()),
      by = "siteID"
    ) %>%
    # Filter for approximate depth matches (within 5 cm)
    filter(
      abs(depth_cm.x - depth_cm.y) <= 5 | 
      is.na(depth_cm.x) | is.na(depth_cm.y) |
      (depth_category.x == depth_category.y & !is.na(depth_category.x))
    ) %>%
    # Resolve depth conflicts
    mutate(
      depth_cm = coalesce(depth_cm.x, depth_cm.y),
      depth_category = coalesce(depth_category.x, depth_category.y)
    ) %>%
    # Remove duplicate columns
    select(-ends_with(".x"), -ends_with(".y"), -starts_with("depth_cm."), -starts_with("depth_category."))
  
  # Handle missing matches
  n_merged <- nrow(merged_data)
  n_root <- nrow(root_data)
  n_soil <- nrow(soil_data)
  
  message(sprintf("  Merged %d observations from %d root and %d soil samples", 
                  n_merged, n_root, n_soil))
  
  if (n_merged < max(n_root, n_soil) * 0.5) {
    warning(sprintf("Low merge success rate: %.1f%%", n_merged / max(n_root, n_soil) * 100))
  }
  
  return(merged_data)
}

#' Process root sample metadata
#'
#' Processes root sample metadata from NEON data.
#'
#' @param file_path Character string specifying input file path
#' @param site_id Character string specifying site ID
#' @param config List containing site configuration
#' @param validate Logical indicating whether to perform validation
#'
#' @return Cleaned data frame
#' @export
process_root_samples <- function(file_path, site_id, config, validate = TRUE) {
  
  raw_data <- readr::read_csv(file_path, show_col_types = FALSE)
  
  # Filter for current site
  site_data <- raw_data %>%
    filter(siteID == site_id)
  
  if (nrow(site_data) == 0) {
    warning(sprintf("No data found for site %s in root samples file", site_id))
    return(NULL)
  }
  
  # Standard processing
  processed_data <- site_data %>%
    # Remove samples with missing sample IDs
    filter(!is.na(sampleID)) %>%
    # Standardize column names
    select(
      siteID, domainID, pitID, horizonName,
      sampleID,
      sizeCategory,
      rootStatus,
      everything()
    )
  
  message(sprintf("  Processed %d root sample records", nrow(processed_data)))
  return(processed_data)
}

#' Process soil chemistry data
#'
#' Processes soil chemistry data from NEON samples.
#'
#' @param file_path Character string specifying input file path
#' @param site_id Character string specifying site ID
#' @param config List containing site configuration
#' @param validate Logical indicating whether to perform validation
#'
#' @return Cleaned data frame
#' @export
process_soil_chemistry <- function(file_path, site_id, config, validate = TRUE) {
  
  raw_data <- readr::read_csv(file_path, show_col_types = FALSE)
  
  # Filter for current site
  site_data <- raw_data %>%
    filter(siteID == site_id)
  
  if (nrow(site_data) == 0) {
    warning(sprintf("No data found for site %s in soil chemistry file", site_id))
    return(NULL)
  }
  
  # Standard processing
  processed_data <- site_data %>%
    # Remove samples with missing critical variables
    filter(!is.na(pH), !is.na(organicC)) %>%
    # Standardize column names
    select(
      siteID, domainID, pitID, horizonName,
      depth_cm = soilChemCenterDepth,
      pH, organicC, totalN,
      everything()
    )
  
  message(sprintf("  Processed %d soil chemistry samples", nrow(processed_data)))
  return(processed_data)
}

#' Process biomass data
#'
#' Processes biomass data from NEON samples.
#'
#' @param file_path Character string specifying input file path
#' @param site_id Character string specifying site ID
#' @param config List containing site configuration
#' @param validate Logical indicating whether to perform validation
#'
#' @return Cleaned data frame
#' @export
process_biomass_data <- function(file_path, site_id, config, validate = TRUE) {
  
  raw_data <- readr::read_csv(file_path, show_col_types = FALSE)
  
  # Filter for current site
  site_data <- raw_data %>%
    filter(siteID == site_id)
  
  if (nrow(site_data) == 0) {
    warning(sprintf("No data found for site %s in biomass file", site_id))
    return(NULL)
  }
  
  # Standard processing
  processed_data <- site_data %>%
    # Remove samples with missing biomass values
    filter(!is.na(biomass)) %>%
    # Standardize column names
    select(
      siteID, domainID, pitID, horizonName,
      depth_cm = biomassCenterDepth,
      biomass,
      everything()
    )
  
  message(sprintf("  Processed %d biomass samples", nrow(processed_data)))
  return(processed_data)
}

#' Extract depth information from NEON sample IDs
#'
#' Parses depth information from NEON sample ID format.
#'
#' @param sample_ids Character vector of NEON sample IDs
#' @return Data frame with depth information
#' @export
extract_depth_info <- function(sample_ids) {
  
  if (length(sample_ids) == 0) {
    return(tibble(sample_id = character(), depth_cm = numeric()))
  }
  
  # Extract depth from sample ID format: SITE.X.XX.STATUS.SIZE
  depth_info <- tibble(sample_id = sample_ids) %>%
    mutate(
      # Split by dots and extract depth component
      parts = stringr::str_split(sample_id, "\\."),
      depth_part = purrr::map_chr(parts, ~ .x[3] %||% NA_character_)
    ) %>%
    # Convert depth part to numeric depth in cm
    mutate(
      depth_cm = case_when(
        !is.na(depth_part) & nchar(depth_part) >= 2 ~ as.numeric(stringr::str_sub(depth_part, 1, 2)),
        !is.na(depth_part) & nchar(depth_part) == 1 ~ as.numeric(depth_part) * 10,
        TRUE ~ NA_real_
      ),
      depth_cm = ifelse(depth_cm > 0, depth_cm, NA_real_)
    ) %>%
    select(sample_id, depth_cm)
  
  return(depth_info)
}

#' Create depth categories based on breaks
#'
#' Creates categorical depth variables for analysis.
#'
#' @param depth_cm Numeric vector of depths in cm
#' @param depth_breaks Numeric vector of depth interval breaks
#' @return Character vector of depth categories
#' @export
create_depth_categories <- function(depth_cm, depth_breaks = c(0, 10, 25, 50, 100, 200)) {
  
  if (all(is.na(depth_cm))) {
    return(rep(NA_character_, length(depth_cm)))
  }
  
  valid_depths <- depth_cm[!is.na(depth_cm)]
  
  if (length(valid_depths) == 0) {
    return(rep(NA_character_, length(depth_cm)))
  }
  
  # Create categories
  categories <- cut(
    valid_depths,
    breaks = depth_breaks,
    labels = sprintf("Depth_%d-%d", depth_breaks[-length(depth_breaks)], depth_breaks[-1]),
    include.lowest = TRUE
  )
  
  # Handle NA values
  result <- rep(NA_character_, length(depth_cm))
  result[!is.na(depth_cm)] <- categories
  
  return(result)
}

#' Create root size categories
#'
#' Creates categorical size variables based on diameter thresholds.
#'
#' @param sample_ids Character vector of sample IDs
#' @param size_classes List defining size class thresholds
#' @return Character vector of size categories
#' @export
create_size_categories <- function(sample_ids, size_classes = list(fine = "≤4mm", coarse = ">4mm")) {
  
  if (length(sample_ids) == 0) {
    return(rep(NA_character_, length(sample_ids)))
  }
  
  # Default size classification based on NEON conventions
  size_categories <- rep("Unknown", length(sample_ids))
  
  # Try to extract size information from sample metadata if available
  # This is a placeholder - actual implementation would depend on NEON data structure
  
  return(size_categories)
}

#' Validate root chemistry data
#'
#' Performs comprehensive validation checks on processed root chemistry data.
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

#' Validate soil bulk density data
#'
#' Performs comprehensive validation checks on processed soil bulk density data.
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

#' Create comprehensive summary statistics
#'
#' Generates detailed summary statistics for all processed datasets.
#'
#' @param processed_data List containing processed data frames
#' @param site_id Character string specifying site ID
#' @return List containing summary statistics
#' @export
create_comprehensive_summary <- function(processed_data, site_id) {
  
  summary_stats <- list(
    site_id = site_id,
    processing_date = Sys.Date(),
    datasets = list()
  )
  
  # Root chemistry summary
  if (!is.null(processed_data$root_chemistry)) {
    root_summary <- processed_data$root_chemistry %>%
      summarise(
        n_samples = n(),
        n_pits = n_distinct(pitID, na.rm = TRUE),
        n_horizons = n_distinct(horizonName, na.rm = TRUE),
        carbon_mean = mean(carbonPercent, na.rm = TRUE),
        carbon_sd = sd(carbonPercent, na.rm = TRUE),
        nitrogen_mean = mean(nitrogenPercent, na.rm = TRUE),
        nitrogen_sd = sd(nitrogenPercent, na.rm = TRUE),
        cn_ratio_mean = mean(cn_ratio, na.rm = TRUE),
        cn_ratio_sd = sd(cn_ratio, na.rm = TRUE),
        depth_range = paste(round(min(depth_cm, na.rm = TRUE), 0), 
                           round(max(depth_cm, na.rm = TRUE), 0), sep = "-")
      )
    
    summary_stats$datasets$root_chemistry <- root_summary
  }
  
  # Soil bulk density summary
  if (!is.null(processed_data$soil_bulk_density)) {
    soil_summary <- processed_data$soil_bulk_density %>%
      summarise(
        n_samples = n(),
        n_pits = n_distinct(pitID, na.rm = TRUE),
        n_horizons = n_distinct(horizonName, na.rm = TRUE),
        bulk_density_mean = mean(bulk_density, na.rm = TRUE),
        bulk_density_sd = sd(bulk_density, na.rm = TRUE),
        depth_range = paste(round(min(depth_cm, na.rm = TRUE), 0), 
                           round(max(depth_cm, na.rm = TRUE), 0), sep = "-")
      )
    
    summary_stats$datasets$soil_bulk_density <- soil_summary
  }
  
  # Merged data summary
  if (!is.null(processed_data$merged_data)) {
    merged_summary <- processed_data$merged_data %>%
      summarise(
        n_merged_observations = n(),
        n_root_samples = n_distinct(cnSampleID, na.rm = TRUE),
        n_soil_samples = n_distinct(pitID.y, na.rm = TRUE),
        merge_success_rate = n() / max(n_distinct(cnSampleID, na.rm = TRUE), 
                                       n_distinct(pitID.y, na.rm = TRUE)) * 100
      )
    
    summary_stats$datasets$merged_data <- merged_summary
  }
  
  return(summary_stats)
}
