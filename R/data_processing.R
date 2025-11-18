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

#' Clean root chemistry table
#'
#' Remove obviously invalid or missing values but keep reasonable data.
clean_root_chemistry <- function(df) {
  stopifnot(is.data.frame(df))

  # Check for required columns
  required_cols <- c("carbonPercent", "nitrogenPercent")
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  cleaned <- df %>%
    dplyr::filter(
      !is.na(carbonPercent),
      !is.na(nitrogenPercent),
      carbonPercent > 0,
      nitrogenPercent > 0
    ) %>%
    # If QF columns exist, keep OK/GOOD/blank but don't drop everything if unknown
    {
      if ("cnPercentQF" %in% names(.)) {
        dplyr::filter(., is.na(cnPercentQF) | cnPercentQF %in% c("OK", "good", "GOOD"))
      } else .
    } %>%
    {
      if ("cnIsotopeQF" %in% names(.)) {
        dplyr::filter(., is.na(cnIsotopeQF) | cnIsotopeQF %in% c("OK", "good", "GOOD"))
      } else .
    } %>%
    # Calculate C:N ratio
    dplyr::mutate(cn_ratio = carbonPercent / nitrogenPercent)
  
  return(cleaned)
}

#' Clean soil bulk density table
#'
#' Filter out non-positive or missing bulk density values.
clean_soil_data <- function(df) {
  stopifnot(is.data.frame(df))

  bd_col <- dplyr::coalesce(
    df$bulk_density,
    df$bulkDensExclCoarseFrag
  )

  df$bulk_density <- bd_col

  df %>%
    dplyr::filter(
      !is.na(bulk_density),
      bulk_density > 0
    )
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

#' Process all NEON data for a given site
#'
#' @param data_dir Path to raw data directory (e.g., "data/raw_data")
#' @param site_id  NEON site ID (e.g., "DEJU")
#' @param config   Optional list with configuration (depth_breaks, etc.)
#' @param validate Logical; if TRUE, run extra validation checks
#' @return A list with root_chemistry, root_samples, soil_bulk_density, soil_chemistry, biomass, merged_data
process_neon_data <- function(data_dir,
                              site_id = "DEJU",
                              config = NULL,
                              validate = TRUE) {
  
  message(glue::glue("Processing NEON data for site: {site_id}"))
  
  # --- 1. Expected file names -----------------------------------------------
  expected_files <- c(
    root_chemistry    = "megapit_carbon_nitrogen.csv",
    root_samples      = "megapit_root_samples.csv",
    soil_bulk_density = "soil_bulk_density.csv",
    soil_chemistry    = "soil_chemistry.csv",
    biomass           = "megapit_biomass.csv"
  )
  
  # --- 2. Directory & file checks -------------------------------------------
  if (!dir.exists(data_dir)) {
    stop(
      glue::glue("Could not load data: directory does not exist: {data_dir}"),
      call. = FALSE
    )
  }
  
  file_paths <- file.path(data_dir, expected_files)
  available_files <- file.exists(file_paths)
  names(available_files) <- names(expected_files)
  
  critical_files <- c("root_chemistry", "soil_bulk_density")
  critical_available <- available_files[critical_files]
  
  if (any(is.na(critical_available)) || !all(critical_available)) {
    stop(
      glue::glue(
        "Could not load data: critical data files missing for site {site_id}.\n",
        "Expected at: {paste(file_paths[critical_files], collapse = ', ')}"
      ),
      call. = FALSE
    )
  }
  
  # Optional: warn but continue if non-critical files missing
  noncritical_missing <- names(available_files)[!available_files & !names(available_files) %in% critical_files]
  if (length(noncritical_missing) > 0) {
    warning(
      glue::glue("Some non-critical files are missing and will be skipped: {paste(noncritical_missing, collapse = ', ')}"),
      call. = FALSE
    )
  }
  
  # --- 3. Default config if none provided -----------------------------------
  if (is.null(config)) {
    config <- list(
      depth_breaks = c(0, 10, 30, 60, 100, 200),
      root_size_classes = list(
        fine   = "≤4mm",
        coarse = ">4mm"
      )
    )
  }
  
  # --- 4. Process each data type --------------------------------------------
  root_chemistry <- process_root_chemistry(
    file.path(data_dir, expected_files["root_chemistry"]),
    site_id = site_id,
    config  = config,
    validate = validate
  )
  
  root_samples <- if (available_files["root_samples"]) {
    process_root_samples(
      file.path(data_dir, expected_files["root_samples"]),
      site_id = site_id,
      config  = config,
      validate = validate
    )
  } else NULL
  
  soil_bulk_density <- process_soil_bulk_density(
    file.path(data_dir, expected_files["soil_bulk_density"]),
    site_id = site_id,
    config  = config,
    validate = validate
  )
  
  soil_chemistry <- if (available_files["soil_chemistry"]) {
    process_soil_chemistry(
      file.path(data_dir, expected_files["soil_chemistry"]),
      site_id = site_id,
      config  = config,
      validate = validate
    )
  } else NULL
  
  biomass <- if (available_files["biomass"]) {
    process_biomass_data(
      file.path(data_dir, expected_files["biomass"]),
      site_id = site_id,
      config  = config,
      validate = validate
    )
  } else NULL
  
  # --- 5. Merge root + soil where possible ----------------------------------
  merged_data <- NULL
  if (!is.null(root_chemistry) && !is.null(soil_bulk_density)) {
    merged_data <- merge_root_soil_data(root_chemistry, soil_bulk_density, site_id, config)
  }
  
  list(
    root_chemistry    = root_chemistry,
    root_samples      = root_samples,
    soil_bulk_density = soil_bulk_density,
    soil_chemistry    = soil_chemistry,
    biomass           = biomass,
    merged_data       = merged_data
  )
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
  
  # Extract depth information from sample IDs
  depth_info <- extract_depth_info(site_data$cnSampleID)
  
  # Standard processing
  processed_data <- site_data %>%
    # Remove samples with missing critical variables or quality issues
    filter(!is.na(carbonPercent), !is.na(nitrogenPercent), 
           carbonPercent > 0, nitrogenPercent > 0,
           cnPercentQF == "OK", cnIsotopeQF == "OK") %>%
    # Calculate C:N ratio
    mutate(cn_ratio = carbonPercent / nitrogenPercent) %>%
    # Join with depth information
    left_join(depth_info, by = c("cnSampleID" = "cnSampleID")) %>%
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
    filter(!is.na(bulkDensExclCoarseFrag)) %>%
    # Use corrected bulk density (excluding coarse fragments) if available
    mutate(
      bulk_density = bulkDensExclCoarseFrag
    ) %>%
    # Create depth categories
    mutate(
      depth_category = create_depth_categories(bulkDensCenterDepth, config$depth_breaks)
    ) %>%
    # Remove extreme outliers
    filter(
      abs(bulk_density - mean(bulk_density, na.rm = TRUE)) <= 
        4 * sd(bulk_density, na.rm = TRUE)
    ) %>%
    # Standardize column names
    select(
      siteID, domainID, pitID, horizonName,
      depth_cm = bulkDensCenterDepth,
      depth_cm_top = bulkDensTopDepth,
      depth_cm_bottom = bulkDensBottomDepth,
      bulk_density,
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
  
  # Validate input data
  if (!"depth_cm" %in% names(root_data)) {
    stop("Root data must contain 'depth_cm' column")
  }
  if (!"depth_cm" %in% names(soil_data)) {
    stop("Soil data must contain 'depth_cm' column")
  }
  if (!"bulk_density" %in% names(soil_data)) {
    stop("Soil data must contain 'bulk_density' column")
  }
  
  # Create a simple merge based on site and approximate depth
  # First, add depth categories to soil data if not present
  if (!"depth_category" %in% names(soil_data)) {
    soil_data <- soil_data %>%
      mutate(depth_category = create_depth_categories(depth_cm, config$depth_breaks))
  }
  
  # Perform a simple merge by finding closest depth matches
  # Create a lookup table for soil data
  soil_lookup <- soil_data %>%
    select(depth_cm, bulk_density, depth_category, horizonName) %>%
    distinct()
  
  # Find closest matches for each root sample
  merge_info <- root_data %>%
    select(cnSampleID, depth_cm) %>%
    distinct() %>%
    rowwise() %>%
    mutate(
      closest_soil_idx = which.min(abs(soil_lookup$depth_cm - depth_cm)),
      bulk_density = soil_lookup$bulk_density[closest_soil_idx],
      soil_depth_cm = soil_lookup$depth_cm[closest_soil_idx],
      soil_depth_category = soil_lookup$depth_category[closest_soil_idx],
      soil_horizon = soil_lookup$horizonName[closest_soil_idx]
    ) %>%
    ungroup() %>%
    filter(!is.na(bulk_density))
  
  # Merge the soil data back to root data
  merged_data <- root_data %>%
    left_join(merge_info, by = c("cnSampleID", "depth_cm")) %>%
    filter(!is.na(bulk_density)) %>%
    select(
      # Root chemistry data
      cnSampleID, siteID, domainID, plotID,
      carbonPercent, nitrogenPercent, cn_ratio,
      depth_cm, depth_category, sizeCategory, root_status,
      # Soil data
      bulk_density, soil_depth_cm = soil_depth_cm, 
      soil_depth_category = soil_depth_category, soil_horizon = soil_horizon
    )
  
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
  
  # Check if siteID column exists, otherwise use pitNamedLocation
  if ("siteID" %in% names(raw_data)) {
    site_data <- raw_data %>%
      filter(siteID == site_id)
  } else if ("pitNamedLocation" %in% names(raw_data)) {
    # Extract site from pitNamedLocation (format: "D19_DEJU_PIT1")
    site_data <- raw_data %>%
      filter(grepl(paste0("^", site_id, "_"), pitNamedLocation))
  } else {
    warning("No site identification column found in root samples file")
    return(NULL)
  }
  
  if (nrow(site_data) == 0) {
    warning(sprintf("No data found for site %s in root samples file", site_id))
    return(NULL)
  }
  
  # Standard processing
  processed_data <- site_data %>%
    # Remove samples with missing sample IDs
    filter(!is.na(sampleID)) %>%
    # Add siteID if not present
    mutate(
      siteID = if ("siteID" %in% names(.)) siteID else site_id,
      domainID = if ("domainID" %in% names(.)) domainID else substr(pitNamedLocation, 1, 3),
      pitID = if ("pitID" %in% names(.)) pitID else substr(pitNamedLocation, 5, nchar(pitNamedLocation))
    ) %>%
    # Standardize column names
    select(
      siteID, domainID, pitID = pitID, horizonName = horizonName,
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
  
  # Check if required chemistry columns exist
  required_cols <- c("pH", "organicC", "totalN")
  available_chem_cols <- intersect(required_cols, names(raw_data))
  
  if (length(available_chem_cols) == 0) {
    warning(sprintf("No chemistry data columns found in soil chemistry file for site %s", site_id))
    return(NULL)
  }
  
  # Filter for current site
  site_data <- raw_data %>%
    filter(siteID == site_id)
  
  if (nrow(site_data) == 0) {
    warning(sprintf("No data found for site %s in soil chemistry file", site_id))
    return(NULL)
  }
  
  # Standard processing - only filter on available columns
  processed_data <- site_data
  if (length(available_chem_cols) > 0) {
    processed_data <- processed_data %>%
      filter(across(all_of(available_chem_cols), ~ !is.na(.)))
  }
  
  # Standardize column names
  processed_data <- processed_data %>%
    select(
      siteID, domainID, pitID, horizonName,
      depth_cm = horizonTopDepth,
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
  
  # Check if required columns exist
  if (!"siteID" %in% names(raw_data)) {
    warning(sprintf("No siteID column found in biomass file for site %s", site_id))
    return(NULL)
  }
  
  # Filter for current site
  site_data <- raw_data %>%
    filter(siteID == site_id)
  
  if (nrow(site_data) == 0) {
    warning(sprintf("No data found for site %s in biomass file", site_id))
    return(NULL)
  }
  
  # Check if biomass column exists
  if (!"biomass" %in% names(site_data)) {
    warning(sprintf("No biomass column found in biomass file for site %s", site_id))
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

#' Extract depth and size info from NEON cnSampleID
#'
#' @param sample_ids Character vector of cnSampleID values (e.g., "DEJU.1.20.LIVE.<4MM")
#' @return tibble with columns: cnSampleID, depth_category, root_status, size_class, depth_cm
extract_depth_info <- function(sample_ids) {
  if (!is.character(sample_ids)) {
    stop("sample_ids must be a character vector", call. = FALSE)
  }

  # Split "DEJU.1.20.LIVE.<4MM" into 5 parts
  parts <- stringr::str_split_fixed(sample_ids, "\\.", 5)

  if (ncol(parts) < 5) {
    stop("cnSampleID format not recognized. Expected 5 dot-separated fields.", call. = FALSE)
  }

  site   <- parts[, 1]
  core   <- parts[, 2]
  depth  <- parts[, 3]
  status <- parts[, 4]
  size   <- parts[, 5]

  depth_category <- paste(core, depth, sep = ".")  # e.g., "1.20", "2.30"

  size_class <- dplyr::case_when(
    size %in% c("<4MM", "≤4MM") ~ "≤4mm",
    size %in% c(">4MM")         ~ ">4mm",
    TRUE                        ~ size
  )

  depth_cm <- suppressWarnings(as.numeric(depth))

  tibble::tibble(
    cnSampleID     = sample_ids,
    depth_category = depth_category,
    root_status    = status,
    size_class     = size_class,
    depth_cm       = depth_cm
  )
}

#' Create depth category labels from numeric depths
#'
#' @param depths Numeric vector of depths (cm)
#' @param breaks Numeric vector of breakpoints (default: 0,10,30,60,100,Inf)
#' @param labels Optional character vector of labels; if NULL, constructed automatically
#' @return Character vector of depth categories
create_depth_categories <- function(depths,
                                    breaks = c(0, 10, 30, 60, 100, Inf),
                                    labels = NULL) {
  if (!is.numeric(depths)) {
    stop("depths must be numeric (cm)", call. = FALSE)
  }

  if (is.null(labels)) {
    # Create labels like "0-10cm", "10-30cm", ..., "100+cm"
    lower <- head(breaks, -1)
    upper <- tail(breaks, -1)

    labels <- paste0(lower, "-", upper, "cm")
    # Make last one "X+cm"
    labels[length(labels)] <- paste0(lower[length(lower)], "+cm")
  }

  cut(depths,
      breaks = breaks,
      labels = labels,
      include.lowest = TRUE,
      right = FALSE) |>
    as.character()
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
