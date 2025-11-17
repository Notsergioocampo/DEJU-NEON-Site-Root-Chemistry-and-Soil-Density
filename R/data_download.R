#' NEON Data Download and Caching Module
#'
#' This module provides functions for downloading and caching NEON data products
#' related to root chemistry and soil bulk density. It handles API interactions,
#' data validation, and local caching for reproducible research.
#'
#' @name data_download
#' @docType package
NULL

#' Download NEON root chemistry and soil bulk density data
#'
#' Downloads specified NEON data products for a given site and time period.
#' Implements caching to avoid repeated API calls and provides comprehensive
#' error handling for robust data acquisition.
#'
#' @param site_id Character string specifying NEON site ID (e.g., "DEJU", "HARV")
#' @param start_date Character string specifying start date in YYYY-MM-DD format
#' @param end_date Character string specifying end date in YYYY-MM-DD format
#' @param data_dir Character string specifying directory for data storage
#' @param overwrite Logical indicating whether to overwrite existing files
#' @param check_size Logical indicating whether to check data size before download
#' @param timeout Numeric specifying API timeout in seconds
#' @param max_retries Numeric specifying maximum number of retry attempts
#'
#' @return List containing paths to downloaded data files
#' @export
#'
#' @examples
#' \dontrun{
#' # Download data for DEJU site
#' data_paths <- download_neon_data(
#'   site_id = "DEJU",
#'   start_date = "2015-01-01",
#'   end_date = "2023-12-31",
#'   data_dir = "data/raw_data"
#' )
#' }
download_neon_data <- function(site_id = "DEJU",
                               start_date = "2015-01-01",
                               end_date = "2023-12-31",
                               data_dir = "data/raw_data",
                               overwrite = FALSE,
                               check_size = FALSE,
                               timeout = 300,
                               max_retries = 3) {
  
  message(sprintf("Downloading NEON data for site: %s", site_id))
  message(sprintf("Date range: %s to %s", start_date, end_date))
  
  # Validate inputs
  if (!is.character(site_id) || nchar(site_id) != 4) {
    stop("site_id must be a 4-character NEON site code")
  }
  
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
    message(sprintf("Created directory: %s", data_dir))
  }
  
  # Load site configuration
  config <- load_site_config(site_id)
  
  # Define data products to download
  data_products <- list(
    root_chemistry = list(
      product_id = config$data_products$root_chemistry,
      table_name = "mpr_carbonNitrogen",
      file_name = "megapit_carbon_nitrogen.csv"
    ),
    root_samples = list(
      product_id = config$data_products$root_chemistry,
      table_name = "mpr_perrootsample",
      file_name = "megapit_root_samples.csv"
    ),
    soil_bulk_density = list(
      product_id = config$data_products$soil_bulk_density,
      table_name = "mgp_perbulksample",
      file_name = "soil_bulk_density.csv"
    ),
    soil_chemistry = list(
      product_id = config$data_products$soil_bulk_density,
      table_name = "mgp_perchemistry",
      file_name = "soil_chemistry.csv"
    ),
    biomass = list(
      product_id = config$data_products$root_chemistry,
      table_name = "mpr_perbiomass",
      file_name = "megapit_biomass.csv"
    )
  )
  
  # Download each data product
  downloaded_files <- list()
  
  for (product_name in names(data_products)) {
    product_info <- data_products[[product_name]]
    file_path <- file.path(data_dir, product_info$file_name)
    
    # Check if file exists and should be skipped
    if (file.exists(file_path) && !overwrite) {
      message(sprintf("  Skipping %s (file exists)", product_info$file_name))
      downloaded_files[[product_name]] <- file_path
      next
    }
    
    message(sprintf("  Downloading %s...", product_info$file_name))
    
    # Attempt download with retries
    success <- FALSE
    attempt <- 1
    
    while (attempt <= max_retries && !success) {
      tryCatch({
        # Download data using neonUtilities
        neon_data <- neonUtilities::loadByProduct(
          dpID = product_info$product_id,
          site = site_id,
          startdate = start_date,
          enddate = end_date,
          package = "basic",
          check.size = check_size,
          token = Sys.getenv("NEON_TOKEN")
        )
        
        # Extract the specific table
        if (product_info$table_name %in% names(neon_data)) {
          data_table <- neon_data[[product_info$table_name]]
          
          # Write to CSV
          readr::write_csv(data_table, file_path)
          message(sprintf("  ✓ Saved %d rows to %s", nrow(data_table), product_info$file_name))
          
          downloaded_files[[product_name]] <- file_path
          success <- TRUE
          
        } else {
          warning(sprintf("Table %s not found in downloaded data", product_info$table_name))
          downloaded_files[[product_name]] <- NULL
          success <- TRUE
        }
        
        # Clean up large objects
        rm(neon_data)
        gc()
        
      }, error = function(e) {
        message(sprintf("  Attempt %d failed: %s", attempt, e$message))
        if (attempt == max_retries) {
          warning(sprintf("Failed to download %s after %d attempts", product_info$file_name, max_retries))
          downloaded_files[[product_name]] <- NULL
          success <- TRUE
        } else {
          message(sprintf("  Retrying in 5 seconds..."))
          Sys.sleep(5)
        }
      })
      
      attempt <- attempt + 1
    }
  }
  
  # Validate downloaded data
  validate_downloaded_data(downloaded_files, site_id)
  
  message(sprintf("✓ Data download complete for site: %s", site_id))
  return(downloaded_files)
}

#' Load site configuration from YAML file
#'
#' @param site_id Character string specifying NEON site ID
#' @return List containing site configuration parameters
#' @export
load_site_config <- function(site_id) {
  config_path <- here::here("config", "sites.yml")
  
  if (!file.exists(config_path)) {
    stop("Site configuration file not found: ", config_path)
  }
  
  tryCatch({
    config <- yaml::read_yaml(config_path)
    
    if (!site_id %in% names(config$sites)) {
      stop(sprintf("Site '%s' not found in configuration file", site_id))
    }
    
    site_config <- config$sites[[site_id]]
    
    # Add defaults if not specified
    if (is.null(site_config$min_samples_per_class)) {
      site_config$min_samples_per_class <- config$defaults$min_samples_per_class
    }
    
    if (is.null(site_config$significance_level)) {
      site_config$significance_level <- config$defaults$significance_level
    }
    
    return(site_config)
    
  }, error = function(e) {
    stop(sprintf("Error loading site configuration: %s", e$message))
  })
}

#' Validate downloaded NEON data
#'
#' Checks that downloaded data files contain expected variables and meet
#' minimum quality standards for analysis.
#'
#' @param file_paths List of file paths to downloaded data
#' @param site_id Character string specifying NEON site ID
#' @export
validate_downloaded_data <- function(file_paths, site_id) {
  message("Validating downloaded data...")
  
  required_variables <- list(
    root_chemistry = c("siteID", "carbonPercent", "nitrogenPercent", "cnSampleID"),
    soil_bulk_density = c("siteID", "bulk_density", "depth_cm"),
    root_samples = c("siteID", "sampleID", "sizeCategory"),
    soil_chemistry = c("siteID", "horizonName", "pH", "organicC"),
    biomass = c("siteID", "rootDryMass", "incrementRootBiomass")
  )
  
  validation_results <- list()
  
  for (data_type in names(file_paths)) {
    file_path <- file_paths[[data_type]]
    
    if (is.null(file_path) || !file.exists(file_path)) {
      validation_results[[data_type]] <- "File not downloaded"
      next
    }
    
    tryCatch({
      # Read data
      data <- readr::read_csv(file_path, show_col_types = FALSE)
      
      # Check for required variables
      if (data_type %in% names(required_variables)) {
        required_vars <- required_variables[[data_type]]
        missing_vars <- setdiff(required_vars, names(data))
        
        if (length(missing_vars) > 0) {
          validation_results[[data_type]] <- sprintf("Missing variables: %s", 
                                                     paste(missing_vars, collapse = ", "))
        } else {
          # Check for site-specific data
          if ("siteID" %in% names(data)) {
            site_data <- unique(data$siteID)
            if (length(site_data) == 0) {
              validation_results[[data_type]] <- "No data for any sites"
            } else if (!(site_id %in% site_data)) {
              validation_results[[data_type]] <- sprintf("No data for site %s", site_id)
            } else {
              site_rows <- sum(data$siteID == site_id, na.rm = TRUE)
              if (site_rows < 5) {
                validation_results[[data_type]] <- sprintf("Insufficient data: %d rows", site_rows)
              } else {
                validation_results[[data_type]] <- sprintf("Valid: %d rows for %s", site_rows, site_id)
              }
            }
          } else {
            validation_results[[data_type]] <- "Missing siteID variable"
          }
        }
      } else {
        validation_results[[data_type]] <- sprintf("Read successfully: %d rows", nrow(data))
      }
      
    }, error = function(e) {
      validation_results[[data_type]] <- sprintf("Error reading file: %s", e$message)
    })
  }
  
  # Report validation results
  message("Validation results:")
  for (data_type in names(validation_results)) {
    message(sprintf("  %s: %s", data_type, validation_results[[data_type]]))
  }
  
  # Check if critical data is available
  critical_types <- c("root_chemistry", "soil_bulk_density")
  critical_available <- all(sapply(critical_types, function(x) {
    !is.null(validation_results[[x]]) && 
    grepl("Valid:", validation_results[[x]])
  }))
  
  if (!critical_available) {
    warning("Critical data validation failed. Analysis may be limited.")
  } else {
    message("✓ Critical data validation passed")
  }
  
  return(validation_results)
}

#' Check data availability for a NEON site
#'
#' Determines whether sufficient data exists for analysis without downloading.
#'
#' @param site_id Character string specifying NEON site ID
#' @param data_dir Character string specifying data directory
#' @return Logical indicating whether data is available for analysis
#' @export
check_data_availability <- function(site_id, data_dir = "data/raw_data") {
  required_files <- c(
    "megapit_carbon_nitrogen.csv",
    "soil_bulk_density.csv"
  )
  
  file_paths <- file.path(data_dir, required_files)
  files_exist <- file.exists(file_paths)
  
  if (all(files_exist)) {
    # Quick validation check
    tryCatch({
      root_data <- readr::read_csv(file_paths[1], n_max = 10, show_col_types = FALSE)
      soil_data <- readr::read_csv(file_paths[2], n_max = 10, show_col_types = FALSE)
      
      has_site_data <- any(root_data$siteID == site_id, na.rm = TRUE) &&
                       any(soil_data$siteID == site_id, na.rm = TRUE)
      
      return(has_site_data)
      
    }, error = function(e) {
      return(FALSE)
    })
  }
  
  return(FALSE)
}

#' Cache NEON data locally
#'
#' Implements a caching mechanism to store downloaded data and avoid repeated
#' API calls. Checks modification times and data integrity.
#'
#' @param data Data frame to cache
#' @param file_path Character string specifying cache file path
#' @param force Logical indicating whether to force cache update
#' @export
cache_neon_data <- function(data, file_path, force = FALSE) {
  if (!force && file.exists(file_path)) {
    # Check if cached data is recent (within 30 days)
    file_age <- as.numeric(difftime(Sys.time(), file.mtime(file_path), units = "days"))
    
    if (file_age < 30) {
      message(sprintf("Using cached data (age: %.1f days)", file_age))
      return(FALSE)
    }
  }
  
  # Write data to cache
  readr::write_csv(data, file_path)
  message(sprintf("Cached data to %s", file_path))
  return(TRUE)
}
