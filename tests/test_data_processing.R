#' Unit Tests for Data Processing Functions
#'
#' @author Sergio Ocampo
#' @date November 2025

library(testthat)
library(dplyr)

# Source the functions we're testing
source("../R/data_processing.R")

# Create test data
create_test_cn_data <- function() {
  data.frame(
    cnSampleID = c("DEJU.1.0.LIVE.<4MM", "DEJU.2.0.DEAD.>4MM", "DEJU.3.0.LIVE.<4MM"),
    carbonPercent = c(45.0, 50.0, 40.0),
    nitrogenPercent = c(0.8, 0.6, 0.9),
    siteID = "DEJU",
    domainID = "D19"
  )
}

create_test_soil_data <- function() {
  data.frame(
    bulkDensExclCoarseFrag = c(1.2, 1.3, 1.1),
    bulkDensCenterDepth = c(10, 20, 30),
    siteID = "DEJU",
    domainID = "D19"
  )
}

create_test_sample_data <- function() {
  data.frame(
    sampleID = c("DEJU.1.0.LIVE.<4MM", "DEJU.2.0.DEAD.>4MM", "DEJU.3.0.LIVE.<4MM"),
    sizeCategory = c("<=4mm", ">4mm", "<=4mm"),
    rootStatus = c("live", "dead", "live")
  )
}

# Test clean_root_chemistry
test_that("clean_root_chemistry removes invalid data", {
  test_data <- create_test_cn_data()
  test_data$carbonPercent[2] <- NA
  test_data$nitrogenPercent[3] <- 0
  
  result <- clean_root_chemistry(test_data)
  
  expect_equal(nrow(result), 1)  # Should have 1 row after filtering
  expect_true("cn_ratio" %in% names(result))
  expect_true(all(result$cn_ratio > 0))
})

test_that("clean_root_chemistry calculates C:N ratios correctly", {
  test_data <- create_test_cn_data()
  result <- clean_root_chemistry(test_data)
  
  expected_cn <- test_data$carbonPercent / test_data$nitrogenPercent
  expect_equal(result$cn_ratio, expected_cn, tolerance = 0.01)
})

test_that("clean_root_chemistry handles missing columns", {
  test_data <- data.frame(carbonPercent = c(45, 50))
  
  expect_error(
    clean_root_chemistry(test_data),
    "Missing required columns"
  )
})

# Test clean_soil_data
test_that("clean_soil_data filters invalid measurements", {
  test_data <- create_test_soil_data()
  test_data$bulkDensExclCoarseFrag[2] <- NA
  test_data$bulkDensCenterDepth[3] <- -5
  
  result <- clean_soil_data(test_data)
  
  expect_equal(nrow(result), 2)  # Should have 2 rows after filtering (only NA row removed)
  expect_true(all(result$bulk_density > 0))
  expect_true(all(result$depth_cm >= 0))
})

# Test extract_depth_info
test_that("extract_depth_info parses sample IDs correctly", {
  sample_ids <- c("DEJU.1.20.LIVE.<4MM", "DEJU.2.30.DEAD.>4MM")
  result <- extract_depth_info(sample_ids)
  
  expect_equal(result$depth_category, c("1.20", "2.30"))
  expect_equal(result$root_status, c("LIVE", "DEAD"))
  expect_equal(result$size_class, c("≤4mm", ">4mm"))
  expect_equal(result$depth_cm, c(20, 30))
})

# Test merge_root_data
test_that("merge_root_data joins datasets correctly", {
  cn_data <- create_test_cn_data()
  sample_data <- create_test_sample_data()
  
  result <- merge_root_data(cn_data, sample_data)
  
  expect_equal(nrow(result), 3)
  expect_true("sizeCategory" %in% names(result))
  expect_true("carbonPercent" %in% names(result))
})

# Test create_depth_categories
test_that("create_depth_categories creates appropriate categories", {
  depths <- c(5, 15, 25, 45, 75, 150)
  result <- create_depth_categories(depths)
  
  expected <- c("0-10cm", "10-30cm", "10-30cm", "30-60cm", "60-100cm", "100+cm")
  expect_equal(result, expected)
})

test_that("create_depth_categories handles custom breaks", {
  depths <- c(5, 15, 25)
  result <- create_depth_categories(depths, breaks = c(0, 10, 20, 30))
  
  expected <- c("0-10cm", "10-20cm", "20+cm")
  expect_equal(result, expected)
})

# Test input validation
test_that("functions validate inputs properly", {
  expect_error(clean_root_chemistry("not a dataframe"))
  expect_error(clean_soil_data("not a dataframe"))
  expect_error(extract_depth_info(123))
  expect_error(merge_root_data("not a dataframe", create_test_sample_data()))
  expect_error(merge_root_data(create_test_cn_data(), "not a dataframe"))
})

# Test edge cases
test_that("functions handle edge cases", {
  # Empty data frame
  empty_cn <- data.frame(
    cnSampleID = character(),
    carbonPercent = numeric(),
    nitrogenPercent = numeric()
  )
  result <- clean_root_chemistry(empty_cn)
  expect_equal(nrow(result), 0)
  
  # All missing values
  all_missing <- create_test_cn_data()
  all_missing$carbonPercent <- NA
  result <- clean_root_chemistry(all_missing)
  expect_equal(nrow(result), 0)
  
  # Extreme outliers - should keep all valid rows (no outlier filtering in new version)
  outliers <- create_test_cn_data()
  outliers$carbonPercent[1] <- 150  # > 100%
  outliers$nitrogenPercent[2] <- 15  # > 10%
  
  result <- clean_root_chemistry(outliers)
  expect_equal(nrow(result), 3)  # Should keep all 3 rows (no outlier filtering in new version)
})

# Test process_neon_data (mock test since it requires files)
test_that("process_neon_data validates file existence", {
  # This would normally test with actual files
  # For now, just test that it gives appropriate error for missing files
  expect_error(
    process_neon_data("nonexistent_directory"),
    "Could not load"
  )
})

# Run tests
if (sys.nframe() == 0) {
  test_results <- test_dir("tests", reporter = "summary")
  print(test_results)
}
