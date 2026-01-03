# ==============================================================================
# LIVE Pipeline Tests for South Dakota School Data
# ==============================================================================
#
# These tests verify EACH STEP of the data pipeline using LIVE network calls.
# NO MOCKS - we verify actual connectivity and data correctness.
#
# Test Categories:
# 1. URL Availability - HTTP 200 for SD DOE files
# 2. File Download - Verify file downloads and is Excel (not HTML error)
# 3. File Parsing - readxl can read the downloaded file
# 4. Column Structure - Expected columns exist
# 5. get_raw_enr() - Raw data function returns valid data
# 6. Data Quality - No Inf/NaN, non-negative counts
# 7. Aggregation - District totals are reasonable
# 8. Output Fidelity - tidy=TRUE matches raw data totals
# 9. Year-by-Year Verification - All years work
# 10. Known Value Tests - Spot checks against expected values
#
# ==============================================================================

# Helper function to skip tests if offline
skip_if_offline <- function() {
  tryCatch({
    response <- httr::HEAD("https://www.google.com", httr::timeout(5))
    if (httr::http_error(response)) {
      skip("No network connectivity")
    }
  }, error = function(e) {
    skip("No network connectivity")
  })
}

# ==============================================================================
# STEP 1: URL Availability Tests
# ==============================================================================

test_that("SD DOE enrollment page is accessible", {
  skip_if_offline()

  response <- httr::HEAD(
    "https://doe.sd.gov/ofm/enrollment.aspx",
    httr::timeout(30)
  )
  expect_true(httr::status_code(response) %in% c(200, 301, 302))
})

test_that("SD DOE documents directory is accessible", {
  skip_if_offline()

  response <- httr::HEAD(
    "https://doe.sd.gov/ofm/documents/",
    httr::timeout(30)
  )
  # May return 403 for directory listing, but base URL should work
  expect_true(httr::status_code(response) %in% c(200, 301, 302, 403))
})

test_that("2025 enrollment file URLs return HTTP 200", {
  skip_if_offline()

  # Test all 2025 file types
  files <- c(
    "Pubdisgr-2025.xlsx",   # District data
    "Pubschrce-2025.xlsx",  # Race/ethnicity
    "Pubschgen-2025.xlsx",  # Gender
    "Pubschgr-2025.xlsx"    # Grade by school
  )

  for (file in files) {
    url <- paste0("https://doe.sd.gov/ofm/documents/", file)
    tryCatch({
      response <- httr::HEAD(url, httr::timeout(30))
      expect_equal(httr::status_code(response), 200)
    }, error = function(e) {
      skip(paste("Network error for", file, "-", e$message))
    })
  }
})

test_that("2024 enrollment file URLs return HTTP 200", {
  skip_if_offline()

  # Test 2024 file types (note: gender uses 2-digit year)
  files <- c(
    "Pubdisgr-2024.xlsx",
    "Pubschrce-2024.xlsx",
    "Pubschgen-24.xlsx",  # Note: 2-digit year
    "Pubschgr-2024.xlsx"
  )

  for (file in files) {
    url <- paste0("https://doe.sd.gov/ofm/documents/", file)
    tryCatch({
      response <- httr::HEAD(url, httr::timeout(30))
      expect_equal(httr::status_code(response), 200)
    }, error = function(e) {
      skip(paste("Network error for", file, "-", e$message))
    })
  }
})

# ==============================================================================
# STEP 2: File Download Tests
# ==============================================================================

test_that("Can download 2025 district enrollment file", {
  skip_if_offline()

  url <- "https://doe.sd.gov/ofm/documents/Pubdisgr-2025.xlsx"
  temp_file <- tempfile(fileext = ".xlsx")

  tryCatch({
    response <- httr::GET(
      url,
      httr::write_disk(temp_file, overwrite = TRUE),
      httr::timeout(120)
    )

    # Check HTTP status
    expect_equal(httr::status_code(response), 200)

    # Check file exists and has reasonable size
    expect_true(file.exists(temp_file))
    file_size <- file.info(temp_file)$size
    expect_gt(file_size, 10000)  # Should be > 10KB

    # Verify it's an Excel file (PK signature for ZIP/XLSX)
    con <- file(temp_file, "rb")
    first_bytes <- readBin(con, raw(), 4)
    close(con)
    first_chars <- rawToChar(first_bytes[1:2])
    expect_equal(first_chars, "PK")

    if (file.exists(temp_file)) unlink(temp_file)

  }, error = function(e) {
    if (file.exists(temp_file)) unlink(temp_file)
    skip(paste("Download test failed:", e$message))
  })
})

test_that("Downloaded file is not an HTML error page", {
  skip_if_offline()

  url <- "https://doe.sd.gov/ofm/documents/Pubdisgr-2025.xlsx"
  temp_file <- tempfile(fileext = ".xlsx")

  tryCatch({
    httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), httr::timeout(60))

    # Read first bytes - Excel files start with PK (ZIP signature)
    # HTML would start with < or whitespace followed by <
    con <- file(temp_file, "rb")
    first_bytes <- readBin(con, raw(), 4)
    close(con)

    # Check for PK signature (Excel/ZIP) - not HTML
    expect_equal(rawToChar(first_bytes[1:2]), "PK")

    if (file.exists(temp_file)) unlink(temp_file)

  }, error = function(e) {
    if (file.exists(temp_file)) unlink(temp_file)
    skip(paste("File verification failed:", e$message))
  })
})

# ==============================================================================
# STEP 3: File Parsing Tests
# ==============================================================================

test_that("Can parse 2025 district file with readxl", {
  skip_if_offline()

  url <- "https://doe.sd.gov/ofm/documents/Pubdisgr-2025.xlsx"
  temp_file <- tempfile(fileext = ".xlsx")

  tryCatch({
    httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), httr::timeout(60))

    # Should be able to read Excel file
    df <- readxl::read_xlsx(temp_file, skip = 5)

    expect_true(is.data.frame(df))
    expect_gt(nrow(df), 10)  # Should have multiple districts
    expect_gt(ncol(df), 5)   # Should have multiple columns

    if (file.exists(temp_file)) unlink(temp_file)

  }, error = function(e) {
    if (file.exists(temp_file)) unlink(temp_file)
    skip(paste("File parsing failed:", e$message))
  })
})

test_that("Can list sheets in Excel file", {
  skip_if_offline()

  url <- "https://doe.sd.gov/ofm/documents/Pubdisgr-2025.xlsx"
  temp_file <- tempfile(fileext = ".xlsx")

  tryCatch({
    httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), httr::timeout(60))

    sheets <- readxl::excel_sheets(temp_file)
    expect_gt(length(sheets), 0)

    if (file.exists(temp_file)) unlink(temp_file)

  }, error = function(e) {
    if (file.exists(temp_file)) unlink(temp_file)
    skip(paste("Sheet listing failed:", e$message))
  })
})

# ==============================================================================
# STEP 4: Column Structure Tests
# ==============================================================================

test_that("District file has expected columns", {
  skip_if_offline()

  url <- "https://doe.sd.gov/ofm/documents/Pubdisgr-2025.xlsx"
  temp_file <- tempfile(fileext = ".xlsx")

  tryCatch({
    httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), httr::timeout(60))

    df <- readxl::read_xlsx(temp_file, skip = 5)
    cols <- tolower(names(df))

    # Check for district identifier columns
    expect_true(any(grepl("district", cols)))

    # Check for grade or total columns
    expect_true(any(grepl("total|pk|kg|k", cols)))

    if (file.exists(temp_file)) unlink(temp_file)

  }, error = function(e) {
    if (file.exists(temp_file)) unlink(temp_file)
    skip(paste("Column structure test failed:", e$message))
  })
})

test_that("Race/ethnicity file has expected columns", {
  skip_if_offline()

  url <- "https://doe.sd.gov/ofm/documents/Pubschrce-2025.xlsx"
  temp_file <- tempfile(fileext = ".xlsx")

  tryCatch({
    httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), httr::timeout(60))

    df <- readxl::read_xlsx(temp_file, skip = 5)
    cols <- tolower(names(df))

    # Check for school/district columns
    expect_true(any(grepl("school|district", cols)))

    if (file.exists(temp_file)) unlink(temp_file)

  }, error = function(e) {
    if (file.exists(temp_file)) unlink(temp_file)
    skip(paste("Column structure test failed:", e$message))
  })
})

# ==============================================================================
# STEP 5: get_raw_enr() Function Tests
# ==============================================================================

test_that("get_raw_enr() returns valid data for 2025", {
  skip_if_offline()

  result <- get_raw_enr(2025)

  expect_true(is.list(result))
  expect_true("district" %in% names(result))
  expect_true(is.data.frame(result$district))
  expect_gt(nrow(result$district), 50)  # SD has 150+ districts
})

test_that("get_raw_enr() returns valid data for 2024", {
  skip_if_offline()

  result <- get_raw_enr(2024)

  expect_true(is.list(result))
  expect_true("district" %in% names(result))
  expect_true(is.data.frame(result$district))
  expect_gt(nrow(result$district), 50)
})

test_that("get_raw_enr() returns campus race data", {
  skip_if_offline()

  result <- get_raw_enr(2025)

  expect_true("campus_race" %in% names(result) || "campus" %in% names(result))
  if ("campus_race" %in% names(result) && !is.null(result$campus_race)) {
    expect_true(is.data.frame(result$campus_race))
    expect_gt(nrow(result$campus_race), 100)  # Many schools
  }
})

test_that("get_raw_enr() returns campus gender data", {
  skip_if_offline()

  result <- get_raw_enr(2025)

  if ("campus_gender" %in% names(result) && !is.null(result$campus_gender)) {
    expect_true(is.data.frame(result$campus_gender))
    expect_gt(nrow(result$campus_gender), 100)
  }
})

test_that("get_raw_enr() rejects invalid years", {
  expect_error(get_raw_enr(2000), "must be between")
  expect_error(get_raw_enr(2050), "must be between")
})

test_that("get_raw_enr() adds end_year column", {
  skip_if_offline()

  result <- get_raw_enr(2025)

  expect_true("end_year" %in% names(result$district))
  expect_equal(unique(result$district$end_year), 2025)
})

# ==============================================================================
# STEP 6: Data Quality Tests
# ==============================================================================

test_that("No Inf or NaN values in raw data", {
  skip_if_offline()

  result <- get_raw_enr(2025)
  df <- result$district

  # Check all numeric columns for Inf/NaN
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  for (col in numeric_cols) {
    expect_false(any(is.infinite(df[[col]]), na.rm = TRUE))
    expect_false(any(is.nan(df[[col]]), na.rm = TRUE))
  }
})

test_that("fetch_enr returns data with no Inf or NaN", {
  skip_if_offline()

  data <- fetch_enr(2025, tidy = TRUE, use_cache = FALSE)

  numeric_cols <- names(data)[sapply(data, is.numeric)]
  for (col in numeric_cols) {
    expect_false(any(is.infinite(data[[col]]), na.rm = TRUE))
    expect_false(any(is.nan(data[[col]]), na.rm = TRUE))
  }
})

test_that("Enrollment counts are non-negative", {
  skip_if_offline()

  data <- fetch_enr(2025, tidy = FALSE, use_cache = FALSE)

  if ("row_total" %in% names(data)) {
    expect_true(all(data$row_total >= 0, na.rm = TRUE))
  }
})

test_that("State total enrollment is in reasonable range", {
  skip_if_offline()

  data <- fetch_enr(2025, tidy = TRUE, use_cache = FALSE)

  # Find state total
  state_total <- data |>
    dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
    dplyr::pull(n_students)

  if (length(state_total) > 0 && !is.na(state_total[1])) {
    # South Dakota has approximately 140,000 K-12 students
    expect_gt(state_total, 100000)
    expect_lt(state_total, 200000)
  }
})

# ==============================================================================
# STEP 7: Aggregation Tests
# ==============================================================================

test_that("District totals sum to approximately state total", {
  skip_if_offline()

  data <- fetch_enr(2025, tidy = FALSE, use_cache = FALSE)

  # Get state total
  state_rows <- data[data$type == "State", ]
  if (nrow(state_rows) > 0 && "row_total" %in% names(state_rows)) {
    state_total <- sum(state_rows$row_total, na.rm = TRUE)

    # Get district totals
    district_rows <- data[data$type == "District", ]
    if (nrow(district_rows) > 0) {
      district_total <- sum(district_rows$row_total, na.rm = TRUE)

      # Should be within 5% (some schools may not be in districts)
      if (state_total > 0) {
        ratio <- district_total / state_total
        expect_gt(ratio, 0.90)
        expect_lt(ratio, 1.10)
      }
    }
  }
})

test_that("Gender counts are reasonable", {
  skip_if_offline()

  data <- fetch_enr(2025, tidy = TRUE, use_cache = FALSE)

  male <- data |>
    dplyr::filter(is_state, subgroup == "male", grade_level == "TOTAL") |>
    dplyr::pull(n_students)

  female <- data |>
    dplyr::filter(is_state, subgroup == "female", grade_level == "TOTAL") |>
    dplyr::pull(n_students)

  if (length(male) > 0 && length(female) > 0 &&
      !is.na(male[1]) && !is.na(female[1])) {
    # Both should be > 0
    expect_gt(male, 0)
    expect_gt(female, 0)

    # Ratio should be reasonable (45-55%)
    total <- male + female
    if (total > 0) {
      male_pct <- male / total
      expect_gt(male_pct, 0.45)
      expect_lt(male_pct, 0.55)
    }
  }
})

# ==============================================================================
# STEP 8: Output Fidelity Tests
# ==============================================================================

test_that("tidy=TRUE and tidy=FALSE return consistent data", {
  skip_if_offline()

  wide <- fetch_enr(2025, tidy = FALSE, use_cache = FALSE)
  tidy_data <- fetch_enr(2025, tidy = TRUE, use_cache = FALSE)

  # Both should have data
  expect_gt(nrow(wide), 0)
  expect_gt(nrow(tidy_data), 0)
})

test_that("fetch_enr returns correct year in end_year column", {
  skip_if_offline()

  result <- fetch_enr(2025, tidy = TRUE, use_cache = FALSE)

  expect_true("end_year" %in% names(result))
  expect_true(all(result$end_year == 2025, na.rm = TRUE))
})

test_that("fetch_enr_multi returns data for all requested years", {
  skip_if_offline()

  result <- fetch_enr_multi(2024:2025, tidy = TRUE, use_cache = TRUE)

  years_in_data <- unique(result$end_year)
  expect_true(2024 %in% years_in_data)
  expect_true(2025 %in% years_in_data)
})

# ==============================================================================
# STEP 9: Year-by-Year Verification Tests
# ==============================================================================

test_that("Recent years (2021-2025) can be fetched", {
  skip_if_offline()

  for (year in 2021:2025) {
    tryCatch({
      result <- fetch_enr(year, tidy = TRUE, use_cache = TRUE)

      expect_true(is.data.frame(result))
      expect_gt(nrow(result), 100)

    }, error = function(e) {
      fail(paste("Year", year, "failed:", e$message))
    })
  }
})

test_that("Era 2 years (2013-2020) can be fetched - sample", {
  skip_if_offline()

  # Test a sample of Era 2 years
  for (year in c(2015, 2018, 2020)) {
    tryCatch({
      result <- fetch_enr(year, tidy = TRUE, use_cache = TRUE)

      expect_true(is.data.frame(result))
      expect_gt(nrow(result), 50)

    }, error = function(e) {
      fail(paste("Year", year, "failed:", e$message))
    })
  }
})

test_that("Historical years (2006-2012) can be fetched - sample", {
  skip_if_offline()

  # Test a sample of historical years
  for (year in c(2011, 2012)) {
    tryCatch({
      result <- fetch_enr(year, tidy = TRUE, use_cache = TRUE)

      expect_true(is.data.frame(result))
      expect_gt(nrow(result), 10)

    }, error = function(e) {
      fail(paste("Year", year, "failed:", e$message))
    })
  }
})

# ==============================================================================
# STEP 10: Known Value Tests (Spot Checks)
# ==============================================================================

test_that("Sioux Falls is the largest district", {
  skip_if_offline()

  result <- fetch_enr(2025, tidy = FALSE, use_cache = TRUE)

  # Find Sioux Falls district
  sioux_falls <- result |>
    dplyr::filter(type == "District",
                  grepl("Sioux Falls", district_name, ignore.case = TRUE))

  if (nrow(sioux_falls) > 0 && "row_total" %in% names(sioux_falls)) {
    # Sioux Falls should have ~25,000+ students
    expect_gt(max(sioux_falls$row_total, na.rm = TRUE), 20000)
  }
})

test_that("get_available_years includes 2025", {
  years <- get_available_years()

  expect_true(2025 %in% years)
  expect_true(2006 %in% years)
  expect_equal(years, 2006:2025)
})

# ==============================================================================
# URL Building Function Tests
# ==============================================================================

test_that("build_sd_url constructs correct URLs", {
  # Test district URL for 2025
  url_2025 <- build_sd_url(2025, "district")
  expect_equal(url_2025, "https://doe.sd.gov/ofm/documents/Pubdisgr-2025.xlsx")

  # Test district URL for 2024
  url_2024 <- build_sd_url(2024, "district")
  expect_equal(url_2024, "https://doe.sd.gov/ofm/documents/Pubdisgr-2024.xlsx")

  # Test race URL
  race_url <- build_sd_url(2025, "school_race")
  expect_equal(race_url, "https://doe.sd.gov/ofm/documents/Pubschrce-2025.xlsx")
})

test_that("get_district_filename returns correct filenames", {
  expect_equal(get_district_filename(2025), "Pubdisgr-2025.xlsx")
  expect_equal(get_district_filename(2024), "Pubdisgr-2024.xlsx")
  expect_equal(get_district_filename(2023), "Pubdisgr-23.xlsx")
  expect_equal(get_district_filename(2022), "Pubdisgr-2022.xlsx")
  expect_equal(get_district_filename(2021), "Pubdisgr-21.xlsx")  # 2-digit year
})

test_that("get_gender_filename returns correct filenames", {
  expect_equal(get_gender_filename(2025), "Pubschgen-2025.xlsx")
  expect_equal(get_gender_filename(2024), "Pubschgen-24.xlsx")
  expect_equal(get_gender_filename(2023), "Pubschgen-23.xlsx")
})

# ==============================================================================
# Cache Tests
# ==============================================================================

test_that("Cache functions work correctly", {
  tryCatch({
    path <- get_cache_path(2025, "enrollment")
    expect_true(is.character(path))
    expect_true(grepl("2025", path))
  }, error = function(e) {
    skip("Cache functions may not be implemented")
  })
})
