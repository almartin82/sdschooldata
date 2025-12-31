# Tests for enrollment functions
# Note: Most tests are marked as skip_on_cran since they require network access

test_that("safe_numeric handles various inputs", {
  # Normal numbers
  expect_equal(safe_numeric("100"), 100)
  expect_equal(safe_numeric("1,234"), 1234)

  # Suppressed values
  expect_true(is.na(safe_numeric("*")))
  expect_true(is.na(safe_numeric("-1")))
  expect_true(is.na(safe_numeric("<5")))
  expect_true(is.na(safe_numeric("")))
  expect_true(is.na(safe_numeric("N/A")))

  # Whitespace handling
  expect_equal(safe_numeric("  100  "), 100)

  # Already numeric
  expect_equal(safe_numeric(42), 42)

  # Vector input
  expect_equal(safe_numeric(c("1", "2", "3")), c(1, 2, 3))
})

test_that("get_available_years returns valid range", {
  years <- get_available_years()
  expect_true(is.integer(years))
  expect_true(length(years) > 0)
  expect_true(min(years) >= 2011)
  expect_true(max(years) <= 2030)
})

test_that("fetch_enr validates year parameter", {
  expect_error(fetch_enr(2000), "end_year must be between")
  expect_error(fetch_enr(2030), "end_year must be between")
})

test_that("build_sd_url constructs valid URLs", {
  url <- build_sd_url(2024, "district")
  expect_true(grepl("doe.sd.gov", url))
  expect_true(grepl("documents", url))
  expect_true(grepl("\\.xlsx$", url))
})

test_that("get_district_filename returns correct filenames for each era", {
  # Era 1
  expect_equal(get_district_filename(2011), "FE11_Psum.xlsx")
  expect_equal(get_district_filename(2012), "FE12_Pscgr.xlsx")

  # Era 2
  expect_equal(get_district_filename(2015), "Pubdsgr15.xlsx")
  expect_equal(get_district_filename(2017), "Pubdsgr17b.xlsx")
  expect_equal(get_district_filename(2019), "Pubdisgr-19.xlsx")

  # Era 3
  expect_equal(get_district_filename(2024), "Pubdisgr-2024.xlsx")
  expect_equal(get_district_filename(2025), "Pubdisgr-2025.xlsx")
})

test_that("get_cache_dir returns valid path", {
  cache_dir <- get_cache_dir()
  expect_true(is.character(cache_dir))
  expect_true(grepl("sdschooldata", cache_dir))
})

test_that("cache functions work correctly", {
  # Test cache path generation
  path <- get_cache_path(2024, "tidy")
  expect_true(grepl("enr_tidy_2024.rds", path))

  # Test cache_exists returns FALSE for non-existent cache
  # (Assuming no cache exists for year 9999)
  expect_false(cache_exists(9999, "tidy"))
})

# Integration tests (require network access)
test_that("fetch_enr downloads and processes data for Era 3 (2021+)", {
  skip_on_cran()
  skip_if_offline()

  # Use a recent year
  result <- fetch_enr(2024, tidy = FALSE, use_cache = FALSE)

  # Check structure
  expect_true(is.data.frame(result))
  expect_true("district_id" %in% names(result))
  expect_true("row_total" %in% names(result))
  expect_true("type" %in% names(result))

  # Check we have state and district levels
  expect_true("State" %in% result$type)
  expect_true("District" %in% result$type)

  # Check ID format (5-digit district IDs)
  districts <- result[result$type == "District" & !is.na(result$district_id), ]
  expect_true(all(nchar(districts$district_id) == 5))

  # Check state total is reasonable (South Dakota has ~145k students)
  state_row <- result[result$type == "State", ]
  expect_true(state_row$row_total > 100000)
  expect_true(state_row$row_total < 200000)
})

test_that("fetch_enr works for Era 2 (2013-2020)", {
  skip_on_cran()
  skip_if_offline()

  # Use 2015 which has more consistent naming
  result <- fetch_enr(2015, tidy = FALSE, use_cache = TRUE)

  expect_true(is.data.frame(result))
  expect_true("State" %in% result$type)
  expect_true("District" %in% result$type)

  # Check total is reasonable
  state_row <- result[result$type == "State", ]
  expect_true(state_row$row_total > 100000)
})

test_that("fetch_enr works for Era 1 (2011-2012)", {
  skip_on_cran()
  skip_if_offline()

  result <- fetch_enr(2011, tidy = FALSE, use_cache = TRUE)

  expect_true(is.data.frame(result))
  expect_true("State" %in% result$type)
  expect_true("District" %in% result$type)

  # Check total is reasonable
  state_row <- result[result$type == "State", ]
  expect_true(state_row$row_total > 100000)
})

test_that("tidy_enr produces correct long format", {
  skip_on_cran()
  skip_if_offline()

  # Get wide data
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  # Tidy it
  tidy_result <- tidy_enr(wide)

  # Check structure
  expect_true("grade_level" %in% names(tidy_result))
  expect_true("subgroup" %in% names(tidy_result))
  expect_true("n_students" %in% names(tidy_result))
  expect_true("pct" %in% names(tidy_result))

  # Check subgroups include expected values
  subgroups <- unique(tidy_result$subgroup)
  expect_true("total_enrollment" %in% subgroups)
})

test_that("id_enr_aggs adds correct flags", {
  skip_on_cran()
  skip_if_offline()

  # Get tidy data with aggregation flags
  result <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # Check flags exist
  expect_true("is_state" %in% names(result))
  expect_true("is_district" %in% names(result))
  expect_true("is_campus" %in% names(result))
  expect_true("is_public" %in% names(result))

  # Check flags are boolean
  expect_true(is.logical(result$is_state))
  expect_true(is.logical(result$is_district))

  # Check that at least state and district flags are used
  expect_true(any(result$is_state))
  expect_true(any(result$is_district))
})

test_that("fetch_enr_multi works for multiple years", {
  skip_on_cran()
  skip_if_offline()

  # Use years that are more likely to have consistent data
  result <- fetch_enr_multi(c(2024, 2025), tidy = TRUE, use_cache = TRUE)

  expect_true(is.data.frame(result))
  expect_true(all(c(2024, 2025) %in% result$end_year))

  # Check each year has state totals
  state_data <- result[result$is_state & result$subgroup == "total_enrollment" &
                       result$grade_level == "TOTAL", ]
  expect_equal(nrow(state_data), 2)
})

test_that("grade columns are present in data", {
  skip_on_cran()
  skip_if_offline()

  result <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  # Check for key grade columns
  expect_true("grade_pk" %in% names(result) || "grade_k" %in% names(result))
  expect_true("grade_01" %in% names(result) || "grade_12" %in% names(result))
})

test_that("enr_grade_aggs creates grade groupings", {
  skip_on_cran()
  skip_if_offline()

  tidy_data <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  grade_aggs <- enr_grade_aggs(tidy_data)

  expect_true(is.data.frame(grade_aggs))
  expect_true("K8" %in% grade_aggs$grade_level)
  expect_true("HS" %in% grade_aggs$grade_level)
  expect_true("K12" %in% grade_aggs$grade_level)
})
