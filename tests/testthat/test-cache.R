# Tests for caching functions

test_that("get_cache_dir creates directory if needed", {
  cache_dir <- get_cache_dir()
  expect_true(dir.exists(cache_dir))
  expect_true(grepl("sdschooldata", cache_dir))
})

test_that("get_cache_path returns correct format", {
  path <- get_cache_path(2024, "tidy")
  expect_true(grepl("enr_tidy_2024\\.rds$", path))

  path <- get_cache_path(2023, "wide")
  expect_true(grepl("enr_wide_2023\\.rds$", path))
})

test_that("cache_exists returns FALSE for missing files", {
  # Year 1900 should never have cached data
  expect_false(cache_exists(1900, "tidy"))
  expect_false(cache_exists(1900, "wide"))
})

test_that("cache read/write roundtrip works", {
  # Create test data
  test_df <- data.frame(
    district_id = c("01001", "01002"),
    enrollment = c(100, 200),
    stringsAsFactors = FALSE
  )

  # Write to cache
  test_year <- 1901  # Unlikely to conflict with real data
  write_cache(test_df, test_year, "test")

  # Read back
  result <- read_cache(test_year, "test")

  expect_equal(result, test_df)

  # Clean up
  cache_path <- get_cache_path(test_year, "test")
  if (file.exists(cache_path)) {
    file.remove(cache_path)
  }
})

test_that("cache_exists respects max_age", {
  # Create a cached file
  test_df <- data.frame(x = 1)
  test_year <- 1902
  write_cache(test_df, test_year, "test")

  # Should exist with default max_age
  expect_true(cache_exists(test_year, "test", max_age = 30))

  # Should exist with very short max_age (just created)
  expect_true(cache_exists(test_year, "test", max_age = 0.001))

  # Clean up
  cache_path <- get_cache_path(test_year, "test")
  if (file.exists(cache_path)) {
    file.remove(cache_path)
  }
})

test_that("clear_cache removes files", {
  # Create test file
  test_df <- data.frame(x = 1)
  test_year <- 1903
  write_cache(test_df, test_year, "test")

  cache_path <- get_cache_path(test_year, "test")
  expect_true(file.exists(cache_path))

  # Clear specific file
  clear_cache(test_year, "test")
  expect_false(file.exists(cache_path))
})
