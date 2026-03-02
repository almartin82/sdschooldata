test_that("fetch_directory returns data", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- fetch_directory(use_cache = FALSE)

  expect_s3_class(dir_data, "tbl_df")
  expect_gt(nrow(dir_data), 50)
  expect_true("district_name" %in% names(dir_data))
  expect_true("school_name" %in% names(dir_data))
  expect_true("superintendent_name" %in% names(dir_data))
  expect_true("principal_name" %in% names(dir_data))
  expect_true("state_district_id" %in% names(dir_data))
  expect_true("state_school_id" %in% names(dir_data))
  expect_true("address" %in% names(dir_data))
  expect_true("city" %in% names(dir_data))
  expect_true("state" %in% names(dir_data))
  expect_true("zip" %in% names(dir_data))
  expect_true("county_name" %in% names(dir_data))
  expect_true("grades_served" %in% names(dir_data))
  expect_true("website" %in% names(dir_data))

  # Almost all state values should be SD (a few border schools may be in ND)
  non_na_states <- dir_data$state[!is.na(dir_data$state)]
  pct_sd <- mean(non_na_states == "SD")
  expect_gt(pct_sd, 0.95)
})

test_that("fetch_directory raw format works", {
  skip_on_cran()
  skip_if_offline()

  dir_raw <- fetch_directory(tidy = FALSE, use_cache = FALSE)

  expect_s3_class(dir_raw, "tbl_df")
  expect_gt(nrow(dir_raw), 50)
  # Raw format should have original SD DOE column names
  expect_true("District Name" %in% names(dir_raw))
  expect_true("District Number" %in% names(dir_raw))
  expect_true("School Name" %in% names(dir_raw))
})

test_that("directory cache works", {
  skip_on_cran()
  skip_if_offline()

  dir1 <- fetch_directory(use_cache = TRUE)
  dir2 <- fetch_directory(use_cache = TRUE)

  expect_equal(nrow(dir1), nrow(dir2))

  clear_directory_cache()
})

test_that("fetch_directory has known districts", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- fetch_directory(use_cache = TRUE)

  # Sioux Falls should be present
  sf <- dir_data[grepl("Sioux Falls", dir_data$district_name), ]
  expect_gt(nrow(sf), 0)

  # Rapid City should be present
  rc <- dir_data[grepl("Rapid City", dir_data$district_name), ]
  expect_gt(nrow(rc), 0)

  # Aberdeen should be present
  ab <- dir_data[grepl("Aberdeen", dir_data$district_name), ]
  expect_gt(nrow(ab), 0)
})

test_that("fetch_directory superintendent join works", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- fetch_directory(use_cache = TRUE)

  # At least some rows should have superintendent names
  has_supt <- sum(!is.na(dir_data$superintendent_name))
  expect_gt(has_supt, 0)
})
