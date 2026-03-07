# ==============================================================================
# Exhaustive Enrollment Tests for sdschooldata
# ==============================================================================
#
# Tests every exported enrollment function with every parameter combination.
# All pinned values come from real South Dakota DOE data fetched via cache.
#
# ==============================================================================

# ==============================================================================
# SECTION 1: fetch_enr() — tidy = TRUE, 2024
# ==============================================================================

test_that("fetch_enr(2024) returns correct structure", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  expect_s3_class(enr, "data.frame")
  expect_equal(ncol(enr), 17)
  expect_equal(nrow(enr), 15274)
})

test_that("fetch_enr(2024) has all required columns", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  required_cols <- c(
    "end_year", "type", "district_id", "campus_id",
    "district_name", "campus_name",
    "district_type_code", "district_type_name",
    "grade_level", "subgroup", "n_students", "pct",
    "is_state", "is_district", "is_campus",
    "aggregation_flag", "is_public"
  )
  for (col in required_cols) {
    expect_true(col %in% names(enr), info = paste("Missing column:", col))
  }
})

test_that("fetch_enr(2024) column types are correct", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  expect_type(enr$end_year, "integer")
  expect_type(enr$type, "character")
  expect_type(enr$district_id, "character")
  expect_type(enr$campus_id, "character")
  expect_type(enr$district_name, "character")
  expect_type(enr$campus_name, "character")
  expect_type(enr$district_type_code, "character")
  expect_type(enr$district_type_name, "character")
  expect_type(enr$grade_level, "character")
  expect_type(enr$subgroup, "character")
  expect_type(enr$n_students, "double")
  expect_type(enr$pct, "double")
  expect_type(enr$is_state, "logical")
  expect_type(enr$is_district, "logical")
  expect_type(enr$is_campus, "logical")
  expect_type(enr$aggregation_flag, "character")
  expect_type(enr$is_public, "logical")
})

test_that("fetch_enr(2024) state total enrollment is 140587", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state_total <- enr[enr$is_state & enr$subgroup == "total_enrollment" &
                       enr$grade_level == "TOTAL", ]

  expect_equal(nrow(state_total), 1)
  expect_equal(state_total$n_students, 140587)
  expect_equal(state_total$pct, 1)
  expect_equal(state_total$end_year, 2024L)
  expect_true(state_total$is_state)
  expect_false(state_total$is_district)
  expect_false(state_total$is_campus)
})

test_that("fetch_enr(2024) has correct state grade breakdowns", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state_grades <- enr[enr$is_state & enr$subgroup == "total_enrollment" &
                        enr$grade_level != "TOTAL", ]

  # Pin every grade level
  expected <- data.frame(
    grade_level = c("PK", "K", "01", "02", "03", "04", "05", "06",
                    "07", "08", "09", "10", "11", "12"),
    n_students = c(3274, 11383, 10250, 10521, 10706, 10256, 10522, 10624,
                   10432, 10486, 11424, 10733, 10031, 9945),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(expected))) {
    gl <- expected$grade_level[i]
    actual <- state_grades[state_grades$grade_level == gl, "n_students"]
    expect_equal(actual, expected$n_students[i],
                 info = paste("Grade", gl, "mismatch"))
  }
})

test_that("fetch_enr(2024) grade sum equals state total", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  state_total <- enr$n_students[enr$is_state & enr$subgroup == "total_enrollment" &
                                  enr$grade_level == "TOTAL"]
  grade_sum <- sum(enr$n_students[enr$is_state & enr$subgroup == "total_enrollment" &
                                    enr$grade_level != "TOTAL"], na.rm = TRUE)

  expect_equal(grade_sum, state_total)
})

test_that("fetch_enr(2024) has 148 districts", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  n_districts <- sum(enr$is_district & enr$subgroup == "total_enrollment" &
                       enr$grade_level == "TOTAL")

  expect_equal(n_districts, 148)
})

test_that("fetch_enr(2024) has 684 campuses", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  n_campuses <- sum(enr$is_campus & enr$subgroup == "total_enrollment" &
                      enr$grade_level == "TOTAL")

  expect_equal(n_campuses, 684)
})

test_that("fetch_enr(2024) district sum equals state total", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  state_total <- enr$n_students[enr$is_state & enr$subgroup == "total_enrollment" &
                                  enr$grade_level == "TOTAL"]
  dist_sum <- sum(enr$n_students[enr$is_district & enr$subgroup == "total_enrollment" &
                                   enr$grade_level == "TOTAL"], na.rm = TRUE)

  expect_equal(dist_sum, state_total)
})

test_that("fetch_enr(2024) Sioux Falls has 25060 students", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  sf <- enr[enr$is_district & grepl("Sioux Falls", enr$district_name) &
              enr$subgroup == "total_enrollment" & enr$grade_level == "TOTAL", ]

  expect_equal(nrow(sf), 1)
  expect_equal(sf$district_id, "49005")
  expect_equal(sf$n_students, 25060)
})

test_that("fetch_enr(2024) Rapid City has 12313 students", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  rc <- enr[enr$is_district & grepl("Rapid City", enr$district_name) &
              enr$subgroup == "total_enrollment" & enr$grade_level == "TOTAL", ]

  expect_equal(nrow(rc), 1)
  expect_equal(rc$district_id, "51004")
  expect_equal(rc$n_students, 12313)
})

test_that("fetch_enr(2024) has correct subgroup values", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  subgroups <- sort(unique(enr$subgroup))

  expected_subgroups <- c("asian", "black", "hispanic", "multiracial",
                          "native_american", "pacific_islander",
                          "total_enrollment", "white")

  expect_equal(subgroups, expected_subgroups)
})

test_that("fetch_enr(2024) has correct grade level values", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  grades <- sort(unique(enr$grade_level))

  expected_grades <- c("01", "02", "03", "04", "05", "06", "07", "08",
                       "09", "10", "11", "12", "K", "PK", "TOTAL")

  expect_equal(grades, expected_grades)
})

test_that("fetch_enr(2024) has correct type values", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  types <- sort(unique(enr$type))

  expect_equal(types, c("Campus", "District", "State"))
})

test_that("fetch_enr(2024) has correct aggregation_flag values", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  agg_flags <- sort(unique(enr$aggregation_flag))

  expect_equal(agg_flags, c("campus", "district", "state"))
})

# ==============================================================================
# SECTION 2: fetch_enr() — tidy = TRUE, 2025
# ==============================================================================

test_that("fetch_enr(2025) state total is 138861", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2025, tidy = TRUE, use_cache = TRUE)
  state_total <- enr[enr$is_state & enr$subgroup == "total_enrollment" &
                       enr$grade_level == "TOTAL", ]

  expect_equal(state_total$n_students, 138861)
})

test_that("fetch_enr(2025) has correct state grade breakdowns", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2025, tidy = TRUE, use_cache = TRUE)
  state_grades <- enr[enr$is_state & enr$subgroup == "total_enrollment" &
                        enr$grade_level != "TOTAL", ]

  expected <- data.frame(
    grade_level = c("PK", "K", "01", "02", "03", "04", "05", "06",
                    "07", "08", "09", "10", "11", "12"),
    n_students = c(3284, 10954, 9847, 10201, 10527, 10724, 10314, 10575,
                   10493, 10435, 11041, 10660, 9842, 9964),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(expected))) {
    gl <- expected$grade_level[i]
    actual <- state_grades$n_students[state_grades$grade_level == gl]
    expect_equal(actual, expected$n_students[i],
                 info = paste("2025 grade", gl, "mismatch"))
  }
})

test_that("fetch_enr(2025) has 147 districts", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2025, tidy = TRUE, use_cache = TRUE)
  n_districts <- sum(enr$is_district & enr$subgroup == "total_enrollment" &
                       enr$grade_level == "TOTAL")

  expect_equal(n_districts, 147)
})

test_that("fetch_enr(2025) has 660 campuses", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2025, tidy = TRUE, use_cache = TRUE)
  n_campuses <- sum(enr$is_campus & enr$subgroup == "total_enrollment" &
                      enr$grade_level == "TOTAL")

  expect_equal(n_campuses, 660)
})

test_that("fetch_enr(2025) Sioux Falls has 24841 students", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2025, tidy = TRUE, use_cache = TRUE)
  sf <- enr[enr$is_district & grepl("Sioux Falls", enr$district_name) &
              enr$subgroup == "total_enrollment" & enr$grade_level == "TOTAL", ]

  expect_equal(sf$n_students, 24841)
  expect_equal(sf$district_id, "49005")
})

test_that("fetch_enr(2025) Rapid City has 12040 students", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2025, tidy = TRUE, use_cache = TRUE)
  rc <- enr[enr$is_district & grepl("Rapid City", enr$district_name) &
              enr$subgroup == "total_enrollment" & enr$grade_level == "TOTAL", ]

  expect_equal(rc$n_students, 12040)
})

test_that("fetch_enr(2025) Aberdeen has 4134 students", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2025, tidy = TRUE, use_cache = TRUE)
  ab <- enr[enr$is_district & grepl("Aberdeen", enr$district_name) &
              enr$subgroup == "total_enrollment" & enr$grade_level == "TOTAL", ]

  expect_equal(ab$district_id, "06001")
  expect_equal(ab$n_students, 4134)
})

test_that("fetch_enr(2025) Watertown has 3425 students", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2025, tidy = TRUE, use_cache = TRUE)
  wt <- enr[enr$is_district & grepl("Watertown", enr$district_name) &
              enr$subgroup == "total_enrollment" & enr$grade_level == "TOTAL", ]

  expect_equal(wt$district_id, "14004")
  expect_equal(wt$n_students, 3425)
})

test_that("fetch_enr(2025) campus-level race counts are correct", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2025, tidy = TRUE, use_cache = TRUE)

  # Sum of each race across all campuses at TOTAL grade level
  campus_race <- enr[enr$is_campus & enr$grade_level == "TOTAL" &
                       enr$subgroup != "total_enrollment", ]

  race_sums <- tapply(campus_race$n_students, campus_race$subgroup, sum, na.rm = TRUE)

  expect_equal(as.numeric(race_sums["white"]), 95447)
  expect_equal(as.numeric(race_sums["native_american"]), 14283)
  expect_equal(as.numeric(race_sums["hispanic"]), 12845)
  expect_equal(as.numeric(race_sums["multiracial"]), 8681)
  expect_equal(as.numeric(race_sums["black"]), 5051)
  expect_equal(as.numeric(race_sums["asian"]), 2308)
  expect_equal(as.numeric(race_sums["pacific_islander"]), 246)
})

# ==============================================================================
# SECTION 3: fetch_enr() — tidy = FALSE (wide format), 2024
# ==============================================================================

test_that("fetch_enr(2024, tidy=FALSE) returns wide format", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  expect_s3_class(wide, "data.frame")
  expect_equal(nrow(wide), 833)
  expect_equal(ncol(wide), 32)
})

test_that("fetch_enr(2024, tidy=FALSE) has correct columns", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  expected_cols <- c(
    "end_year", "type", "district_id", "campus_id",
    "district_name", "campus_name", "row_total",
    "grade_pk", "grade_k", "grade_01", "grade_02", "grade_03",
    "grade_04", "grade_05", "grade_06", "grade_07", "grade_08",
    "grade_09", "grade_10", "grade_11", "grade_12",
    "district_type_code", "district_type_name",
    "native_american", "asian", "black", "pacific_islander",
    "hispanic", "multiracial", "white", "male", "female"
  )

  for (col in expected_cols) {
    expect_true(col %in% names(wide),
                info = paste("Missing wide column:", col))
  }
})

test_that("fetch_enr(2024, tidy=FALSE) state row has correct values", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  state <- wide[wide$type == "State", ]

  expect_equal(nrow(state), 1)
  expect_equal(state$row_total, 140587)
  expect_equal(state$grade_pk, 3274)
  expect_equal(state$grade_k, 11383)
  expect_equal(state$grade_01, 10250)
  expect_equal(state$grade_02, 10521)
  expect_equal(state$grade_03, 10706)
  expect_equal(state$grade_04, 10256)
  expect_equal(state$grade_05, 10522)
  expect_equal(state$grade_06, 10624)
  expect_equal(state$grade_07, 10432)
  expect_equal(state$grade_08, 10486)
  expect_equal(state$grade_09, 11424)
  expect_equal(state$grade_10, 10733)
  expect_equal(state$grade_11, 10031)
  expect_equal(state$grade_12, 9945)
})

test_that("fetch_enr(2024, tidy=FALSE) wide total matches tidy total", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy_data <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  wide_state_total <- wide$row_total[wide$type == "State"]
  tidy_state_total <- tidy_data$n_students[tidy_data$is_state &
                                             tidy_data$subgroup == "total_enrollment" &
                                             tidy_data$grade_level == "TOTAL"]

  expect_equal(wide_state_total, tidy_state_total)
})

test_that("fetch_enr(2024, tidy=FALSE) Aberdeen Central HS campus data", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  aberdeen_hs <- wide[wide$type == "Campus" &
                        grepl("Central High School", wide$campus_name) &
                        grepl("Aberdeen", wide$district_name), ]

  expect_equal(nrow(aberdeen_hs), 1)
  expect_equal(aberdeen_hs$row_total, 1384)
  expect_equal(aberdeen_hs$white, 1022)
  expect_equal(aberdeen_hs$black, 31)
  expect_equal(aberdeen_hs$hispanic, 121)
  expect_equal(aberdeen_hs$asian, 38)
  expect_equal(aberdeen_hs$native_american, 93)
  expect_equal(aberdeen_hs$pacific_islander, 9)
  expect_equal(aberdeen_hs$multiracial, 70)
})

test_that("fetch_enr(2024, tidy=FALSE) C.C. Lee Elementary campus data", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  cclee <- wide[wide$type == "Campus" &
                  grepl("C.C. Lee", wide$campus_name) &
                  grepl("Aberdeen", wide$district_name), ]

  expect_equal(nrow(cclee), 1)
  expect_equal(cclee$row_total, 356)
  expect_equal(cclee$white, 289)
  expect_equal(cclee$grade_k, 47)
  expect_equal(cclee$grade_01, 59)
  expect_equal(cclee$grade_02, 60)
  expect_equal(cclee$grade_03, 65)
  expect_equal(cclee$grade_04, 64)
  expect_equal(cclee$grade_05, 61)
})

# ==============================================================================
# SECTION 4: fetch_enr() — Historical Years
# ==============================================================================

test_that("fetch_enr(2023) state total is 141005", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)
  state <- enr[enr$is_state & enr$subgroup == "total_enrollment" &
                 enr$grade_level == "TOTAL", ]

  expect_equal(state$n_students, 141005)
})

test_that("fetch_enr(2023) has 148 districts", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)
  n_dist <- sum(enr$is_district & enr$subgroup == "total_enrollment" &
                  enr$grade_level == "TOTAL")

  expect_equal(n_dist, 148)
})

test_that("fetch_enr(2022) state total is 141429", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2022, tidy = TRUE, use_cache = TRUE)
  state <- enr[enr$is_state & enr$subgroup == "total_enrollment" &
                 enr$grade_level == "TOTAL", ]

  expect_equal(state$n_students, 141429)
})

test_that("fetch_enr(2015) state total is 134054", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2015, tidy = TRUE, use_cache = TRUE)
  state <- enr[enr$is_state & enr$subgroup == "total_enrollment" &
                 enr$grade_level == "TOTAL", ]

  expect_equal(state$n_students, 134054)
})

test_that("fetch_enr(2015) has 150 districts", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2015, tidy = TRUE, use_cache = TRUE)
  n_dist <- sum(enr$is_district & enr$subgroup == "total_enrollment" &
                  enr$grade_level == "TOTAL")

  expect_equal(n_dist, 150)
})

test_that("fetch_enr(2015) Sioux Falls has 24216 students", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2015, tidy = TRUE, use_cache = TRUE)
  sf <- enr[enr$is_district & grepl("Sioux Falls", enr$district_name) &
              enr$subgroup == "total_enrollment" & enr$grade_level == "TOTAL", ]

  expect_equal(sf$n_students, 24216)
})

test_that("fetch_enr(2011) state total is 127769 (Era 1)", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2011, tidy = TRUE, use_cache = TRUE)
  state <- enr[enr$is_state & enr$subgroup == "total_enrollment" &
                 enr$grade_level == "TOTAL", ]

  expect_equal(state$n_students, 127769)
})

test_that("fetch_enr(2011) has 152 districts (Era 1)", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2011, tidy = TRUE, use_cache = TRUE)
  n_dist <- sum(enr$is_district & enr$subgroup == "total_enrollment" &
                  enr$grade_level == "TOTAL")

  expect_equal(n_dist, 152)
})

test_that("fetch_enr(2011) has no campus-level data (Era 1)", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2011, tidy = TRUE, use_cache = TRUE)
  n_campuses <- sum(enr$is_campus & enr$subgroup == "total_enrollment" &
                      enr$grade_level == "TOTAL")

  expect_equal(n_campuses, 0)
})

test_that("fetch_enr(2015) only has total_enrollment subgroup (Era 2)", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2015, tidy = TRUE, use_cache = TRUE)
  subgroups <- unique(enr$subgroup)

  expect_equal(subgroups, "total_enrollment")
})

# ==============================================================================
# SECTION 5: fetch_enr() — Year Validation
# ==============================================================================

test_that("fetch_enr rejects year below range", {
  expect_error(fetch_enr(2005), "end_year must be between")
  expect_error(fetch_enr(2000), "end_year must be between")
  expect_error(fetch_enr(1990), "end_year must be between")
})

test_that("fetch_enr rejects year above range", {
  expect_error(fetch_enr(2027), "end_year must be between")
  expect_error(fetch_enr(2030), "end_year must be between")
  expect_error(fetch_enr(3000), "end_year must be between")
})

# ==============================================================================
# SECTION 6: fetch_enr_multi()
# ==============================================================================

test_that("fetch_enr_multi(2023:2025) returns all years", {
  skip_on_cran()
  skip_if_offline()

  multi <- fetch_enr_multi(2023:2025, tidy = TRUE, use_cache = TRUE)

  expect_s3_class(multi, "data.frame")
  expect_equal(sort(unique(multi$end_year)), c(2023L, 2024L, 2025L))
})

test_that("fetch_enr_multi(2023:2025) has correct state totals per year", {
  skip_on_cran()
  skip_if_offline()

  multi <- fetch_enr_multi(2023:2025, tidy = TRUE, use_cache = TRUE)
  state_totals <- multi[multi$is_state & multi$subgroup == "total_enrollment" &
                          multi$grade_level == "TOTAL",
                        c("end_year", "n_students")]

  expect_equal(state_totals$n_students[state_totals$end_year == 2023], 141005)
  expect_equal(state_totals$n_students[state_totals$end_year == 2024], 140587)
  expect_equal(state_totals$n_students[state_totals$end_year == 2025], 138861)
})

test_that("fetch_enr_multi(2023:2025) row count equals sum of individual years", {
  skip_on_cran()
  skip_if_offline()

  multi <- fetch_enr_multi(2023:2025, tidy = TRUE, use_cache = TRUE)

  # Multi should have expected row count
  expect_equal(nrow(multi), 45080)
})

test_that("fetch_enr_multi validates years", {
  expect_error(fetch_enr_multi(c(2024, 2030)), "Invalid years")
  expect_error(fetch_enr_multi(c(2005, 2024)), "Invalid years")
})

test_that("fetch_enr_multi single year works", {
  skip_on_cran()
  skip_if_offline()

  single <- fetch_enr_multi(2024, tidy = TRUE, use_cache = TRUE)
  direct <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  expect_equal(nrow(single), nrow(direct))
  expect_equal(unique(single$end_year), 2024L)
})

test_that("fetch_enr_multi(tidy=FALSE) returns wide format for all years", {
  skip_on_cran()
  skip_if_offline()

  multi_wide <- fetch_enr_multi(2023:2024, tidy = FALSE, use_cache = TRUE)

  expect_true("row_total" %in% names(multi_wide))
  expect_false("subgroup" %in% names(multi_wide))
  expect_true(all(c(2023, 2024) %in% multi_wide$end_year))
})

# ==============================================================================
# SECTION 7: enr_grade_aggs()
# ==============================================================================

test_that("enr_grade_aggs(2024) state-level K8 is 95180", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  state_aggs <- aggs[aggs$is_state, ]

  expect_equal(state_aggs$n_students[state_aggs$grade_level == "K8"], 95180)
})

test_that("enr_grade_aggs(2024) state-level HS is 42133", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  state_aggs <- aggs[aggs$is_state, ]

  expect_equal(state_aggs$n_students[state_aggs$grade_level == "HS"], 42133)
})

test_that("enr_grade_aggs(2024) state-level K12 is 137313", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  state_aggs <- aggs[aggs$is_state, ]

  expect_equal(state_aggs$n_students[state_aggs$grade_level == "K12"], 137313)
})

test_that("enr_grade_aggs(2024) K8 + HS = K12", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  state_aggs <- aggs[aggs$is_state, ]

  k8 <- state_aggs$n_students[state_aggs$grade_level == "K8"]
  hs <- state_aggs$n_students[state_aggs$grade_level == "HS"]
  k12 <- state_aggs$n_students[state_aggs$grade_level == "K12"]

  expect_equal(k8 + hs, k12)
})

test_that("enr_grade_aggs(2025) state-level values are correct", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2025, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  state_aggs <- aggs[aggs$is_state, ]

  expect_equal(state_aggs$n_students[state_aggs$grade_level == "K8"], 94070)
  expect_equal(state_aggs$n_students[state_aggs$grade_level == "HS"], 41507)
  expect_equal(state_aggs$n_students[state_aggs$grade_level == "K12"], 135577)
})

test_that("enr_grade_aggs Sioux Falls K8/HS/K12 for 2024", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)
  sf_aggs <- aggs[aggs$is_district & grepl("Sioux Falls", aggs$district_name), ]

  expect_equal(sf_aggs$n_students[sf_aggs$grade_level == "K8"], 16671)
  expect_equal(sf_aggs$n_students[sf_aggs$grade_level == "HS"], 7550)
  expect_equal(sf_aggs$n_students[sf_aggs$grade_level == "K12"], 24221)
})

test_that("enr_grade_aggs returns all three grade levels", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)

  expect_true("K8" %in% aggs$grade_level)
  expect_true("HS" %in% aggs$grade_level)
  expect_true("K12" %in% aggs$grade_level)
})

test_that("enr_grade_aggs preserves entity flags", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)

  expect_true("is_state" %in% names(aggs))
  expect_true("is_district" %in% names(aggs))
  expect_true("is_campus" %in% names(aggs))
  expect_true("is_public" %in% names(aggs))
  expect_true(any(aggs$is_state))
  expect_true(any(aggs$is_district))
})

test_that("enr_grade_aggs pct is NA", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)

  expect_true(all(is.na(aggs$pct)))
})

test_that("enr_grade_aggs only uses total_enrollment subgroup", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(enr)

  expect_equal(unique(aggs$subgroup), "total_enrollment")
})

# ==============================================================================
# SECTION 8: Campus-Level Race Data — 2024
# ==============================================================================

test_that("Aberdeen Central HS tidy race data is correct", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aberdeen <- enr[enr$is_campus &
                    grepl("Central High School", enr$campus_name) &
                    grepl("Aberdeen", enr$district_name) &
                    enr$grade_level == "TOTAL", ]

  # Pin race values
  expect_equal(aberdeen$n_students[aberdeen$subgroup == "total_enrollment"], 1384)
  expect_equal(aberdeen$n_students[aberdeen$subgroup == "white"], 1022)
  expect_equal(aberdeen$n_students[aberdeen$subgroup == "black"], 31)
  expect_equal(aberdeen$n_students[aberdeen$subgroup == "hispanic"], 121)
  expect_equal(aberdeen$n_students[aberdeen$subgroup == "asian"], 38)
  expect_equal(aberdeen$n_students[aberdeen$subgroup == "native_american"], 93)
  expect_equal(aberdeen$n_students[aberdeen$subgroup == "pacific_islander"], 9)
  expect_equal(aberdeen$n_students[aberdeen$subgroup == "multiracial"], 70)
})

test_that("race subgroup percentages sum to approximately 1.0 per campus", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # Check Aberdeen Central HS race pcts sum to ~1
  aberdeen <- enr[enr$is_campus &
                    grepl("Central High School", enr$campus_name) &
                    grepl("Aberdeen", enr$district_name) &
                    enr$grade_level == "TOTAL" &
                    !enr$subgroup %in% c("total_enrollment", "male", "female"), ]

  pct_sum <- sum(aberdeen$pct, na.rm = TRUE)
  expect_true(abs(pct_sum - 1.0) < 0.01,
              info = paste("Race pcts sum to", pct_sum, "expected ~1.0"))
})

test_that("race data only exists at campus level", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  race_subgroups <- c("white", "black", "hispanic", "asian",
                      "native_american", "pacific_islander", "multiracial")

  # Race data should NOT exist at state or district level in 2024
  state_race <- enr[enr$is_state & enr$subgroup %in% race_subgroups, ]
  expect_equal(nrow(state_race), 0)

  district_race <- enr[enr$is_district & enr$subgroup %in% race_subgroups, ]
  expect_equal(nrow(district_race), 0)

  # Race data SHOULD exist at campus level
  campus_race <- enr[enr$is_campus & enr$subgroup %in% race_subgroups, ]
  expect_gt(nrow(campus_race), 0)
})

# ==============================================================================
# SECTION 9: get_available_years()
# ==============================================================================

test_that("get_available_years returns 2006:2026", {
  years <- get_available_years()

  expect_equal(years, 2006:2026)
  expect_true(is.integer(years))
  expect_equal(length(years), 21)
  expect_equal(min(years), 2006L)
  expect_equal(max(years), 2026L)
})

# ==============================================================================
# SECTION 10: tidy_enr() and id_enr_aggs()
# ==============================================================================

test_that("tidy_enr transforms wide to long correctly", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy_result <- tidy_enr(wide)

  expect_true("grade_level" %in% names(tidy_result))
  expect_true("subgroup" %in% names(tidy_result))
  expect_true("n_students" %in% names(tidy_result))
  expect_true("pct" %in% names(tidy_result))
  expect_gt(nrow(tidy_result), nrow(wide))
})

test_that("tidy_enr preserves total enrollment values", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy_result <- tidy_enr(wide)

  # State row total in wide
  wide_state_total <- wide$row_total[wide$type == "State"]

  # Should match in tidy
  tidy_state_total <- tidy_result$n_students[tidy_result$type == "State" &
                                               tidy_result$subgroup == "total_enrollment" &
                                               tidy_result$grade_level == "TOTAL"]

  expect_equal(tidy_state_total, wide_state_total)
})

test_that("tidy_enr filters out NA n_students rows", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy_result <- tidy_enr(wide)

  expect_false(any(is.na(tidy_result$n_students)))
})

test_that("id_enr_aggs adds all required flags", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy_result <- tidy_enr(wide)
  with_aggs <- id_enr_aggs(tidy_result)

  expect_true("is_state" %in% names(with_aggs))
  expect_true("is_district" %in% names(with_aggs))
  expect_true("is_campus" %in% names(with_aggs))
  expect_true("aggregation_flag" %in% names(with_aggs))
  expect_true("is_public" %in% names(with_aggs))
})

test_that("id_enr_aggs flags are mutually consistent", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # State rows should not be district or campus
  state_rows <- enr[enr$is_state, ]
  expect_true(all(!state_rows$is_district))
  expect_true(all(!state_rows$is_campus))

  # District rows should not be state or campus
  district_rows <- enr[enr$is_district, ]
  expect_true(all(!district_rows$is_state))
  expect_true(all(!district_rows$is_campus))

  # Campus rows should not be state or district
  campus_rows <- enr[enr$is_campus, ]
  expect_true(all(!campus_rows$is_state))
  expect_true(all(!campus_rows$is_district))
})

test_that("id_enr_aggs aggregation_flag matches entity flags", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  state_rows <- enr[enr$is_state, ]
  expect_true(all(state_rows$aggregation_flag == "state"))

  district_rows <- enr[enr$is_district, ]
  expect_true(all(district_rows$aggregation_flag == "district"))

  campus_rows <- enr[enr$is_campus, ]
  expect_true(all(campus_rows$aggregation_flag == "campus"))
})

test_that("is_public flag is TRUE for all 2024 data", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  expect_true(all(enr$is_public))
})

test_that("district_type_code is '10' for all districts in 2024", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  district_rows <- enr[enr$is_district & !is.na(enr$district_type_code), ]

  expect_true(all(district_rows$district_type_code == "10"))
})

# ==============================================================================
# SECTION 11: cache_status() and clear_cache()
# ==============================================================================

test_that("cache_status returns data frame", {
  result <- cache_status()
  expect_true(is.data.frame(result))
})

test_that("clear_cache with specific year and type works", {
  # Write a test cache entry
  test_df <- data.frame(x = 1:3)
  test_year <- 1950
  write_cache(test_df, test_year, "test")

  # Verify it exists
  expect_true(cache_exists(test_year, "test"))

  # Clear it
  clear_cache(test_year, "test")

  # Verify it's gone
  path <- get_cache_path(test_year, "test")
  expect_false(file.exists(path))
})

test_that("clear_cache with only year clears all types for year", {
  test_df <- data.frame(x = 1)
  test_year <- 1951

  write_cache(test_df, test_year, "tidy")
  write_cache(test_df, test_year, "wide")

  expect_true(cache_exists(test_year, "tidy"))
  expect_true(cache_exists(test_year, "wide"))

  clear_cache(test_year)

  expect_false(file.exists(get_cache_path(test_year, "tidy")))
  expect_false(file.exists(get_cache_path(test_year, "wide")))
})

test_that("clear_cache with only type clears all years for type", {
  test_df <- data.frame(x = 1)

  write_cache(test_df, 1952, "testtype")
  write_cache(test_df, 1953, "testtype")

  clear_cache(type = "testtype")

  expect_false(file.exists(get_cache_path(1952, "testtype")))
  expect_false(file.exists(get_cache_path(1953, "testtype")))
})
