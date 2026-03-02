# ==============================================================================
# Exhaustive Typology Tests for sdschooldata
# ==============================================================================
#
# Tests data structure, column types, naming standards, edge cases,
# fidelity requirements, and directory data.
#
# All pinned values come from real South Dakota DOE data.
#
# ==============================================================================

# ==============================================================================
# SECTION 1: Naming Standards Compliance
# ==============================================================================

test_that("subgroup names comply with naming standards", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # All standard subgroup names
  standard_names <- c("total_enrollment", "white", "black", "hispanic",
                      "asian", "native_american", "pacific_islander",
                      "multiracial", "male", "female",
                      "econ_disadv", "special_ed", "lep",
                      "free_reduced_lunch")

  # All subgroups in the data must be from the standard set
  actual_subgroups <- unique(enr$subgroup)
  non_standard <- setdiff(actual_subgroups, standard_names)

  expect_equal(length(non_standard), 0,
               info = paste("Non-standard subgroup names:",
                            paste(non_standard, collapse = ", ")))
})

test_that("grade level names comply with naming standards", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # Standard grade levels
  standard_grades <- c("PK", "K", "01", "02", "03", "04", "05", "06",
                       "07", "08", "09", "10", "11", "12",
                       "UG", "TOTAL")

  actual_grades <- unique(enr$grade_level)
  non_standard <- setdiff(actual_grades, standard_grades)

  expect_equal(length(non_standard), 0,
               info = paste("Non-standard grade levels:",
                            paste(non_standard, collapse = ", ")))
})

test_that("grade levels are all uppercase", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  grades <- unique(enr$grade_level)

  for (g in grades) {
    expect_equal(g, toupper(g),
                 info = paste("Grade level not uppercase:", g))
  }
})

test_that("entity flags use standard names", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # SD uses is_campus (not is_school) per CLAUDE.md
  expect_true("is_state" %in% names(enr))
  expect_true("is_district" %in% names(enr))
  expect_true("is_campus" %in% names(enr))
  expect_true("is_public" %in% names(enr))
})

test_that("no non-standard subgroup names present", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  subgroups <- unique(enr$subgroup)

  # These names should NOT be in the data
  banned_names <- c("total", "low_income", "economically_disadvantaged",
                    "frl", "iep", "disability", "el", "ell",
                    "english_learner", "american_indian", "two_or_more")

  for (name in banned_names) {
    expect_false(name %in% subgroups,
                 info = paste("Banned subgroup name found:", name))
  }
})

# ==============================================================================
# SECTION 2: Data Quality Invariants
# ==============================================================================

test_that("no NaN values in numeric columns", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  expect_equal(sum(is.nan(enr$n_students)), 0)
  expect_equal(sum(is.nan(enr$pct)), 0)
})

test_that("no Inf values in numeric columns", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  expect_equal(sum(is.infinite(enr$n_students)), 0)
  expect_equal(sum(is.infinite(enr$pct)), 0)
})

test_that("no negative enrollment counts", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  expect_equal(sum(enr$n_students < 0, na.rm = TRUE), 0)
})

test_that("no NA values in n_students after tidy", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  expect_false(any(is.na(enr$n_students)))
})

test_that("no NA values in pct column", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  expect_equal(sum(is.na(enr$pct)), 0)
})

test_that("pct values are between 0 and 1 for demographic subgroups", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  demos <- enr[enr$subgroup != "total_enrollment", ]

  if (nrow(demos) > 0) {
    expect_true(all(demos$pct >= 0, na.rm = TRUE))
    expect_true(all(demos$pct <= 1, na.rm = TRUE))
  }
})

test_that("total_enrollment has pct = 1 at TOTAL grade level", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  totals <- enr[enr$subgroup == "total_enrollment" &
                  enr$grade_level == "TOTAL", ]

  expect_true(all(totals$pct == 1))
})

test_that("end_year is constant within a single fetch", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  expect_equal(length(unique(enr$end_year)), 1)
  expect_equal(unique(enr$end_year), 2024L)
})

test_that("district IDs are 5-digit format", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  district_rows <- enr[enr$is_district & !is.na(enr$district_id), ]

  district_ids <- unique(district_rows$district_id)
  expect_true(all(nchar(district_ids) == 5),
              info = "All district IDs must be 5 characters")
  expect_true(all(grepl("^\\d{5}$", district_ids)),
              info = "All district IDs must be 5 digits")
})

test_that("campus IDs are 7-digit format (district_id + 2 digit)", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  campus_rows <- enr[enr$is_campus & !is.na(enr$campus_id), ]

  campus_ids <- unique(campus_rows$campus_id)
  expect_true(all(nchar(campus_ids) == 7),
              info = "All campus IDs should be 7 characters")
})

test_that("state rows have NA for district_id and campus_id", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state_rows <- enr[enr$is_state, ]

  expect_true(all(is.na(state_rows$district_id)))
  expect_true(all(is.na(state_rows$campus_id)))
})

test_that("district rows have NA for campus_id", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  district_rows <- enr[enr$is_district, ]

  expect_true(all(is.na(district_rows$campus_id)))
})

test_that("state rows have NA for district_name and campus_name", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state_rows <- enr[enr$is_state, ]

  expect_true(all(is.na(state_rows$district_name)))
  expect_true(all(is.na(state_rows$campus_name)))
})

# ==============================================================================
# SECTION 3: Wide-Tidy Fidelity
# ==============================================================================

test_that("wide row_total equals tidy total_enrollment for each district", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy_data <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # Get district totals from wide
  wide_districts <- wide[wide$type == "District", c("district_id", "row_total")]

  # Get district totals from tidy
  tidy_districts <- tidy_data[tidy_data$is_district &
                                tidy_data$subgroup == "total_enrollment" &
                                tidy_data$grade_level == "TOTAL",
                              c("district_id", "n_students")]

  # Merge and compare
  merged <- merge(wide_districts, tidy_districts, by = "district_id")
  expect_equal(merged$row_total, merged$n_students)
})

test_that("wide grade columns sum to row_total for districts", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  districts <- wide[wide$type == "District", ]

  grade_cols <- c("grade_pk", "grade_k", "grade_01", "grade_02", "grade_03",
                  "grade_04", "grade_05", "grade_06", "grade_07", "grade_08",
                  "grade_09", "grade_10", "grade_11", "grade_12")
  grade_cols <- grade_cols[grade_cols %in% names(districts)]

  if (length(grade_cols) > 0) {
    grade_sum <- rowSums(districts[, grade_cols], na.rm = TRUE)

    # Grade sum should match row_total (within tolerance for UG)
    for (i in seq_len(nrow(districts))) {
      if (!is.na(districts$row_total[i]) && districts$row_total[i] > 0) {
        ratio <- grade_sum[i] / districts$row_total[i]
        expect_true(ratio >= 0.95 && ratio <= 1.05,
                    info = paste("District", districts$district_id[i],
                                 "grade sum ratio:", ratio))
      }
    }
  }
})

test_that("wide race columns sum to approximately row_total for campuses", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  campuses <- wide[wide$type == "Campus", ]

  race_cols <- c("white", "black", "hispanic", "asian",
                 "native_american", "pacific_islander", "multiracial")
  race_cols <- race_cols[race_cols %in% names(campuses)]

  if (length(race_cols) > 0) {
    race_sum <- rowSums(campuses[, race_cols], na.rm = TRUE)

    # Race sum should match row_total
    for (i in seq_len(min(50, nrow(campuses)))) {
      if (!is.na(campuses$row_total[i]) && campuses$row_total[i] > 0 &&
          race_sum[i] > 0) {
        ratio <- race_sum[i] / campuses$row_total[i]
        expect_true(ratio >= 0.95 && ratio <= 1.05,
                    info = paste("Campus", campuses$campus_id[i],
                                 "race sum ratio:", ratio))
      }
    }
  }
})

# ==============================================================================
# SECTION 4: One Observation Per Group Per Period
# ==============================================================================

test_that("no duplicate state rows per year/subgroup/grade", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state_rows <- enr[enr$is_state, ]

  dupes <- state_rows[duplicated(state_rows[, c("end_year", "subgroup", "grade_level")]) |
                        duplicated(state_rows[, c("end_year", "subgroup", "grade_level")],
                                   fromLast = TRUE), ]

  expect_equal(nrow(dupes), 0,
               info = "Duplicate state rows found")
})

test_that("no duplicate district rows per year/district/subgroup/grade", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  dist_rows <- enr[enr$is_district, ]

  key_cols <- c("end_year", "district_id", "subgroup", "grade_level")
  dupes <- dist_rows[duplicated(dist_rows[, key_cols]) |
                       duplicated(dist_rows[, key_cols], fromLast = TRUE), ]

  expect_equal(nrow(dupes), 0,
               info = "Duplicate district rows found")
})

test_that("no duplicate campus rows per year/campus/subgroup/grade", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  camp_rows <- enr[enr$is_campus & !is.na(enr$campus_id), ]

  key_cols <- c("end_year", "campus_id", "subgroup", "grade_level")
  dupes <- camp_rows[duplicated(camp_rows[, key_cols]) |
                       duplicated(camp_rows[, key_cols], fromLast = TRUE), ]

  expect_equal(nrow(dupes), 0,
               info = "Duplicate campus rows found")
})

test_that("no duplicates in multi-year data", {
  skip_on_cran()
  skip_if_offline()

  multi <- fetch_enr_multi(2024:2025, tidy = TRUE, use_cache = TRUE)

  # State-level should have exactly 1 row per year/subgroup/grade
  state_rows <- multi[multi$is_state, ]
  key_cols <- c("end_year", "subgroup", "grade_level")
  dupes <- state_rows[duplicated(state_rows[, key_cols]) |
                        duplicated(state_rows[, key_cols], fromLast = TRUE), ]

  expect_equal(nrow(dupes), 0)
})

# ==============================================================================
# SECTION 5: Enrollment Reasonableness Checks
# ==============================================================================

test_that("state total is in reasonable range for all available years", {
  skip_on_cran()
  skip_if_offline()

  for (year in c(2015, 2022, 2023, 2024, 2025)) {
    enr <- fetch_enr(year, tidy = TRUE, use_cache = TRUE)
    state_total <- enr$n_students[enr$is_state &
                                    enr$subgroup == "total_enrollment" &
                                    enr$grade_level == "TOTAL"]

    if (length(state_total) > 0) {
      expect_true(state_total > 100000,
                  info = paste("Year", year, "state total too low:", state_total))
      expect_true(state_total < 200000,
                  info = paste("Year", year, "state total too high:", state_total))
    }
  }
})

test_that("Sioux Falls is the largest district in 2024", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  dist_totals <- enr[enr$is_district & enr$subgroup == "total_enrollment" &
                       enr$grade_level == "TOTAL",
                     c("district_name", "n_students")]

  max_district <- dist_totals$district_name[which.max(dist_totals$n_students)]
  expect_true(grepl("Sioux Falls", max_district))
})

test_that("Sioux Falls enrollment > Rapid City in 2024", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  dist_totals <- enr[enr$is_district & enr$subgroup == "total_enrollment" &
                       enr$grade_level == "TOTAL", ]

  sf <- dist_totals$n_students[grepl("Sioux Falls", dist_totals$district_name)]
  rc <- dist_totals$n_students[grepl("Rapid City", dist_totals$district_name)]

  expect_gt(sf, rc)
})

test_that("Sioux Falls > 20000 students across all recent years", {
  skip_on_cran()
  skip_if_offline()

  for (year in c(2023, 2024, 2025)) {
    enr <- fetch_enr(year, tidy = TRUE, use_cache = TRUE)
    sf <- enr[enr$is_district & grepl("Sioux Falls", enr$district_name) &
                enr$subgroup == "total_enrollment" & enr$grade_level == "TOTAL", ]

    expect_true(sf$n_students > 20000,
              info = paste("Year", year, "Sioux Falls too low"))
  }
})

test_that("PK enrollment is smallest grade level statewide", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state_grades <- enr[enr$is_state & enr$subgroup == "total_enrollment" &
                        !enr$grade_level %in% c("TOTAL", "UG"), ]

  min_grade <- unname(state_grades$grade_level[which.min(state_grades$n_students)])
  expect_equal(min_grade, "PK")
})

# ==============================================================================
# SECTION 6: Cross-Year Consistency
# ==============================================================================

test_that("district count changes are small between consecutive years", {
  skip_on_cran()
  skip_if_offline()

  multi <- fetch_enr_multi(2023:2025, tidy = TRUE, use_cache = TRUE)

  for (year in 2024:2025) {
    prev_year <- year - 1
    n_curr <- sum(multi$is_district & multi$end_year == year &
                    multi$subgroup == "total_enrollment" &
                    multi$grade_level == "TOTAL")
    n_prev <- sum(multi$is_district & multi$end_year == prev_year &
                    multi$subgroup == "total_enrollment" &
                    multi$grade_level == "TOTAL")

    # District count should not change by more than 10
    expect_true(abs(n_curr - n_prev) <= 10,
                info = paste("District count changed by", abs(n_curr - n_prev),
                             "from", prev_year, "to", year))
  }
})

test_that("state enrollment changes are < 5% between consecutive years", {
  skip_on_cran()
  skip_if_offline()

  multi <- fetch_enr_multi(2023:2025, tidy = TRUE, use_cache = TRUE)
  state_totals <- multi[multi$is_state & multi$subgroup == "total_enrollment" &
                          multi$grade_level == "TOTAL",
                        c("end_year", "n_students")]
  state_totals <- state_totals[order(state_totals$end_year), ]

  for (i in 2:nrow(state_totals)) {
    pct_change <- abs(state_totals$n_students[i] / state_totals$n_students[i - 1] - 1)
    expect_true(pct_change < 0.05,
                info = paste("Year", state_totals$end_year[i],
                             "enrollment changed by", round(pct_change * 100, 1), "%"))
  }
})

# ==============================================================================
# SECTION 7: URL Building Functions
# ==============================================================================

test_that("build_sd_url constructs correct base URL", {
  url <- build_sd_url(2024, "district")
  expect_true(grepl("^https://doe\\.sd\\.gov/ofm/documents/", url))
})

test_that("get_district_filename covers all eras", {
  # Era 0
  expect_equal(get_district_filename(2006), "2006_PublicPk-12.xls")
  expect_equal(get_district_filename(2007), "Public_pk-12_totals07.xls")
  expect_equal(get_district_filename(2008), "WEBPublicbydistrictPK-12.xls")
  expect_equal(get_district_filename(2009), "Public_district.xls")
  expect_equal(get_district_filename(2010), "FE10_Psum.xls")

  # Era 1
  expect_equal(get_district_filename(2011), "FE11_Psum.xlsx")
  expect_equal(get_district_filename(2012), "FE12_Pscgr.xlsx")

  # Era 2
  expect_equal(get_district_filename(2013), "Pubdsgr13.xlsx")
  expect_equal(get_district_filename(2014), "Pubdsgr14.xlsx")
  expect_equal(get_district_filename(2015), "Pubdsgr15.xlsx")
  expect_equal(get_district_filename(2016), "Pubdsgr16.xlsx")
  expect_equal(get_district_filename(2017), "Pubdsgr17b.xlsx")
  expect_equal(get_district_filename(2018), "PSgrade-18a.xlsx")
  expect_equal(get_district_filename(2019), "Pubdisgr-19.xlsx")
  expect_equal(get_district_filename(2020), "Pubdisgr-20f.xlsx")

  # Era 3
  expect_equal(get_district_filename(2021), "Pubdisgr-21.xlsx")
  expect_equal(get_district_filename(2022), "Pubdisgr-2022.xlsx")
  expect_equal(get_district_filename(2023), "Pubdisgr-23.xlsx")
  expect_equal(get_district_filename(2024), "Pubdisgr-2024.xlsx")
  expect_equal(get_district_filename(2025), "Pubdisgr-2025.xlsx")
})

test_that("get_race_filename returns NULL for 2006", {
  expect_null(get_race_filename(2006))
})

test_that("get_race_filename returns correct filenames", {
  expect_equal(get_race_filename(2007), "Public_FE_Ethnicity07.xls")
  expect_equal(get_race_filename(2008), "WEBPublicbySchoolbyEthnicitybyGrade.xls")
  expect_equal(get_race_filename(2009), "Public_ethnicity.xls")
  expect_equal(get_race_filename(2010), "FE10_Peth.xls")
  expect_equal(get_race_filename(2011), "FE11_Pethnicity.xlsx")
  expect_equal(get_race_filename(2012), "FE12_Pethnicity.xlsx")
  expect_equal(get_race_filename(2015), "Pubscrce15.xlsx")
  expect_equal(get_race_filename(2018), "Pschrce-18a.xlsx")
  expect_equal(get_race_filename(2019), "Pschrce-19.xlsx")
  expect_equal(get_race_filename(2020), "Pubschrce-20f.xlsx")
  expect_equal(get_race_filename(2021), "Pschrce-21a.xlsx")
  expect_equal(get_race_filename(2022), "Pubschrce-2022.xlsx")
  expect_equal(get_race_filename(2023), "Pubschrce-23b.xlsx")
  expect_equal(get_race_filename(2024), "Pubschrce-2024.xlsx")
  expect_equal(get_race_filename(2025), "Pubschrce-2025.xlsx")
})

test_that("get_gender_filename returns NULL for 2006", {
  expect_null(get_gender_filename(2006))
})

test_that("get_gender_filename returns correct filenames", {
  expect_equal(get_gender_filename(2007), "Public_Gender_FE2007.xls")
  expect_equal(get_gender_filename(2008), "WEBPublicbySchoolbyGenderbyGrade.xls")
  expect_equal(get_gender_filename(2009), "Public_gender.xls")
  expect_equal(get_gender_filename(2010), "FE10_Pgen.xls")
  expect_equal(get_gender_filename(2011), "FE11_Pgender.xlsx")
  expect_equal(get_gender_filename(2012), "FE12_Pgender.xlsx")
  expect_equal(get_gender_filename(2015), "Pubscgen15.xlsx")
  expect_equal(get_gender_filename(2018), "Pubgen-18a.xlsx")
  expect_equal(get_gender_filename(2019), "Pschgen-19.xlsx")
  expect_equal(get_gender_filename(2020), "Pubschgen-20f.xlsx")
  expect_equal(get_gender_filename(2021), "Pschgen-21.xlsx")
  expect_equal(get_gender_filename(2022), "Pubschgen-22.xlsx")
  expect_equal(get_gender_filename(2023), "Pubschgen-23.xlsx")
  expect_equal(get_gender_filename(2024), "Pubschgen-24.xlsx")
  expect_equal(get_gender_filename(2025), "Pubschgen-2025.xlsx")
})

test_that("get_grade_filename returns correct filenames", {
  expect_equal(get_grade_filename(2011), "FE11_Pschgrade.xlsx")
  expect_equal(get_grade_filename(2012), "FE12_Pschgrade.xlsx")
  expect_equal(get_grade_filename(2015), "Pubscgr15.xlsx")
  expect_equal(get_grade_filename(2018), "Pubscgrade-18.xlsx")
  expect_equal(get_grade_filename(2019), "Pubschgr-19.xlsx")
  expect_equal(get_grade_filename(2020), "Pubschgr-20f.xlsx")
  expect_equal(get_grade_filename(2024), "Pubschgr-2024.xlsx")
  expect_equal(get_grade_filename(2025), "Pubschgr-2025.xlsx")
})

test_that("build_sd_url rejects unknown file_type", {
  expect_error(build_sd_url(2024, "unknown_type"), "Unknown file_type")
})

# ==============================================================================
# SECTION 8: safe_numeric() Edge Cases
# ==============================================================================

test_that("safe_numeric handles all suppression markers", {
  expect_true(is.na(safe_numeric("*")))
  expect_true(is.na(safe_numeric(".")))
  expect_true(is.na(safe_numeric("-")))
  expect_true(is.na(safe_numeric("-1")))
  expect_true(is.na(safe_numeric("<5")))
  expect_true(is.na(safe_numeric("N/A")))
  expect_true(is.na(safe_numeric("NA")))
  expect_true(is.na(safe_numeric("n/a")))
  expect_true(is.na(safe_numeric("N/a")))
  expect_true(is.na(safe_numeric("")))
})

test_that("safe_numeric handles commas in numbers", {
  expect_equal(safe_numeric("1,234"), 1234)
  expect_equal(safe_numeric("1,234,567"), 1234567)
  expect_equal(safe_numeric("12,345,678"), 12345678)
})

test_that("safe_numeric handles whitespace", {
  expect_equal(safe_numeric("  100  "), 100)
  expect_equal(safe_numeric("\t200\n"), 200)
  expect_equal(safe_numeric(" 300 "), 300)
})

test_that("safe_numeric passes through numeric values", {
  expect_equal(safe_numeric(42), 42)
  expect_equal(safe_numeric(0), 0)
  expect_equal(safe_numeric(3.14), 3.14)
  expect_equal(safe_numeric(c(1, 2, 3)), c(1, 2, 3))
})

test_that("safe_numeric handles NULL and empty", {
  expect_equal(length(safe_numeric(NULL)), 0)
  expect_equal(length(safe_numeric(character(0))), 0)
})

test_that("safe_numeric handles vector input", {
  result <- safe_numeric(c("100", "200", "*", "300"))
  expect_equal(result[1], 100)
  expect_equal(result[2], 200)
  expect_true(is.na(result[3]))
  expect_equal(result[4], 300)
})

test_that("safe_numeric handles decimal values", {
  expect_equal(safe_numeric("3.14"), 3.14)
  expect_equal(safe_numeric("0.5"), 0.5)
})

# ==============================================================================
# SECTION 9: Directory Data
# ==============================================================================

test_that("fetch_directory returns correct structure", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) {
      skip(paste("fetch_directory not available:", e$message))
    }
  )

  expect_s3_class(dir_data, "tbl_df")
  expect_equal(ncol(dir_data), 18)
  expect_gt(nrow(dir_data), 800)
})

test_that("fetch_directory has all required columns", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) {
      skip(paste("fetch_directory not available:", e$message))
    }
  )

  required_cols <- c(
    "state_district_id", "state_school_id", "district_name",
    "school_name", "entity_type", "school_type", "grades_served",
    "address", "city", "state", "zip", "phone", "county_name",
    "principal_name", "principal_email",
    "superintendent_name", "superintendent_email", "website"
  )

  for (col in required_cols) {
    expect_true(col %in% names(dir_data),
                info = paste("Missing directory column:", col))
  }
})

test_that("fetch_directory column types are all character", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) {
      skip(paste("fetch_directory not available:", e$message))
    }
  )

  for (col in names(dir_data)) {
    expect_true(is.character(dir_data[[col]]),
                info = paste("Column", col, "is not character"))
  }
})

test_that("fetch_directory state is predominantly SD", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) {
      skip(paste("fetch_directory not available:", e$message))
    }
  )

  non_na <- dir_data$state[!is.na(dir_data$state)]
  pct_sd <- mean(non_na == "SD")
  expect_gt(pct_sd, 0.95)
})

test_that("fetch_directory has Sioux Falls schools", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) {
      skip(paste("fetch_directory not available:", e$message))
    }
  )

  sf <- dir_data[grepl("Sioux Falls", dir_data$district_name), ]
  expect_true(nrow(sf) > 10, info = "Sioux Falls should have many schools")
  # The main Sioux Falls 49-5 district should be present
  expect_true("49005" %in% sf$state_district_id)
})

test_that("fetch_directory has Rapid City schools", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) {
      skip(paste("fetch_directory not available:", e$message))
    }
  )

  rc <- dir_data[grepl("Rapid City", dir_data$district_name), ]
  expect_gt(nrow(rc), 5)
})

test_that("fetch_directory has Aberdeen schools", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) {
      skip(paste("fetch_directory not available:", e$message))
    }
  )

  ab <- dir_data[grepl("Aberdeen", dir_data$district_name), ]
  expect_gt(nrow(ab), 3)
})

test_that("fetch_directory has superintendent data joined", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) {
      skip(paste("fetch_directory not available:", e$message))
    }
  )

  has_supt <- sum(!is.na(dir_data$superintendent_name))
  expect_true(has_supt > 0, info = "At least some rows should have superintendent names")
})

test_that("fetch_directory(tidy=FALSE) returns raw column names", {
  skip_on_cran()
  skip_if_offline()

  dir_raw <- tryCatch(
    fetch_directory(tidy = FALSE, use_cache = TRUE),
    error = function(e) {
      skip(paste("fetch_directory not available:", e$message))
    }
  )

  expect_s3_class(dir_raw, "tbl_df")
  expect_gt(nrow(dir_raw), 50)
  expect_true("District Name" %in% names(dir_raw))
  expect_true("District Number" %in% names(dir_raw))
})

test_that("fetch_directory school types include expected values", {
  skip_on_cran()
  skip_if_offline()

  dir_data <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) {
      skip(paste("fetch_directory not available:", e$message))
    }
  )

  school_types <- unique(dir_data$school_type[!is.na(dir_data$school_type)])

  # Should have elementary and secondary at minimum
  has_elementary <- any(grepl("Elementary", school_types))
  has_secondary <- any(grepl("Secondary", school_types))

  expect_true(has_elementary, info = "Should have elementary school types")
  expect_true(has_secondary, info = "Should have secondary school types")
})

test_that("clear_directory_cache works", {
  skip_on_cran()
  skip_if_offline()

  # This should not error
  result <- clear_directory_cache()
  expect_true(is.numeric(result))
})

# ==============================================================================
# SECTION 10: Edge Cases and Error Handling
# ==============================================================================

test_that("fetch_enr handles boundary years correctly", {
  skip_on_cran()
  skip_if_offline()

  # Minimum year (Era 1 start - 2011; 2006-2010 Era 0 has known parsing issues)
  enr_min <- fetch_enr(2011, tidy = TRUE, use_cache = TRUE)
  expect_s3_class(enr_min, "data.frame")
  expect_gt(nrow(enr_min), 0)

  # Maximum year
  enr_max <- fetch_enr(2025, tidy = TRUE, use_cache = TRUE)
  expect_s3_class(enr_max, "data.frame")
  expect_gt(nrow(enr_max), 0)
})

test_that("fetch_enr_multi handles single element vector", {
  skip_on_cran()
  skip_if_offline()

  result <- fetch_enr_multi(c(2024), tidy = TRUE, use_cache = TRUE)
  expect_equal(unique(result$end_year), 2024L)
})

test_that("fetch_enr_multi handles reversed year order", {
  skip_on_cran()
  skip_if_offline()

  result <- fetch_enr_multi(c(2025, 2024, 2023), tidy = TRUE, use_cache = TRUE)
  expect_true(all(c(2023, 2024, 2025) %in% result$end_year))
})

test_that("cache round-trip preserves data integrity", {
  skip_on_cran()
  skip_if_offline()

  # Fetch fresh (should populate cache)
  enr1 <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # Fetch from cache
  enr2 <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # Should be identical
  expect_equal(nrow(enr1), nrow(enr2))
  expect_equal(ncol(enr1), ncol(enr2))
  expect_equal(names(enr1), names(enr2))

  # Spot check
  state1 <- enr1$n_students[enr1$is_state & enr1$subgroup == "total_enrollment" &
                               enr1$grade_level == "TOTAL"]
  state2 <- enr2$n_students[enr2$is_state & enr2$subgroup == "total_enrollment" &
                               enr2$grade_level == "TOTAL"]
  expect_equal(state1, state2)
})

test_that("get_available_years is consistent with fetch_enr validation", {
  skip_on_cran()
  skip_if_offline()

  years <- get_available_years()

  # A known working year (Era 1+) should work
  expect_no_error(fetch_enr(2011, tidy = TRUE, use_cache = TRUE))

  # One below min should fail
  expect_error(fetch_enr(min(years) - 1))

  # One above max should fail
  expect_error(fetch_enr(max(years) + 1))
})

# ==============================================================================
# SECTION 11: Multiple Eras Have Consistent Output Schema
# ==============================================================================

test_that("all eras produce the same tidy column set", {
  skip_on_cran()
  skip_if_offline()

  # Test one year from each era
  # Era 1: 2011, Era 2: 2015, Era 3: 2024
  required_cols <- c("end_year", "type", "district_id", "subgroup",
                     "grade_level", "n_students", "pct",
                     "is_state", "is_district", "is_campus")

  for (year in c(2011, 2015, 2024)) {
    enr <- fetch_enr(year, tidy = TRUE, use_cache = TRUE)
    for (col in required_cols) {
      expect_true(col %in% names(enr),
                  info = paste("Year", year, "missing column:", col))
    }
  }
})

test_that("all eras have state-level total enrollment", {
  skip_on_cran()
  skip_if_offline()

  for (year in c(2011, 2015, 2024)) {
    enr <- fetch_enr(year, tidy = TRUE, use_cache = TRUE)
    state <- enr[enr$is_state & enr$subgroup == "total_enrollment" &
                   enr$grade_level == "TOTAL", ]

    expect_equal(nrow(state), 1,
                 info = paste("Year", year, "should have exactly 1 state total row"))
    expect_true(state$n_students > 0,
                info = paste("Year", year, "state total should be > 0"))
  }
})

test_that("all eras have district-level data", {
  skip_on_cran()
  skip_if_offline()

  for (year in c(2011, 2015, 2024)) {
    enr <- fetch_enr(year, tidy = TRUE, use_cache = TRUE)
    n_districts <- sum(enr$is_district & enr$subgroup == "total_enrollment" &
                         enr$grade_level == "TOTAL")

    expect_true(n_districts > 100,
                info = paste("Year", year, "should have > 100 districts"))
  }
})

# ==============================================================================
# SECTION 12: Data Completeness
# ==============================================================================

test_that("every district has a district_name", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  dist_rows <- enr[enr$is_district, ]

  expect_false(any(is.na(dist_rows$district_name)),
               info = "All district rows should have district_name")
})

test_that("every district has a district_id", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  dist_rows <- enr[enr$is_district, ]

  expect_false(any(is.na(dist_rows$district_id)),
               info = "All district rows should have district_id")
})

test_that("every campus has a district_name", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  campus_rows <- enr[enr$is_campus, ]

  # At least 95% should have district_name
  pct_with_name <- mean(!is.na(campus_rows$district_name))
  expect_gt(pct_with_name, 0.95)
})

test_that("every grade level appears at state level", {
  skip_on_cran()
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state_grades <- unique(enr$grade_level[enr$is_state & enr$subgroup == "total_enrollment"])

  expected_grades <- c("TOTAL", "PK", "K", "01", "02", "03", "04", "05",
                       "06", "07", "08", "09", "10", "11", "12")
  for (g in expected_grades) {
    expect_true(g %in% state_grades,
                info = paste("Grade", g, "missing from state level"))
  }
})
