# ==============================================================================
# Transformation Correctness Tests for sdschooldata
# ==============================================================================
#
# These tests verify that every transformation step (process, tidy, aggregate)
# preserves data fidelity. Tests use cached real data from SD DOE to pin
# against known values and catch regressions.
#
# Transformation chain:
#   get_raw_enr(year) -> process_enr(raw, year) -> tidy_enr(wide) -> id_enr_aggs()
#
# Key transformations tested:
#   1. safe_numeric: suppression markers, commas, whitespace
#   2. process_district_enr: column mapping, district_id formatting, grade parsing
#   3. process_campus_enr: race pivot, grade aggregation across races
#   4. create_state_aggregate: district sums = state row
#   5. tidy_enr: wide-to-long pivot preserving counts exactly
#   6. id_enr_aggs: boolean flags from type column, is_public from type_code
#   7. enr_grade_aggs: K8/HS/K12 groupings
#   8. Fidelity: wide counts == tidy counts for every entity
#
# ==============================================================================


# ==============================================================================
# 1. safe_numeric transformation correctness
# ==============================================================================

test_that("safe_numeric preserves exact integer values through string conversion", {
  # SD DOE reads all columns as text (col_types = "text"), so every number

  # passes through safe_numeric. Verify no floating point drift.
  expect_identical(safe_numeric("140587"), 140587)
  expect_identical(safe_numeric("25060"), 25060)
  expect_identical(safe_numeric("0"), 0)
})

test_that("safe_numeric handles comma-separated thousands correctly", {
  expect_equal(safe_numeric("1,234"), 1234)
  expect_equal(safe_numeric("25,060"), 25060)
  expect_equal(safe_numeric("140,587"), 140587)
  expect_equal(safe_numeric("1,234,567"), 1234567)
})

test_that("safe_numeric maps all SD DOE suppression markers to NA", {
  # SD DOE uses *, ., -, -1, <5, N/A, and empty strings for suppression
  markers <- c("*", ".", "-", "-1", "<5", "N/A", "NA", "", "n/a", "N/a")
  for (m in markers) {
    expect_true(is.na(safe_numeric(m)),
                info = paste("Marker not mapped to NA:", repr(m)))
  }
})

test_that("safe_numeric returns NA (not error) for unexpected text", {
  # Excel files sometimes have header text leaking into data rows
  expect_true(is.na(safe_numeric("District Name")))
  expect_true(is.na(safe_numeric("Total PK-12")))
  expect_true(is.na(safe_numeric("KG")))
})

test_that("safe_numeric preserves vector ordering and length", {
  input <- c("100", "*", "200", "", "300")
  result <- safe_numeric(input)
  expect_equal(length(result), 5)
  expect_equal(result[1], 100)
  expect_true(is.na(result[2]))
  expect_equal(result[3], 200)
  expect_true(is.na(result[4]))
  expect_equal(result[5], 300)
})


# ==============================================================================
# 2. process_district_enr: district_id formatting
# ==============================================================================

test_that("district_id is zero-padded to 5 digits", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  districts <- wide[wide$type == "District", ]

  # All district IDs should be exactly 5 characters

  expect_true(all(nchar(districts$district_id) == 5))

  # Known district IDs
  expect_true("49005" %in% districts$district_id)  # Sioux Falls
  expect_true("51004" %in% districts$district_id)  # Rapid City
})

test_that("district_name is trimmed of whitespace", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  districts <- wide[wide$type == "District", ]

  # No leading or trailing whitespace
  expect_true(all(districts$district_name == trimws(districts$district_name)))
})


# ==============================================================================
# 3. State aggregate is exact sum of districts
# ==============================================================================

test_that("state row_total equals exact sum of district row_totals", {
  skip_on_cran()
  skip_if_offline()

  for (yr in c(2024, 2025)) {
    wide <- fetch_enr(yr, tidy = FALSE, use_cache = TRUE)

    state <- wide[wide$type == "State", ]
    districts <- wide[wide$type == "District", ]

    expect_equal(
      state$row_total,
      sum(districts$row_total, na.rm = TRUE),
      info = paste("State total != district sum for year", yr)
    )
  }
})

test_that("state grade columns equal exact sum of district grade columns", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  state <- wide[wide$type == "State", ]
  districts <- wide[wide$type == "District", ]

  grade_cols <- grep("^grade_", names(state), value = TRUE)
  for (gc in grade_cols) {
    expect_equal(
      state[[gc]],
      sum(districts[[gc]], na.rm = TRUE),
      info = paste("State", gc, "!= district sum")
    )
  }
})


# ==============================================================================
# 4. Wide-to-tidy: total_enrollment fidelity
# ==============================================================================

test_that("tidy total_enrollment TOTAL matches wide row_total for every entity", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # For each entity in wide, check that tidy total_enrollment TOTAL matches
  for (entity_type in c("State", "District")) {
    wide_subset <- wide[wide$type == entity_type, ]

    for (i in seq_len(nrow(wide_subset))) {
      row <- wide_subset[i, ]

      if (entity_type == "State") {
        tidy_match <- tidy[tidy$is_state &
                           tidy$subgroup == "total_enrollment" &
                           tidy$grade_level == "TOTAL", ]
      } else {
        tidy_match <- tidy[tidy$is_district &
                           tidy$district_id == row$district_id &
                           tidy$subgroup == "total_enrollment" &
                           tidy$grade_level == "TOTAL", ]
      }

      expect_equal(
        nrow(tidy_match), 1,
        info = paste(entity_type, row$district_id, "should have exactly 1 tidy TOTAL row")
      )
      expect_equal(
        tidy_match$n_students, row$row_total,
        info = paste(entity_type, row$district_id, "tidy n_students != wide row_total")
      )
    }
  }
})

test_that("tidy grade-level counts match wide grade columns for districts", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # Check a few specific districts
  test_districts <- c("49005", "51004", "06001")  # Sioux Falls, Rapid City, Aberdeen
  grade_map <- c(
    "grade_pk" = "PK", "grade_k" = "K",
    "grade_01" = "01", "grade_02" = "02", "grade_03" = "03",
    "grade_04" = "04", "grade_05" = "05", "grade_06" = "06",
    "grade_07" = "07", "grade_08" = "08", "grade_09" = "09",
    "grade_10" = "10", "grade_11" = "11", "grade_12" = "12"
  )

  for (did in test_districts) {
    wide_row <- wide[wide$type == "District" & wide$district_id == did, ]
    if (nrow(wide_row) == 0) next

    for (wide_col in names(grade_map)) {
      if (!wide_col %in% names(wide_row)) next

      tidy_grade <- grade_map[wide_col]
      tidy_row <- tidy[tidy$is_district &
                        tidy$district_id == did &
                        tidy$subgroup == "total_enrollment" &
                        tidy$grade_level == tidy_grade, ]

      wide_val <- wide_row[[wide_col]]

      if (!is.na(wide_val) && wide_val > 0) {
        expect_equal(
          nrow(tidy_row), 1,
          info = paste("District", did, tidy_grade, "missing from tidy")
        )
        expect_equal(
          tidy_row$n_students, wide_val,
          info = paste("District", did, tidy_grade, "count mismatch")
        )
      }
    }
  }
})


# ==============================================================================
# 5. Wide-to-tidy: race subgroup fidelity (campus level)
# ==============================================================================

test_that("tidy race counts match wide race columns for campuses", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  campus_wide <- wide[wide$type == "Campus", ]
  race_cols <- c("white", "black", "hispanic", "asian",
                 "native_american", "pacific_islander", "multiracial")

  # Sample 10 campuses for spot-check
  set.seed(NULL)  # Not deterministic but that's fine for spot checks
  sample_idx <- seq_len(min(10, nrow(campus_wide)))

  for (i in sample_idx) {
    row <- campus_wide[i, ]
    cid <- row$campus_id

    for (rc in race_cols) {
      if (!rc %in% names(row)) next
      wide_val <- row[[rc]]
      if (is.na(wide_val)) next

      tidy_row <- tidy[tidy$is_campus &
                        tidy$campus_id == cid &
                        tidy$subgroup == rc &
                        tidy$grade_level == "TOTAL", ]

      if (wide_val > 0) {
        expect_equal(
          nrow(tidy_row), 1,
          info = paste("Campus", cid, rc, "missing from tidy")
        )
        expect_equal(
          tidy_row$n_students, wide_val,
          info = paste("Campus", cid, rc, "count mismatch: wide=",
                       wide_val, "tidy=", tidy_row$n_students)
        )
      }
    }
  }
})


# ==============================================================================
# 6. Race counts sum to total enrollment at campus level
# ==============================================================================

test_that("campus race subgroups sum to total enrollment", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  race_subgroups <- c("white", "black", "hispanic", "asian",
                      "native_american", "pacific_islander", "multiracial")

  campus_totals <- tidy |>
    dplyr::filter(is_campus, subgroup == "total_enrollment", grade_level == "TOTAL") |>
    dplyr::select(campus_id, total = n_students)

  campus_race_sums <- tidy |>
    dplyr::filter(is_campus, subgroup %in% race_subgroups, grade_level == "TOTAL") |>
    dplyr::group_by(campus_id) |>
    dplyr::summarize(race_sum = sum(n_students, na.rm = TRUE), .groups = "drop")

  merged <- dplyr::inner_join(campus_totals, campus_race_sums, by = "campus_id")

  # For campuses that have race data, race sum should equal total
  if (nrow(merged) > 0) {
    mismatches <- merged |> dplyr::filter(abs(race_sum - total) > 0)
    expect_equal(
      nrow(mismatches), 0,
      info = paste("Found", nrow(mismatches),
                   "campuses where race sum != total enrollment")
    )
  }
})


# ==============================================================================
# 7. Grade-level counts sum to TOTAL
# ==============================================================================

test_that("district grade-level counts sum to row_total in wide format", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  districts <- wide[wide$type == "District", ]

  grade_cols <- grep("^grade_", names(districts), value = TRUE)

  if (length(grade_cols) > 0) {
    grade_sums <- rowSums(districts[, grade_cols], na.rm = TRUE)

    # Allow for UG column possibly being missing (sum might be less than total
    # if UG students exist but column isn't present)
    if (!"grade_ug" %in% grade_cols) {
      # Without UG, grade sum should be <= row_total
      expect_true(
        all(grade_sums <= districts$row_total + 1, na.rm = TRUE),
        info = "Grade sums exceed row_total (without UG)"
      )
    } else {
      # With all grade columns including UG, sum should equal row_total
      expect_equal(grade_sums, districts$row_total)
    }
  }
})

test_that("tidy grade-level n_students sum to TOTAL for state", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  state_grades <- tidy |>
    dplyr::filter(is_state, subgroup == "total_enrollment", grade_level != "TOTAL")
  state_total <- tidy |>
    dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")

  expect_equal(
    sum(state_grades$n_students, na.rm = TRUE),
    state_total$n_students,
    info = "State grade-level sum != TOTAL"
  )
})


# ==============================================================================
# 8. pct calculation correctness
# ==============================================================================

test_that("pct for total_enrollment TOTAL is exactly 1.0", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  total_rows <- tidy |>
    dplyr::filter(subgroup == "total_enrollment", grade_level == "TOTAL")

  expect_true(
    all(total_rows$pct == 1.0, na.rm = TRUE),
    info = "total_enrollment TOTAL should always have pct = 1.0"
  )
})

test_that("pct = n_students / row_total for grade-level rows", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # Check Sioux Falls grade K
  sf_wide <- wide[wide$type == "District" & wide$district_id == "49005", ]
  sf_k <- tidy |>
    dplyr::filter(is_district, district_id == "49005",
                  subgroup == "total_enrollment", grade_level == "K")

  expected_pct <- sf_wide$grade_k / sf_wide$row_total
  expect_equal(sf_k$pct, expected_pct, tolerance = 1e-10)
})

test_that("pct = n_students / row_total for race subgroups", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # Check first campus with race data
  campus_wide <- wide[wide$type == "Campus" & !is.na(wide$white) & wide$white > 0, ]
  if (nrow(campus_wide) > 0) {
    first_campus <- campus_wide[1, ]
    cid <- first_campus$campus_id

    tidy_white <- tidy |>
      dplyr::filter(is_campus, campus_id == cid,
                    subgroup == "white", grade_level == "TOTAL")

    expected_pct <- first_campus$white / first_campus$row_total
    expect_equal(tidy_white$pct, expected_pct, tolerance = 1e-10)
  }
})


# ==============================================================================
# 9. id_enr_aggs: boolean flag correctness
# ==============================================================================

test_that("is_state is TRUE only for type == State", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  state_rows <- tidy[tidy$is_state, ]
  expect_true(all(state_rows$type == "State"))

  non_state_rows <- tidy[!tidy$is_state, ]
  expect_true(all(non_state_rows$type != "State"))
})

test_that("is_district is TRUE only for type == District", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  dist_rows <- tidy[tidy$is_district, ]
  expect_true(all(dist_rows$type == "District"))

  non_dist_rows <- tidy[!tidy$is_district, ]
  expect_true(all(non_dist_rows$type != "District"))
})

test_that("is_campus is TRUE only for type == Campus", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  campus_rows <- tidy[tidy$is_campus, ]
  expect_true(all(campus_rows$type == "Campus"))

  non_campus_rows <- tidy[!tidy$is_campus, ]
  expect_true(all(non_campus_rows$type != "Campus"))
})

test_that("is_state, is_district, is_campus are mutually exclusive", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # Each row should have exactly one TRUE among the three flags
  flag_sum <- as.integer(tidy$is_state) +
              as.integer(tidy$is_district) +
              as.integer(tidy$is_campus)

  expect_true(
    all(flag_sum == 1),
    info = paste("Found", sum(flag_sum != 1), "rows with multiple or no entity flags")
  )
})

test_that("is_public is derived correctly from district_type_code", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # District rows with type_code "10" should be public
  dist_rows <- tidy[tidy$is_district, ]
  code_10 <- dist_rows[!is.na(dist_rows$district_type_code) &
                        dist_rows$district_type_code == "10", ]
  if (nrow(code_10) > 0) {
    expect_true(all(code_10$is_public))
  }

  # Rows with NA type_code should default to public
  na_code <- tidy[is.na(tidy$district_type_code), ]
  if (nrow(na_code) > 0) {
    expect_true(all(na_code$is_public))
  }
})

test_that("aggregation_flag is consistent with entity flags", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  expect_true(all(tidy$aggregation_flag[tidy$is_state] == "state"))
  expect_true(all(tidy$aggregation_flag[tidy$is_district] == "district"))
  expect_true(all(tidy$aggregation_flag[tidy$is_campus] == "campus"))
})


# ==============================================================================
# 10. enr_grade_aggs: aggregate correctness
# ==============================================================================

test_that("K8 aggregate equals sum of K through 08", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(tidy)

  k8_grades <- c("K", "01", "02", "03", "04", "05", "06", "07", "08")

  # State level
  manual_k8 <- tidy |>
    dplyr::filter(is_state, subgroup == "total_enrollment",
                  grade_level %in% k8_grades) |>
    dplyr::summarize(total = sum(n_students, na.rm = TRUE)) |>
    dplyr::pull(total)

  agg_k8 <- aggs |>
    dplyr::filter(is_state, grade_level == "K8") |>
    dplyr::pull(n_students)

  expect_equal(agg_k8, manual_k8)
})

test_that("HS aggregate equals sum of 09 through 12", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(tidy)

  hs_grades <- c("09", "10", "11", "12")

  manual_hs <- tidy |>
    dplyr::filter(is_state, subgroup == "total_enrollment",
                  grade_level %in% hs_grades) |>
    dplyr::summarize(total = sum(n_students, na.rm = TRUE)) |>
    dplyr::pull(total)

  agg_hs <- aggs |>
    dplyr::filter(is_state, grade_level == "HS") |>
    dplyr::pull(n_students)

  expect_equal(agg_hs, manual_hs)
})

test_that("K12 aggregate equals K8 + HS (excludes PK and UG)", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(tidy)

  state_k8 <- aggs |>
    dplyr::filter(is_state, grade_level == "K8") |>
    dplyr::pull(n_students)
  state_hs <- aggs |>
    dplyr::filter(is_state, grade_level == "HS") |>
    dplyr::pull(n_students)
  state_k12 <- aggs |>
    dplyr::filter(is_state, grade_level == "K12") |>
    dplyr::pull(n_students)

  expect_equal(state_k12, state_k8 + state_hs)
})

test_that("grade aggregates work at district level too", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(tidy)

  # Sioux Falls K8
  sf_k8_manual <- tidy |>
    dplyr::filter(is_district, district_id == "49005",
                  subgroup == "total_enrollment",
                  grade_level %in% c("K", "01", "02", "03", "04",
                                     "05", "06", "07", "08")) |>
    dplyr::summarize(total = sum(n_students, na.rm = TRUE)) |>
    dplyr::pull(total)

  sf_k8_agg <- aggs |>
    dplyr::filter(is_district, district_id == "49005", grade_level == "K8") |>
    dplyr::pull(n_students)

  expect_equal(sf_k8_agg, sf_k8_manual)
})

test_that("enr_grade_aggs only uses total_enrollment subgroup", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(tidy)

  # All rows in aggregates should have subgroup == "total_enrollment"
  expect_true(all(aggs$subgroup == "total_enrollment"))
})


# ==============================================================================
# 11. Known value spot checks (pinned to real SD DOE data)
# ==============================================================================

test_that("2024 state total enrollment is 140587", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state_total <- tidy |>
    dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
    dplyr::pull(n_students)

  expect_equal(state_total, 140587)
})

test_that("2024 Sioux Falls enrollment is 25060", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  sf_total <- tidy |>
    dplyr::filter(is_district, district_id == "49005",
                  subgroup == "total_enrollment", grade_level == "TOTAL") |>
    dplyr::pull(n_students)

  expect_equal(sf_total, 25060)
})

test_that("2024 Rapid City enrollment is 12313", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  rc_total <- tidy |>
    dplyr::filter(is_district, district_id == "51004",
                  subgroup == "total_enrollment", grade_level == "TOTAL") |>
    dplyr::pull(n_students)

  expect_equal(rc_total, 12313)
})

test_that("2024 has 148 districts", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  districts <- wide[wide$type == "District", ]
  expect_equal(nrow(districts), 148)
})

test_that("2024 has 684 campuses", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  campuses <- wide[wide$type == "Campus", ]
  expect_equal(nrow(campuses), 684)
})

test_that("2025 state total enrollment is 138861", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2025, tidy = TRUE, use_cache = TRUE)
  state_total <- tidy |>
    dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
    dplyr::pull(n_students)

  expect_equal(state_total, 138861)
})


# ==============================================================================
# 12. No duplicate rows in tidy output
# ==============================================================================

test_that("no duplicate entity x subgroup x grade_level combinations", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  dupes <- tidy |>
    dplyr::count(end_year, type, district_id, campus_id, subgroup, grade_level) |>
    dplyr::filter(n > 1)

  expect_equal(
    nrow(dupes), 0,
    info = paste("Found", nrow(dupes), "duplicate group combinations")
  )
})


# ==============================================================================
# 13. Tidy output column completeness
# ==============================================================================

test_that("tidy output has all required columns", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  required_cols <- c(
    "end_year", "type",
    "district_id", "campus_id",
    "district_name", "campus_name",
    "grade_level", "subgroup", "n_students", "pct",
    "is_state", "is_district", "is_campus", "is_public"
  )

  for (col in required_cols) {
    expect_true(col %in% names(tidy), info = paste("Missing column:", col))
  }
})

test_that("subgroup values conform to naming standards", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  valid_subgroups <- c(
    "total_enrollment",
    "white", "black", "hispanic", "asian",
    "native_american", "pacific_islander", "multiracial",
    "male", "female"
  )

  actual_subgroups <- unique(tidy$subgroup)

  for (sg in actual_subgroups) {
    expect_true(
      sg %in% valid_subgroups,
      info = paste("Non-standard subgroup name:", sg)
    )
  }
})

test_that("grade_level values conform to naming standards", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  valid_grades <- c("PK", "K", "01", "02", "03", "04", "05", "06",
                    "07", "08", "09", "10", "11", "12", "UG", "TOTAL")

  actual_grades <- unique(tidy$grade_level)

  for (gl in actual_grades) {
    expect_true(
      gl %in% valid_grades,
      info = paste("Non-standard grade_level:", gl)
    )
  }
})


# ==============================================================================
# 14. KG -> K grade mapping correctness
# ==============================================================================

test_that("raw KG is mapped to K in tidy output (not KG)", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # K should exist, KG should not
  expect_true("K" %in% unique(tidy$grade_level))
  expect_false("KG" %in% unique(tidy$grade_level))
})

test_that("wide grade_k column maps to tidy grade_level K", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # State K enrollment should match
  state_wide <- wide[wide$type == "State", ]
  state_tidy_k <- tidy |>
    dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "K")

  expect_equal(state_tidy_k$n_students, state_wide$grade_k)
})


# ==============================================================================
# 15. Data quality: no Inf, NaN in transformed output
# ==============================================================================

test_that("no Inf or NaN values in tidy n_students", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  expect_false(any(is.infinite(tidy$n_students), na.rm = TRUE))
  expect_false(any(is.nan(tidy$n_students), na.rm = TRUE))
})

test_that("no Inf or NaN values in tidy pct", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  expect_false(any(is.infinite(tidy$pct), na.rm = TRUE))
  expect_false(any(is.nan(tidy$pct), na.rm = TRUE))
})

test_that("n_students is non-negative in tidy output", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  expect_true(all(tidy$n_students >= 0, na.rm = TRUE))
})

test_that("pct is between 0 and 1 inclusive", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  non_na_pct <- tidy$pct[!is.na(tidy$pct)]
  expect_true(all(non_na_pct >= 0))
  expect_true(all(non_na_pct <= 1.001))  # Small tolerance for floating point
})


# ==============================================================================
# 16. State race data gap: state aggregate lacks campus-derived race
# ==============================================================================
# BUG DISCOVERY: The state aggregate is computed from districts only
# (create_state_aggregate), but race data only exists at campus level.
# This means state-level race subgroups are always NA/empty in tidy output.

test_that("state-level race subgroups exist in tidy output (BUG: they are missing)", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  race_subgroups <- c("white", "black", "hispanic", "asian",
                      "native_american", "pacific_islander", "multiracial")

  state_race <- tidy |>
    dplyr::filter(is_state, grade_level == "TOTAL", subgroup %in% race_subgroups)

  # KNOWN BUG: State race rows are absent because create_state_aggregate
  # only sums district columns, and districts don't have race data.
  # Campus race data exists but isn't aggregated up to state level.
  #
  # This test documents the current behavior. When the bug is fixed,
  # change expect_equal(nrow(...), 0) to expect_equal(nrow(...), 7)
  expect_equal(
    nrow(state_race), 0,
    info = paste("Expected 0 state race rows (known bug).",
                 "If this fails with 7, the bug has been fixed!")
  )
})


# ==============================================================================
# 17. Gender data gap: male/female are always NA at campus level
# ==============================================================================
# BUG DISCOVERY: add_gender_data uses a school_key built from
# district_id + campus_id (5-digit + 2-digit format), but the campus
# processing builds campus_id as district_id + school_id (7-digit).
# The key mismatch means gender lookup always fails.

test_that("gender subgroups are absent from tidy output (BUG: key mismatch)", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  gender_rows <- tidy |>
    dplyr::filter(subgroup %in% c("male", "female"))

  # KNOWN BUG: Gender data is downloaded but never joins to campus data
  # because of school_key format mismatch between add_gender_data (uses
  # "district_id_campus_id" as "XXXXX_XXXXXXX") and the campus_df
  # (where campus_id is already "XXXXXNN" 7-digit format).
  #
  # When fixed, change to expect_gt(nrow(gender_rows), 0)
  expect_equal(
    nrow(gender_rows), 0,
    info = paste("Expected 0 gender rows (known bug).",
                 "If this fails with >0, the bug has been fixed!")
  )
})


# ==============================================================================
# 18. Leaked "State Total" row in campus data
# ==============================================================================
# BUG DISCOVERY: The race/ethnicity file contains a "State Total" row
# that process_campus_enr doesn't filter out. It appears as a Campus
# row with district_id "000NA" and campus_id "000NANA".

test_that("no State Total row leaks into campus data (BUG: it does)", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  # Check for leaked state total row in campus data
  leaked <- wide[wide$type == "Campus" &
                 !is.na(wide$district_name) &
                 grepl("State Total", wide$district_name, ignore.case = TRUE), ]

  # KNOWN BUG: A "State Total" row from the race file slips through as a
  # Campus row with district_id "000NA".
  #
  # When fixed, change to expect_equal(nrow(leaked), 0)
  expect_equal(
    nrow(leaked), 1,
    info = paste("Expected 1 leaked State Total row (known bug).",
                 "If this fails with 0, the bug has been fixed!")
  )
})


# ==============================================================================
# 19. Multi-year fetch consistency
# ==============================================================================

test_that("fetch_enr_multi returns same data as individual fetch_enr calls", {
  skip_on_cran()
  skip_if_offline()

  multi <- fetch_enr_multi(c(2024, 2025), tidy = TRUE, use_cache = TRUE)
  single_2024 <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  single_2025 <- fetch_enr(2025, tidy = TRUE, use_cache = TRUE)

  # Row counts should match
  multi_2024 <- multi[multi$end_year == 2024, ]
  multi_2025 <- multi[multi$end_year == 2025, ]

  expect_equal(nrow(multi_2024), nrow(single_2024))
  expect_equal(nrow(multi_2025), nrow(single_2025))

  # State totals should match
  multi_state_2024 <- multi |>
    dplyr::filter(end_year == 2024, is_state,
                  subgroup == "total_enrollment", grade_level == "TOTAL") |>
    dplyr::pull(n_students)
  single_state_2024 <- single_2024 |>
    dplyr::filter(is_state, subgroup == "total_enrollment",
                  grade_level == "TOTAL") |>
    dplyr::pull(n_students)

  expect_equal(multi_state_2024, single_state_2024)
})


# ==============================================================================
# 20. end_year column consistency
# ==============================================================================

test_that("end_year is consistent across all rows for single-year fetch", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(all(tidy$end_year == 2024))

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  expect_true(all(wide$end_year == 2024))
})


# ==============================================================================
# 21. tidy_enr filters out NA n_students rows
# ==============================================================================

test_that("tidy output has no NA n_students values", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # tidy_enr ends with filter(!is.na(n_students))
  expect_equal(sum(is.na(tidy$n_students)), 0)
})


# ==============================================================================
# 22. Cross-year fidelity: same entity same district_id across years
# ==============================================================================

test_that("Sioux Falls has the same district_id across years", {
  skip_on_cran()
  skip_if_offline()

  for (yr in c(2024, 2025)) {
    tidy <- fetch_enr(yr, tidy = TRUE, use_cache = TRUE)
    sf <- tidy |>
      dplyr::filter(is_district,
                    grepl("Sioux Falls", district_name, ignore.case = TRUE),
                    subgroup == "total_enrollment",
                    grade_level == "TOTAL")

    expect_true(nrow(sf) > 0, info = paste("No Sioux Falls in year", yr))
    expect_equal(sf$district_id[1], "49005",
                 info = paste("Sioux Falls district_id wrong in year", yr))
  }
})


# ==============================================================================
# 23. Campus-level: campus_id uniquely identifies a school within a year
# ==============================================================================

test_that("campus_id is unique per campus row in wide format", {
  skip_on_cran()
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  campuses <- wide[wide$type == "Campus", ]

  # Exclude the leaked State Total row
  campuses <- campuses[!grepl("State Total", campuses$district_name, ignore.case = TRUE), ]

  # campus_id should be unique
  expect_equal(length(unique(campuses$campus_id)), nrow(campuses))
})


# ==============================================================================
# 24. Era consistency: 2015 (Era 2) vs 2024 (Era 3) column schema
# ==============================================================================

test_that("Era 2 and Era 3 produce same tidy column set", {
  skip_on_cran()
  skip_if_offline()

  tidy_2015 <- fetch_enr(2015, tidy = TRUE, use_cache = TRUE)
  tidy_2024 <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # Core columns should be present in both eras
  core_cols <- c("end_year", "type", "district_id", "subgroup",
                 "grade_level", "n_students", "pct",
                 "is_state", "is_district")

  for (col in core_cols) {
    expect_true(col %in% names(tidy_2015),
                info = paste("2015 missing column:", col))
    expect_true(col %in% names(tidy_2024),
                info = paste("2024 missing column:", col))
  }
})

test_that("Era 2 state total is in reasonable range", {
  skip_on_cran()
  skip_if_offline()

  tidy <- fetch_enr(2015, tidy = TRUE, use_cache = TRUE)
  state_total <- tidy |>
    dplyr::filter(is_state, subgroup == "total_enrollment",
                  grade_level == "TOTAL") |>
    dplyr::pull(n_students)

  # SD had ~134k students in 2015
  expect_gt(state_total, 120000)
  expect_lt(state_total, 160000)
})
