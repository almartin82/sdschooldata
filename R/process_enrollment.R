# ==============================================================================
# Enrollment Data Processing Functions
# ==============================================================================
#
# This file contains functions for processing raw SD DOE enrollment data into a
# clean, standardized format.
#
# ==============================================================================

#' Process raw SD DOE enrollment data
#'
#' Transforms raw Excel data into a standardized schema combining district
#' and campus data.
#'
#' @param raw_data List containing district and campus data frames from get_raw_enr
#' @param end_year School year end
#' @return Processed data frame with standardized columns
#' @keywords internal
process_enr <- function(raw_data, end_year) {

  # Process district data
  district_processed <- process_district_enr(raw_data$district, end_year)

  # Process campus data (from race/ethnicity and gender files)
  campus_processed <- NULL
  if (!is.null(raw_data$campus_race)) {
    campus_processed <- process_campus_enr(
      raw_data$campus_race,
      raw_data$campus_gender,
      end_year
    )
  }

  # Create state aggregate
  state_processed <- create_state_aggregate(district_processed, end_year)

  # Combine all levels
  if (!is.null(campus_processed)) {
    result <- dplyr::bind_rows(state_processed, district_processed, campus_processed)
  } else {
    result <- dplyr::bind_rows(state_processed, district_processed)
  }

  result
}


#' Process district-level enrollment data
#'
#' @param df Raw district data frame
#' @param end_year School year end
#' @return Processed district data frame
#' @keywords internal
process_district_enr <- function(df, end_year) {

  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  cols <- names(df)
  n_rows <- nrow(df)

  # Helper to find column by pattern (case-insensitive)
  find_col <- function(patterns) {
    for (pattern in patterns) {
      # First try exact match
      if (pattern %in% cols) return(pattern)
      # Then try case-insensitive match
      matched <- grep(paste0("^", pattern, "$"), cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
    }
    NULL
  }

  # Build result dataframe
  result <- data.frame(
    end_year = rep(end_year, n_rows),
    type = rep("District", n_rows),
    stringsAsFactors = FALSE
  )

  # District ID - clean to 5-digit format
  district_id_col <- find_col(c("District No.", "District Number", "District Num", "DistrictNo"))
  if (is.null(district_id_col)) {
    # Try to find column containing "No" or "Number"
    no_cols <- grep("No\\.|Number|Num$", cols, value = TRUE, ignore.case = TRUE)
    if (length(no_cols) > 0) district_id_col <- no_cols[1]
  }
  # If still not found, check if first column contains numeric district IDs
  if (is.null(district_id_col) && length(cols) > 0) {
    first_col <- cols[1]
    first_vals <- df[[first_col]][1:min(5, nrow(df))]
    if (all(grepl("^\\d+$", first_vals, na.rm = TRUE))) {
      district_id_col <- first_col
    }
  }
  if (!is.null(district_id_col) && !is.na(district_id_col)) {
    result$district_id <- sprintf("%05d", as.integer(gsub("[^0-9]", "", df[[district_id_col]])))
  } else {
    result$district_id <- NA_character_
  }

  # Campus ID is NA for district rows
  result$campus_id <- rep(NA_character_, n_rows)

  # District Name
  district_name_col <- find_col(c("District Name", "DistrictName"))
  if (is.null(district_name_col)) {
    # Try to find column containing "Name" that's not a school
    name_cols <- grep("Name", cols, value = TRUE, ignore.case = TRUE)
    name_cols <- name_cols[!grepl("School|Campus", name_cols, ignore.case = TRUE)]
    if (length(name_cols) > 0) district_name_col <- name_cols[1]
  }
  # If still not found, check if second column contains district names
  if (is.null(district_name_col) && length(cols) > 1) {
    second_col <- cols[2]
    second_vals <- df[[second_col]][1:min(3, nrow(df))]
    # District names typically contain letters and spaces
    if (all(grepl("[A-Za-z]", second_vals, na.rm = TRUE))) {
      district_name_col <- second_col
    }
  }
  if (!is.null(district_name_col) && !is.na(district_name_col)) {
    result$district_name <- trimws(df[[district_name_col]])
  } else {
    result$district_name <- NA_character_
  }

  result$campus_name <- rep(NA_character_, n_rows)

  # District type
  type_code_col <- find_col(c("District Type", "District Type Code"))
  if (!is.null(type_code_col)) {
    result$district_type_code <- trimws(df[[type_code_col]])
  }

  type_name_col <- find_col(c("District Type Description", "District Type Desc", "District Type Des"))
  if (!is.null(type_name_col)) {
    result$district_type_name <- trimws(df[[type_name_col]])
  }

  # Grade level columns - SD uses PK, KG, 01-12, UG
  grade_map <- list(
    grade_pk = c("PK", "Pre-K", "PreK"),
    grade_k = c("KG", "K", "Kindergarten"),
    grade_01 = c("01", "1"),
    grade_02 = c("02", "2"),
    grade_03 = c("03", "3"),
    grade_04 = c("04", "4"),
    grade_05 = c("05", "5"),
    grade_06 = c("06", "6"),
    grade_07 = c("07", "7"),
    grade_08 = c("08", "8"),
    grade_09 = c("09", "9"),
    grade_10 = c("10"),
    grade_11 = c("11"),
    grade_12 = c("12"),
    grade_ug = c("UG", "Ungraded")
  )

  for (name in names(grade_map)) {
    col <- find_col(grade_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(df[[col]])
    }
  }

  # Total enrollment - look for TOTAL column
  total_col <- find_col(c("Total", "TOTAL", "TOTAL PK-12", "TOTAL KG-12", "Total PK-12"))
  if (is.null(total_col)) {
    # Look for column containing "TOTAL"
    total_col <- grep("TOTAL|Total$", cols, value = TRUE, ignore.case = FALSE)[1]
  }
  if (!is.null(total_col)) {
    result$row_total <- safe_numeric(df[[total_col]])
  } else {
    # Calculate from grade columns
    grade_cols <- grep("^grade_", names(result), value = TRUE)
    if (length(grade_cols) > 0) {
      result$row_total <- rowSums(result[, grade_cols], na.rm = TRUE)
    }
  }

  # Filter out header rows and state totals
  result <- result[!is.na(result$district_id) & result$district_id != "NA", ]
  result <- result[!grepl("Total|State|District Name", result$district_name, ignore.case = TRUE), ]

  result
}


#' Process campus-level enrollment data
#'
#' Combines race/ethnicity and gender data at the school level.
#'
#' @param race_df Raw race/ethnicity data frame
#' @param gender_df Raw gender data frame
#' @param end_year School year end
#' @return Processed campus data frame
#' @keywords internal
process_campus_enr <- function(race_df, gender_df, end_year) {

  if (is.null(race_df) || nrow(race_df) == 0) {
    return(NULL)
  }

  cols <- names(race_df)

  # Helper to find column by pattern
  find_col <- function(patterns, col_names = cols) {
    for (pattern in patterns) {
      if (pattern %in% col_names) return(pattern)
      matched <- grep(paste0("^", pattern, "$"), col_names, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
    }
    NULL
  }

  # Find key columns
  district_name_col <- find_col(c("District Name", "DistrictName"))
  district_id_col <- find_col(c("District No.", "District Number", "DistrictNo"))
  school_name_col <- find_col(c("School Name", "SchoolName", "School"))
  school_id_col <- find_col(c("School No.", "School Number", "SchoolNo"))
  race_col <- find_col(c("Race/Ethnicity", "Race", "Ethnicity"))

  if (is.null(school_name_col) || is.null(race_col)) {
    message("    Warning: Could not identify required columns in race data")
    return(NULL)
  }

  # Clean race_df - remove header rows
  race_df <- race_df[!grepl("^District Name$|^Race|^School Name$", race_df[[1]], ignore.case = TRUE), ]

  # Get grade columns (PK through 12 plus UG and Total)
  grade_cols <- c("PK", "KG", "01", "02", "03", "04", "05", "06",
                  "07", "08", "09", "10", "11", "12", "UG", "Total")
  available_grade_cols <- intersect(grade_cols, cols)

  # Pivot from long (one row per race) to wide (one row per school)
  # First, identify school and aggregate by race

  # Build unique school identifier
  race_df$school_key <- paste(
    if (!is.null(district_id_col)) race_df[[district_id_col]] else "",
    if (!is.null(school_id_col)) race_df[[school_id_col]] else race_df[[school_name_col]],
    sep = "_"
  )

  # Get unique schools
  unique_schools <- unique(race_df$school_key)

  # Build result dataframe for each school
  result_list <- lapply(unique_schools, function(sk) {
    school_rows <- race_df[race_df$school_key == sk, ]

    if (nrow(school_rows) == 0) return(NULL)

    first_row <- school_rows[1, ]

    row_data <- data.frame(
      end_year = end_year,
      type = "Campus",
      district_id = if (!is.null(district_id_col))
        sprintf("%05d", as.integer(gsub("[^0-9]", "", first_row[[district_id_col]])))
        else NA_character_,
      campus_id = if (!is.null(school_id_col))
        sprintf("%02d", as.integer(gsub("[^0-9]", "", first_row[[school_id_col]])))
        else NA_character_,
      district_name = if (!is.null(district_name_col))
        trimws(first_row[[district_name_col]])
        else NA_character_,
      campus_name = trimws(first_row[[school_name_col]]),
      stringsAsFactors = FALSE
    )

    # Make campus_id unique by combining district_id and school_id
    if (!is.na(row_data$district_id) && !is.na(row_data$campus_id)) {
      row_data$campus_id <- paste0(row_data$district_id, row_data$campus_id)
    }

    # Get total enrollment from Total column (sum across all races)
    total_col <- find_col(c("Total", "TOTAL"))
    if (!is.null(total_col)) {
      row_data$row_total <- sum(safe_numeric(school_rows[[total_col]]), na.rm = TRUE)
    }

    # Aggregate race counts
    race_mapping <- list(
      native_american = c("American Indian or Alaskan Native", "American Indian/Alaskan Native"),
      asian = c("Asian"),
      black = c("Black or African American", "Black"),
      pacific_islander = c("Hawaiian or Pacific Islander", "Native Hawaiian/Pacific Islander"),
      hispanic = c("Hispanic/Latino", "Hispanic"),
      multiracial = c("Two or More Races", "Two or more races"),
      white = c("White")
    )

    for (race_name in names(race_mapping)) {
      race_patterns <- race_mapping[[race_name]]
      race_rows <- school_rows[school_rows[[race_col]] %in% race_patterns, ]
      if (nrow(race_rows) > 0 && !is.null(total_col)) {
        row_data[[race_name]] <- sum(safe_numeric(race_rows[[total_col]]), na.rm = TRUE)
      } else {
        row_data[[race_name]] <- NA_integer_
      }
    }

    # Aggregate grade-level counts (sum across all races)
    grade_map <- list(
      grade_pk = "PK",
      grade_k = "KG",
      grade_01 = "01",
      grade_02 = "02",
      grade_03 = "03",
      grade_04 = "04",
      grade_05 = "05",
      grade_06 = "06",
      grade_07 = "07",
      grade_08 = "08",
      grade_09 = "09",
      grade_10 = "10",
      grade_11 = "11",
      grade_12 = "12",
      grade_ug = "UG"
    )

    for (grade_name in names(grade_map)) {
      col_name <- grade_map[[grade_name]]
      if (col_name %in% cols) {
        row_data[[grade_name]] <- sum(safe_numeric(school_rows[[col_name]]), na.rm = TRUE)
      }
    }

    row_data
  })

  result <- dplyr::bind_rows(result_list)

  # Process gender data if available
  if (!is.null(gender_df) && nrow(gender_df) > 0) {
    result <- add_gender_data(result, gender_df, end_year)
  }

  result
}


#' Add gender data to campus enrollment data
#'
#' @param campus_df Processed campus data frame
#' @param gender_df Raw gender data frame
#' @param end_year School year end
#' @return Campus data frame with gender columns added
#' @keywords internal
add_gender_data <- function(campus_df, gender_df, end_year) {

  if (is.null(gender_df) || nrow(gender_df) == 0) {
    campus_df$male <- NA_integer_
    campus_df$female <- NA_integer_
    return(campus_df)
  }

  cols <- names(gender_df)

  # Helper to find column
  find_col <- function(patterns, col_names = cols) {
    for (pattern in patterns) {
      if (pattern %in% col_names) return(pattern)
      matched <- grep(paste0("^", pattern, "$"), col_names, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
    }
    NULL
  }

  # Find key columns
  district_id_col <- find_col(c("District No.", "District Number", "DistrictNo"))
  school_id_col <- find_col(c("School No.", "School Number", "SchoolNo"))
  school_name_col <- find_col(c("School Name", "SchoolName", "School"))
  gender_col <- find_col(c("Gender"))
  total_col <- find_col(c("Total", "TOTAL"))

  if (is.null(gender_col) || is.null(total_col)) {
    campus_df$male <- NA_integer_
    campus_df$female <- NA_integer_
    return(campus_df)
  }

  # Clean gender_df - remove header rows
  gender_df <- gender_df[!grepl("^District Name$|^Gender$|^School Name$", gender_df[[1]], ignore.case = TRUE), ]

  # Build school key for matching
  gender_df$school_key <- paste(
    if (!is.null(district_id_col))
      sprintf("%05d", as.integer(gsub("[^0-9]", "", gender_df[[district_id_col]])))
    else "",
    if (!is.null(school_id_col))
      sprintf("%02d", as.integer(gsub("[^0-9]", "", gender_df[[school_id_col]])))
    else "",
    sep = "_"
  )

  # Create lookup of gender totals by school
  gender_lookup <- list()
  for (i in seq_len(nrow(gender_df))) {
    sk <- gender_df$school_key[i]
    gender_val <- gender_df[[gender_col]][i]
    total_val <- safe_numeric(gender_df[[total_col]][i])

    if (is.na(sk) || sk == "_") next

    if (!sk %in% names(gender_lookup)) {
      gender_lookup[[sk]] <- list(male = 0, female = 0)
    }

    if (grepl("Male", gender_val, ignore.case = TRUE) && !grepl("Female", gender_val, ignore.case = TRUE)) {
      gender_lookup[[sk]]$male <- gender_lookup[[sk]]$male + ifelse(is.na(total_val), 0, total_val)
    } else if (grepl("Female", gender_val, ignore.case = TRUE)) {
      gender_lookup[[sk]]$female <- gender_lookup[[sk]]$female + ifelse(is.na(total_val), 0, total_val)
    }
  }

  # Match to campus data
  campus_df$school_key <- paste(campus_df$district_id, campus_df$campus_id, sep = "_")

  campus_df$male <- sapply(campus_df$school_key, function(sk) {
    if (sk %in% names(gender_lookup)) gender_lookup[[sk]]$male else NA_integer_
  })

  campus_df$female <- sapply(campus_df$school_key, function(sk) {
    if (sk %in% names(gender_lookup)) gender_lookup[[sk]]$female else NA_integer_
  })

  campus_df$school_key <- NULL

  campus_df
}


#' Create state-level aggregate from district data
#'
#' @param district_df Processed district data frame
#' @param end_year School year end
#' @return Single-row data frame with state totals
#' @keywords internal
create_state_aggregate <- function(district_df, end_year) {

  if (is.null(district_df) || nrow(district_df) == 0) {
    return(NULL)
  }

  # Columns to sum
  sum_cols <- c(
    "row_total",
    "white", "black", "hispanic", "asian",
    "pacific_islander", "native_american", "multiracial",
    "male", "female",
    "grade_pk", "grade_k",
    "grade_01", "grade_02", "grade_03", "grade_04",
    "grade_05", "grade_06", "grade_07", "grade_08",
    "grade_09", "grade_10", "grade_11", "grade_12",
    "grade_ug"
  )

  # Filter to columns that exist
  sum_cols <- sum_cols[sum_cols %in% names(district_df)]

  # Create state row
  state_row <- data.frame(
    end_year = end_year,
    type = "State",
    district_id = NA_character_,
    campus_id = NA_character_,
    district_name = NA_character_,
    campus_name = NA_character_,
    stringsAsFactors = FALSE
  )

  # Sum each column
  for (col in sum_cols) {
    state_row[[col]] <- sum(district_df[[col]], na.rm = TRUE)
  }

  state_row
}
