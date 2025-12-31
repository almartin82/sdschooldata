# ==============================================================================
# Raw Enrollment Data Download Functions
# ==============================================================================
#
# This file contains functions for downloading raw enrollment data from the
# South Dakota Department of Education. Data comes from Excel files hosted
# at https://doe.sd.gov/ofm/enrollment.aspx
#
# Data structure:
# - District-level: Enrollment by district and grade
# - School-level: Enrollment by school, grade, race/ethnicity, and gender
#
# Format eras:
# - Era 0 (2006-2010): Various XLS formats with different naming conventions
#   - 2007: Summary totals only (no grade breakdown)
#   - 2006, 2008-2010: Full grade breakdown available
# - Era 1 (2011-2012): FE{YY}_Psum.xlsx format with simple column structure
# - Era 2 (2013-2020): Pubdsgr{YY}.xlsx format with slight variations
# - Era 3 (2021+): Pubdisgr-{YYYY}.xlsx format with consistent structure
#
# ==============================================================================

#' Download raw enrollment data from SD DOE
#'
#' Downloads district and school enrollment data from SD DOE's Excel files.
#' Combines grade-level enrollment with demographic breakdowns.
#'
#' @param end_year School year end (2024-25 = 2025)
#' @return List with district and campus data frames
#' @keywords internal
get_raw_enr <- function(end_year) {

  # Validate year
  available_years <- get_available_years()
  if (!end_year %in% available_years) {
    stop("end_year must be between ", min(available_years), " and ",
         max(available_years), ". Got: ", end_year)
  }

  message(paste("Downloading SD DOE enrollment data for", end_year, "..."))

  # Route to appropriate download function based on year/era
  if (end_year <= 2010) {
    result <- get_raw_enr_era0(end_year)
  } else if (end_year <= 2012) {
    result <- get_raw_enr_era1(end_year)
  } else if (end_year <= 2020) {
    result <- get_raw_enr_era2(end_year)
  } else {
    result <- get_raw_enr_era3(end_year)
  }

  result
}


#' Download raw enrollment data for Era 0 (2006-2010)
#'
#' Historical data from South Dakota DOE. These files use XLS format
#' with varying structures. 2007 only has summary totals, while other
#' years have grade-level breakdowns.
#'
#' @param end_year School year end
#' @return List with district and campus data frames
#' @keywords internal
get_raw_enr_era0 <- function(end_year) {

  message("  Downloading district data (Era 0)...")

  # Download district-level file
  district_url <- build_sd_url(end_year, "district")

  # 2007 file structure is different (summary only, skip_rows = 0)
  # Other years have header rows to skip
  skip_rows <- if (end_year == 2007) 0 else 2

  district_data <- download_sd_excel(district_url, end_year, skip_rows = skip_rows)

  # Download school-level race/ethnicity data (not available for 2006)
  race_url <- get_race_filename(end_year)
  race_data <- NULL
  if (!is.null(race_url)) {
    message("  Downloading school race/ethnicity data...")
    race_url_full <- build_sd_url(end_year, "school_race")
    race_data <- tryCatch(
      download_sd_excel(race_url_full, end_year, skip_rows = 5),
      error = function(e) {
        message("    Warning: Could not download race data: ", e$message)
        NULL
      }
    )
  }

  # Download school-level gender data (not available for 2006)
  gender_url <- get_gender_filename(end_year)
  gender_data <- NULL
  if (!is.null(gender_url)) {
    message("  Downloading school gender data...")
    gender_url_full <- build_sd_url(end_year, "school_gender")
    gender_data <- tryCatch(
      download_sd_excel(gender_url_full, end_year, skip_rows = 5),
      error = function(e) {
        message("    Warning: Could not download gender data: ", e$message)
        NULL
      }
    )
  }

  # Add year column
  district_data$end_year <- end_year
  if (!is.null(race_data)) race_data$end_year <- end_year
  if (!is.null(gender_data)) gender_data$end_year <- end_year

  list(
    district = district_data,
    campus_race = race_data,
    campus_gender = gender_data
  )
}


#' Download raw enrollment data for Era 1 (2011-2012)
#'
#' @param end_year School year end
#' @return List with district and campus data frames
#' @keywords internal
get_raw_enr_era1 <- function(end_year) {

  message("  Downloading district data (Era 1)...")

  # Download district-level file
  district_url <- build_sd_url(end_year, "district")
  district_data <- download_sd_excel(district_url, end_year, skip_rows = 2)

  # Add year column
  district_data$end_year <- end_year

  # For Era 1, we only have district-level data in a simple format
  # School-level data may not be available in consistent format

  list(
    district = district_data,
    campus = NULL  # Campus data not reliably available for Era 1
  )
}


#' Download raw enrollment data for Era 2 (2013-2020)
#'
#' @param end_year School year end
#' @return List with district and campus data frames
#' @keywords internal
get_raw_enr_era2 <- function(end_year) {

  message("  Downloading district data (Era 2)...")

  # Download district-level file
  district_url <- build_sd_url(end_year, "district")

  # Skip rows varies by year due to header changes
  # 2013-2017: 2 rows, 2018-2020: 3 rows (different header structure)
  skip_rows <- if (end_year <= 2017) 2 else 3

  district_data <- download_sd_excel(district_url, end_year, skip_rows = skip_rows)

  # Download school-level race/ethnicity data
  message("  Downloading school race/ethnicity data...")
  race_url <- build_sd_url(end_year, "school_race")
  race_data <- tryCatch(
    download_sd_excel(race_url, end_year, skip_rows = 5),
    error = function(e) {
      message("    Warning: Could not download race data: ", e$message)
      NULL
    }
  )

  # Download school-level gender data
  message("  Downloading school gender data...")
  gender_url <- build_sd_url(end_year, "school_gender")
  gender_data <- tryCatch(
    download_sd_excel(gender_url, end_year, skip_rows = 5),
    error = function(e) {
      message("    Warning: Could not download gender data: ", e$message)
      NULL
    }
  )

  # Add year column
  district_data$end_year <- end_year
  if (!is.null(race_data)) race_data$end_year <- end_year
  if (!is.null(gender_data)) gender_data$end_year <- end_year

  list(
    district = district_data,
    campus_race = race_data,
    campus_gender = gender_data
  )
}


#' Download raw enrollment data for Era 3 (2021+)
#'
#' @param end_year School year end
#' @return List with district and campus data frames
#' @keywords internal
get_raw_enr_era3 <- function(end_year) {

  message("  Downloading district data (Era 3)...")

  # Download district-level file
  district_url <- build_sd_url(end_year, "district")
  district_data <- download_sd_excel(district_url, end_year, skip_rows = 5)

  # Download school-level race/ethnicity data
  message("  Downloading school race/ethnicity data...")
  race_url <- build_sd_url(end_year, "school_race")
  race_data <- tryCatch(
    download_sd_excel(race_url, end_year, skip_rows = 5),
    error = function(e) {
      message("    Warning: Could not download race data: ", e$message)
      NULL
    }
  )

  # Download school-level gender data
  message("  Downloading school gender data...")
  gender_url <- build_sd_url(end_year, "school_gender")
  gender_data <- tryCatch(
    download_sd_excel(gender_url, end_year, skip_rows = 5),
    error = function(e) {
      message("    Warning: Could not download gender data: ", e$message)
      NULL
    }
  )

  # Add year column
  district_data$end_year <- end_year
  if (!is.null(race_data)) race_data$end_year <- end_year
  if (!is.null(gender_data)) gender_data$end_year <- end_year

  list(
    district = district_data,
    campus_race = race_data,
    campus_gender = gender_data
  )
}


#' Download an Excel file from SD DOE
#'
#' Downloads and reads an Excel file from the SD DOE website.
#'
#' @param url Full URL to the Excel file
#' @param end_year School year end (for error messages)
#' @param skip_rows Number of header rows to skip
#' @return Data frame
#' @keywords internal
download_sd_excel <- function(url, end_year, skip_rows = 0) {

  # Create temp file
  ext <- if (grepl("\\.xlsx$", url, ignore.case = TRUE)) ".xlsx" else ".xls"
  tname <- tempfile(
    pattern = paste0("sd_enr_", end_year, "_"),
    tmpdir = tempdir(),
    fileext = ext
  )

  # Download file with extended timeouts for slow SD DOE server
  tryCatch({
    response <- httr::GET(
      url,
      httr::write_disk(tname, overwrite = TRUE),
      httr::timeout(600),  # 10 minute timeout
      httr::config(connecttimeout = 120)  # 2 minute connection timeout
    )

    # Check for HTTP errors
    if (httr::http_error(response)) {
      stop(paste("HTTP error:", httr::status_code(response)))
    }

    # Check file size (small files likely error pages)
    file_info <- file.info(tname)
    if (file_info$size < 500) {
      content <- readLines(tname, n = 10, warn = FALSE)
      if (any(grepl("error|not found|404", content, ignore.case = TRUE))) {
        stop(paste("File not found or error page returned"))
      }
    }

  }, error = function(e) {
    stop(paste("Failed to download data for year", end_year,
               "\nURL:", url,
               "\nError:", e$message))
  })

  # Read the Excel file
  df <- tryCatch({
    if (ext == ".xlsx") {
      readxl::read_xlsx(
        tname,
        skip = skip_rows,
        col_types = "text"  # Read all as text for consistent handling
      )
    } else {
      # Try readxl first, then fall back to Python xlrd for older XLS files
      tryCatch({
        readxl::read_xls(
          tname,
          skip = skip_rows,
          col_types = "text"
        )
      }, error = function(e) {
        # readxl can't handle some older XLS formats; try Python xlrd
        read_xls_via_python(tname, skip_rows)
      })
    }
  }, error = function(e) {
    stop(paste("Failed to read Excel file for year", end_year,
               "\nError:", e$message))
  })

  # Clean up temp file
  unlink(tname)

  # Remove empty rows and columns
  df <- df[rowSums(!is.na(df) & df != "") > 0, ]

  df
}


#' Read XLS file using Python xlrd
#'
#' Fallback function for reading older XLS files that readxl cannot handle.
#' Requires Python with xlrd package installed.
#'
#' @param filepath Path to the XLS file
#' @param skip_rows Number of rows to skip
#' @return Data frame with file contents
#' @keywords internal
read_xls_via_python <- function(filepath, skip_rows = 0) {

  # Check if Python is available
  python_path <- Sys.which("python3")
  if (python_path == "") {
    python_path <- Sys.which("python")
  }
  if (python_path == "") {
    stop("Python is required to read older XLS files. Please install Python and the xlrd package.")
  }

  # Create a temporary CSV file to receive the converted data
  csv_temp <- tempfile(fileext = ".csv")

  # Python script to convert XLS to CSV
  python_script <- sprintf('
import sys
import csv

try:
    import xlrd
except ImportError:
    print("ERROR: xlrd package not installed. Install with: pip install xlrd", file=sys.stderr)
    sys.exit(1)

try:
    wb = xlrd.open_workbook("%s")
    sheet = wb.sheet_by_index(0)

    with open("%s", "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        for row_idx in range(%d, sheet.nrows):
            row = []
            for col_idx in range(sheet.ncols):
                cell = sheet.cell(row_idx, col_idx)
                value = cell.value
                # Convert numbers to string
                if cell.ctype == 2:  # XL_CELL_NUMBER
                    if value == int(value):
                        value = str(int(value))
                    else:
                        value = str(value)
                elif cell.ctype == 0:  # XL_CELL_EMPTY
                    value = ""
                else:
                    value = str(value)
                row.append(value)
            writer.writerow(row)
    print("SUCCESS")
except Exception as e:
    print(f"ERROR: {e}", file=sys.stderr)
    sys.exit(1)
', filepath, csv_temp, skip_rows)

  # Write the Python script to a temp file
  script_temp <- tempfile(fileext = ".py")
  writeLines(python_script, script_temp)

  # Run Python
  result <- tryCatch({
    system2(python_path, args = script_temp, stdout = TRUE, stderr = TRUE)
  }, error = function(e) {
    stop(paste("Failed to run Python:", e$message))
  })

  # Clean up script
  unlink(script_temp)

  # Check for success
  if (!"SUCCESS" %in% result) {
    error_msg <- paste(result, collapse = "\n")
    stop(paste("Python xlrd failed to read XLS file:", error_msg))
  }

  # Read the CSV
  df <- tryCatch({
    readr::read_csv(csv_temp, col_types = readr::cols(.default = readr::col_character()),
                    show_col_types = FALSE)
  }, error = function(e) {
    stop(paste("Failed to read converted CSV:", e$message))
  })

  # Clean up CSV
  unlink(csv_temp)

  df
}


#' Get column mapping for SD DOE enrollment data
#'
#' Returns a list mapping raw SD DOE column names to standardized names.
#' SD DOE uses varying column names across years.
#'
#' @param end_year School year end
#' @return Named list of column mappings
#' @keywords internal
get_sd_column_map <- function(end_year) {

  # District file columns
  district_cols <- list(
    district_name = c("District Name", "District"),
    district_id = c("District No.", "District Number", "District Num"),
    district_type_code = c("District Type", "District Type Code"),
    district_type_name = c("District Type Description", "District Type Desc"),
    grade_pk = c("PK", "Pre-K", "PreK"),
    grade_k = c("KG", "K", "Kindergarten"),
    grade_01 = c("01", "1", "Grade 1"),
    grade_02 = c("02", "2", "Grade 2"),
    grade_03 = c("03", "3", "Grade 3"),
    grade_04 = c("04", "4", "Grade 4"),
    grade_05 = c("05", "5", "Grade 5"),
    grade_06 = c("06", "6", "Grade 6"),
    grade_07 = c("07", "7", "Grade 7"),
    grade_08 = c("08", "8", "Grade 8"),
    grade_09 = c("09", "9", "Grade 9"),
    grade_10 = c("10", "Grade 10"),
    grade_11 = c("11", "Grade 11"),
    grade_12 = c("12", "Grade 12"),
    grade_ug = c("UG", "Ungraded"),
    total = c("Total", "TOTAL", "TOTAL PK-12", "TOTAL KG-12", "Total PK-12")
  )

  # Race/ethnicity values
  race_values <- list(
    native_american = c("American Indian or Alaskan Native", "American Indian/Alaskan Native",
                       "American Indian", "Native American"),
    asian = c("Asian"),
    black = c("Black or African American", "Black", "African American"),
    pacific_islander = c("Hawaiian or Pacific Islander", "Native Hawaiian/Pacific Islander",
                        "Pacific Islander"),
    hispanic = c("Hispanic/Latino", "Hispanic", "Latino"),
    multiracial = c("Two or More Races", "Two or more races", "Multiracial"),
    white = c("White")
  )

  # Gender values
  gender_values <- list(
    female = c("Female", "F"),
    male = c("Male", "M")
  )

  list(
    district = district_cols,
    race = race_values,
    gender = gender_values
  )
}
