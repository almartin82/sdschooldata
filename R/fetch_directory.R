# ==============================================================================
# School Directory Data Fetching Functions
# ==============================================================================
#
# This file contains functions for downloading school directory data from the
# South Dakota Department of Education (SD DOE).
#
# Data source: https://doe.sd.gov/ofm/edudir.aspx
#
# SD DOE publishes two separate Excel files:
#   - Principal & School Info: school-level data with principal names, addresses
#   - Superintendents, Administrators & District Info: district-level data with
#     superintendent names, district addresses, website
#
# These are joined by district number to produce a combined directory.
#
# ==============================================================================


#' Fetch South Dakota school directory data
#'
#' Downloads and processes school directory data from the South Dakota Department
#' of Education. This joins two separate SD DOE files (principal/school info and
#' superintendent/district info) to create a combined directory with both
#' school-level and district-level contact information.
#'
#' @param end_year Currently unused. The directory data represents the current
#'   school year. Included for API consistency with other fetch functions.
#' @param tidy If TRUE (default), returns data in a standardized format with
#'   consistent column names. If FALSE, returns raw column names from SD DOE.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#'   Set to FALSE to force re-download from SD DOE.
#' @return A tibble with school directory data. Columns include:
#'   \itemize{
#'     \item \code{state_district_id}: District identifier (e.g., "06001")
#'     \item \code{state_school_id}: School identifier
#'     \item \code{district_name}: District name
#'     \item \code{school_name}: School name
#'     \item \code{entity_type}: District type description
#'     \item \code{school_type}: School type description
#'     \item \code{grades_served}: Grade span (e.g., "KG05", "0912")
#'     \item \code{address}: Physical street address
#'     \item \code{city}: Physical address city
#'     \item \code{state}: State (always "SD")
#'     \item \code{zip}: Physical address ZIP code
#'     \item \code{phone}: Principal phone number
#'     \item \code{county_name}: Physical county
#'     \item \code{principal_name}: Principal full name (first + last)
#'     \item \code{principal_email}: NA (not available from SD DOE)
#'     \item \code{superintendent_name}: Superintendent full name (first + last)
#'     \item \code{superintendent_email}: NA (not available from SD DOE)
#'     \item \code{website}: District website URL
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Get school directory data
#' dir_data <- fetch_directory()
#'
#' # Get raw format (original SD DOE column names)
#' dir_raw <- fetch_directory(tidy = FALSE)
#'
#' # Force fresh download (ignore cache)
#' dir_fresh <- fetch_directory(use_cache = FALSE)
#'
#' # Filter to specific district
#' library(dplyr)
#' sioux_falls <- dir_data |>
#'   filter(grepl("Sioux Falls", district_name))
#' }
fetch_directory <- function(end_year = NULL, tidy = TRUE, use_cache = TRUE) {

  # Determine cache type based on tidy parameter
  cache_type <- if (tidy) "directory_tidy" else "directory_raw"

  # Check cache first
  if (use_cache && cache_exists_directory(cache_type)) {
    message("Using cached school directory data")
    return(read_cache_directory(cache_type))
  }

  # Get raw data from SD DOE (downloads BOTH files and joins)
  raw <- get_raw_directory()

  # Process to standard schema
  if (tidy) {
    result <- process_directory(raw)
  } else {
    result <- raw
  }

  # Cache the result
  if (use_cache) {
    write_cache_directory(result, cache_type)
  }

  result
}


#' Get raw school directory data from SD DOE
#'
#' Downloads both the principal/school info file and the superintendent/district
#' info file from SD DOE and joins them by district number.
#'
#' @return Combined data frame with school and district directory data
#' @keywords internal
get_raw_directory <- function() {

  message("Downloading school directory data from SD DOE...")

  # Download principal/school file
  principal_url <- "https://doe.sd.gov/ofm/documents/1025-Principal.xlsx"
  principal_data <- download_directory_excel(principal_url, "principal", skip_rows = 4)
  message(paste("  Principal file:", nrow(principal_data), "schools"))


  # Download superintendent/district file
  sups_url <- "https://doe.sd.gov/ofm/documents/1025-SupsAdmin.xlsx"
  sups_data <- download_directory_excel(sups_url, "superintendent", skip_rows = 5)
  message(paste("  Superintendent file:", nrow(sups_data), "districts"))

  # Join by district number
  # The superintendent file has district-level info that should be merged
  # into the school-level file
  sups_cols <- c("District Number", "District Website",
                 "Superintendent First Name", "Superintendent Last Name",
                 "Superintendent Phone Number")

  # Only select columns that exist in the sups data
  available_sups_cols <- sups_cols[sups_cols %in% names(sups_data)]
  sups_subset <- sups_data[, available_sups_cols, drop = FALSE]

  # Remove duplicate district rows (keep first occurrence)
  if ("District Number" %in% names(sups_subset)) {
    sups_subset <- sups_subset[!duplicated(sups_subset[["District Number"]]), ]
  }

  # Join on District Number
  if ("District Number" %in% names(principal_data) &&
      "District Number" %in% names(sups_subset)) {
    result <- merge(
      principal_data,
      sups_subset,
      by = "District Number",
      all.x = TRUE
    )
  } else {
    # If join column missing, return principal data as-is
    result <- principal_data
  }

  dplyr::as_tibble(result)
}


#' Download a directory Excel file from SD DOE
#'
#' Downloads and reads a directory Excel file from the SD DOE website.
#'
#' @param url Full URL to the Excel file
#' @param file_type Type identifier for error messages ("principal" or "superintendent")
#' @param skip_rows Number of header rows to skip
#' @return Data frame
#' @keywords internal
download_directory_excel <- function(url, file_type, skip_rows = 0) {

  tname <- tempfile(
    pattern = paste0("sd_dir_", file_type, "_"),
    tmpdir = tempdir(),
    fileext = ".xlsx"
  )

  tryCatch({
    response <- httr::GET(
      url,
      httr::write_disk(tname, overwrite = TRUE),
      httr::timeout(300),
      httr::config(connecttimeout = 60)
    )

    if (httr::http_error(response)) {
      stop(paste("HTTP error:", httr::status_code(response)))
    }

    # Check file size
    file_info <- file.info(tname)
    if (file_info$size < 500) {
      stop("Download failed - file too small, may be error page")
    }

  }, error = function(e) {
    stop(paste("Failed to download", file_type, "directory data from SD DOE:",
               e$message))
  })

  # Read the Excel file
  df <- tryCatch({
    readxl::read_xlsx(
      tname,
      skip = skip_rows,
      col_types = "text"
    )
  }, error = function(e) {
    stop(paste("Failed to read", file_type, "directory Excel file:",
               e$message))
  })

  # Clean up temp file
  unlink(tname)

  # Remove completely empty rows
  df <- df[rowSums(!is.na(df) & df != "") > 0, ]

  df
}


#' Process raw directory data to standard schema
#'
#' Takes raw combined directory data from get_raw_directory() and standardizes
#' column names, types, and format.
#'
#' @param raw_data Raw data frame from get_raw_directory()
#' @return Processed data frame with standard schema
#' @keywords internal
process_directory <- function(raw_data) {

  cols <- names(raw_data)

  # Helper to safely extract a column, trimming whitespace
  safe_col <- function(col_name) {
    if (col_name %in% cols) {
      trimws(raw_data[[col_name]])
    } else {
      rep(NA_character_, nrow(raw_data))
    }
  }

  # Build principal name from first + last
  principal_first <- safe_col("Principal First Name")
  principal_last <- safe_col("Principal Last Name")
  principal_name <- ifelse(
    is.na(principal_first) & is.na(principal_last),
    NA_character_,
    trimws(paste(
      ifelse(is.na(principal_first), "", principal_first),
      ifelse(is.na(principal_last), "", principal_last)
    ))
  )
  # Clean up empty strings
  principal_name[principal_name == ""] <- NA_character_

  # Build superintendent name from first + last
  supt_first <- safe_col("Superintendent First Name")
  supt_last <- safe_col("Superintendent Last Name")
  superintendent_name <- ifelse(
    is.na(supt_first) & is.na(supt_last),
    NA_character_,
    trimws(paste(
      ifelse(is.na(supt_first), "", supt_first),
      ifelse(is.na(supt_last), "", supt_last)
    ))
  )
  superintendent_name[superintendent_name == ""] <- NA_character_

  # Build result tibble
  result <- dplyr::tibble(
    state_district_id = safe_col("District Number"),
    state_school_id = safe_col("School Number"),
    district_name = safe_col("District Name"),
    school_name = safe_col("School Name"),
    entity_type = safe_col("District Type"),
    school_type = safe_col("School Type"),
    grades_served = safe_col("Grade Span"),
    address = safe_col("Physical Address"),
    city = safe_col("Physical Address City"),
    state = ifelse(
      !is.na(safe_col("Physical Address State")),
      safe_col("Physical Address State"),
      "SD"
    ),
    zip = safe_col("Physical Address Zip Code"),
    phone = safe_col("Principal Phone Number"),
    county_name = safe_col("Physical County"),
    principal_name = principal_name,
    principal_email = NA_character_,
    superintendent_name = superintendent_name,
    superintendent_email = NA_character_,
    website = safe_col("District Website")
  )

  result
}


# ==============================================================================
# Directory-specific cache functions
# ==============================================================================

#' Build cache file path for directory data
#'
#' @param cache_type Type of cache ("directory_tidy" or "directory_raw")
#' @return File path string
#' @keywords internal
build_cache_path_directory <- function(cache_type) {
  cache_dir <- get_cache_dir()
  file.path(cache_dir, paste0(cache_type, ".rds"))
}


#' Check if cached directory data exists
#'
#' @param cache_type Type of cache ("directory_tidy" or "directory_raw")
#' @param max_age Maximum age in days (default 7)
#' @return Logical indicating if valid cache exists
#' @keywords internal
cache_exists_directory <- function(cache_type, max_age = 7) {
  cache_path <- build_cache_path_directory(cache_type)

  if (!file.exists(cache_path)) {
    return(FALSE)
  }

  # Check age
  file_info <- file.info(cache_path)
  age_days <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "days"))

  age_days <= max_age
}


#' Read directory data from cache
#'
#' @param cache_type Type of cache ("directory_tidy" or "directory_raw")
#' @return Cached data frame
#' @keywords internal
read_cache_directory <- function(cache_type) {
  cache_path <- build_cache_path_directory(cache_type)
  readRDS(cache_path)
}


#' Write directory data to cache
#'
#' @param data Data frame to cache
#' @param cache_type Type of cache ("directory_tidy" or "directory_raw")
#' @return Invisibly returns the cache path
#' @keywords internal
write_cache_directory <- function(data, cache_type) {
  cache_path <- build_cache_path_directory(cache_type)
  cache_dir <- dirname(cache_path)

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  saveRDS(data, cache_path)
  invisible(cache_path)
}


#' Clear school directory cache
#'
#' Removes cached school directory data files.
#'
#' @return Invisibly returns the number of files removed
#' @export
#' @examples
#' \dontrun{
#' # Clear cached directory data
#' clear_directory_cache()
#' }
clear_directory_cache <- function() {
  cache_dir <- get_cache_dir()

  if (!dir.exists(cache_dir)) {
    message("Cache directory does not exist")
    return(invisible(0))
  }

  files <- list.files(cache_dir, pattern = "^directory_", full.names = TRUE)

  if (length(files) > 0) {
    file.remove(files)
    message(paste("Removed", length(files), "cached directory file(s)"))
  } else {
    message("No cached directory files to remove")
  }

  invisible(length(files))
}
