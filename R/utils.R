# ==============================================================================
# Utility Functions
# ==============================================================================

#' @importFrom rlang .data
NULL


#' Convert to numeric, handling suppression markers
#'
#' SD DOE uses various markers for suppressed data (*, <5, N/A, etc.)
#' and may use commas in large numbers.
#'
#' @param x Vector to convert
#' @return Numeric vector with NA for non-numeric values
#' @keywords internal
safe_numeric <- function(x) {
  # Handle NULL or empty input
 if (is.null(x) || length(x) == 0) return(numeric(0))

  # If already numeric, return as-is
  if (is.numeric(x)) return(x)

  # Convert to character for processing
  x <- as.character(x)

  # Remove commas and whitespace
  x <- gsub(",", "", x)
  x <- trimws(x)

  # Handle common suppression markers
  x[x %in% c("*", ".", "-", "-1", "<5", "N/A", "NA", "", "n/a", "N/a")] <- NA_character_

  suppressWarnings(as.numeric(x))
}


#' Get available years for SD enrollment data
#'
#' Returns the range of years available for download from the
#' South Dakota Department of Education.
#'
#' @return Integer vector of available years
#' @export
#' @examples
#' get_available_years()
get_available_years <- function() {
  # SD DOE provides enrollment data from 2006-present in Excel format
  # Years 2009-2010: XLS format with full grade breakdown
  # Years 2007-2008: XLS format with summary totals only
  # Years 2006: XLS format with full grade breakdown
  # Years 2000-2005: Only PDF files available (not supported)
  # Data source: https://doe.sd.gov/ofm/enrollment.aspx
  2006:2025
}


#' Build SD DOE enrollment data URL
#'
#' Constructs the download URL for a specific year and file type.
#' URL patterns vary by year due to inconsistent naming conventions.
#'
#' @param end_year School year end (e.g., 2025 for 2024-25)
#' @param file_type Type of file: "district", "school_race", "school_gender", "school_grade"
#' @return URL string
#' @keywords internal
build_sd_url <- function(end_year, file_type = "district") {

  base_url <- "https://doe.sd.gov/ofm/documents/"

  # Construct filename based on year and type
 # Naming conventions have changed over time:
  # 2011: FE11_Psum.xlsx
  # 2012: FE12_Pscgr.xlsx
  # 2013-2017: Pubdsgr{YY}.xlsx (or variations like Pubdsgr17b.xlsx)
  # 2018: PSgrade-18a.xlsx
  # 2019-2020: Pubdisgr-{YY}.xlsx (or Pubdisgr-20f.xlsx)
  # 2021-2024: Pubdisgr-{YYYY}.xlsx

  yy <- end_year %% 100  # Two-digit year
  yyyy <- end_year       # Four-digit year

  if (file_type == "district") {
    filename <- get_district_filename(end_year)
  } else if (file_type == "school_race") {
    filename <- get_race_filename(end_year)
  } else if (file_type == "school_gender") {
    filename <- get_gender_filename(end_year)
  } else if (file_type == "school_grade") {
    filename <- get_grade_filename(end_year)
  } else {
    stop("Unknown file_type: ", file_type)
  }

  paste0(base_url, filename)
}


#' Get district enrollment filename for a given year
#'
#' @param end_year School year end
#' @return Filename string
#' @keywords internal
get_district_filename <- function(end_year) {

  yy <- sprintf("%02d", end_year %% 100)
  yyyy <- end_year

  # Handle naming convention changes
  # Era 0: 2006-2010 (XLS format with various naming conventions)
  if (end_year == 2006) {
    return("2006_PublicPk-12.xls")
  } else if (end_year == 2007) {
    return("Public_pk-12_totals07.xls")
  } else if (end_year == 2008) {
    return("WEBPublicbydistrictPK-12.xls")
  } else if (end_year == 2009) {
    return("Public_district.xls")
  } else if (end_year == 2010) {
    return("FE10_Psum.xls")
  } else if (end_year == 2011) {
    # Era 1: 2011-2012 (XLSX format)
    return("FE11_Psum.xlsx")
  } else if (end_year == 2012) {
    return("FE12_Pscgr.xlsx")
  } else if (end_year >= 2013 && end_year <= 2016) {
    # Era 2: 2013-2020
    return(paste0("Pubdsgr", yy, ".xlsx"))
  } else if (end_year == 2017) {
    return("Pubdsgr17b.xlsx")
  } else if (end_year == 2018) {
    return("PSgrade-18a.xlsx")
  } else if (end_year == 2019) {
    return("Pubdisgr-19.xlsx")
  } else if (end_year == 2020) {
    return("Pubdisgr-20f.xlsx")
  } else if (end_year == 2021) {
    # Era 3: 2021+ (Note: 2021 uses 2-digit year, 2022+ uses 4-digit)
    return("Pubdisgr-21.xlsx")
  } else if (end_year == 2022) {
    return("Pubdisgr-2022.xlsx")
  } else if (end_year == 2023) {
    return("Pubdisgr-23.xlsx")  # Note: Uses 2-digit year
  } else if (end_year >= 2024) {
    return(paste0("Pubdisgr-", yyyy, ".xlsx"))
  } else {
    stop("Year ", end_year, " not supported. Available years: 2006-2025")
  }
}


#' Get race/ethnicity enrollment filename for a given year
#'
#' @param end_year School year end
#' @return Filename string or NULL if not available
#' @keywords internal
get_race_filename <- function(end_year) {
  yy <- sprintf("%02d", end_year %% 100)
  yyyy <- end_year

  # Era 0: 2006-2010 (XLS format, varies by year)
  if (end_year == 2006) {
    return(NULL)  # Only PDF available for 2006
  } else if (end_year == 2007) {
    return("Public_FE_Ethnicity07.xls")
  } else if (end_year == 2008) {
    return("WEBPublicbySchoolbyEthnicitybyGrade.xls")
  } else if (end_year == 2009) {
    return("Public_ethnicity.xls")
  } else if (end_year == 2010) {
    return("FE10_Peth.xls")
  } else if (end_year == 2011) {
    # Era 1: 2011-2012
    return("FE11_Pethnicity.xlsx")
  } else if (end_year == 2012) {
    return("FE12_Pethnicity.xlsx")
  } else if (end_year >= 2013 && end_year <= 2017) {
    # Era 2: 2013-2020
    return(paste0("Pubscrce", yy, ".xlsx"))
  } else if (end_year == 2018) {
    return("Pschrce-18a.xlsx")  # Note: Different naming pattern
  } else if (end_year == 2019) {
    return("Pschrce-19.xlsx")   # Note: Different naming pattern
  } else if (end_year == 2020) {
    return("Pubschrce-20f.xlsx")
  } else if (end_year == 2021) {
    # Era 3: 2021+
    return("Pschrce-21a.xlsx")    # Note: Different naming pattern
  } else if (end_year == 2022) {
    return("Pubschrce-2022.xlsx")
  } else if (end_year == 2023) {
    return("Pubschrce-23b.xlsx")  # Note: Uses 2-digit year with suffix
  } else if (end_year >= 2024) {
    return(paste0("Pubschrce-", yyyy, ".xlsx"))
  } else {
    stop("Year ", end_year, " not supported for race data")
  }
}


#' Get gender enrollment filename for a given year
#'
#' @param end_year School year end
#' @return Filename string or NULL if not available
#' @keywords internal
get_gender_filename <- function(end_year) {
  yy <- sprintf("%02d", end_year %% 100)
  yyyy <- end_year

  # Era 0: 2006-2010 (XLS format, varies by year)
  if (end_year == 2006) {
    return(NULL)  # Only PDF available for 2006
  } else if (end_year == 2007) {
    return("Public_Gender_FE2007.xls")
  } else if (end_year == 2008) {
    return("WEBPublicbySchoolbyGenderbyGrade.xls")
  } else if (end_year == 2009) {
    return("Public_gender.xls")
  } else if (end_year == 2010) {
    return("FE10_Pgen.xls")
  } else if (end_year == 2011) {
    # Era 1: 2011-2012
    return("FE11_Pgender.xlsx")
  } else if (end_year == 2012) {
    return("FE12_Pgender.xlsx")
  } else if (end_year >= 2013 && end_year <= 2017) {
    # Era 2: 2013-2020
    return(paste0("Pubscgen", yy, ".xlsx"))
  } else if (end_year == 2018) {
    return("Pubgen-18a.xlsx")    # Note: Different naming pattern
  } else if (end_year == 2019) {
    return("Pschgen-19.xlsx")    # Note: Different naming pattern
  } else if (end_year == 2020) {
    return("Pubschgen-20f.xlsx")
  } else if (end_year == 2021) {
    # Era 3: 2021+
    return("Pschgen-21.xlsx")     # Note: Different naming pattern
  } else if (end_year == 2022) {
    return("Pubschgen-22.xlsx")   # Note: Uses 2-digit year
  } else if (end_year == 2023) {
    return("Pubschgen-23.xlsx")   # Note: Uses 2-digit year
  } else if (end_year == 2024) {
    return("Pubschgen-24.xlsx")
  } else if (end_year >= 2025) {
    return(paste0("Pubschgen-", yyyy, ".xlsx"))
  } else {
    stop("Year ", end_year, " not supported for gender data")
  }
}


#' Get school-level grade enrollment filename for a given year
#'
#' @param end_year School year end
#' @return Filename string
#' @keywords internal
get_grade_filename <- function(end_year) {
  yy <- sprintf("%02d", end_year %% 100)
  yyyy <- end_year

  if (end_year == 2011) {
    return("FE11_Pschgrade.xlsx")
  } else if (end_year == 2012) {
    return("FE12_Pschgrade.xlsx")
  } else if (end_year >= 2013 && end_year <= 2017) {
    return(paste0("Pubscgr", yy, ".xlsx"))
  } else if (end_year == 2018) {
    return("Pubscgrade-18.xlsx")
  } else if (end_year == 2019) {
    return("Pubschgr-19.xlsx")
  } else if (end_year == 2020) {
    return("Pubschgr-20f.xlsx")
  } else if (end_year >= 2021) {
    return(paste0("Pubschgr-", yyyy, ".xlsx"))
  } else {
    stop("Year ", end_year, " not supported for grade data")
  }
}
