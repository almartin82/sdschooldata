# Fetch South Dakota school directory data

Downloads and processes school directory data from the South Dakota
Department of Education. This joins two separate SD DOE files
(principal/school info and superintendent/district info) to create a
combined directory with both school-level and district-level contact
information.

## Usage

``` r
fetch_directory(end_year = NULL, tidy = TRUE, use_cache = TRUE)
```

## Arguments

- end_year:

  Currently unused. The directory data represents the current school
  year. Included for API consistency with other fetch functions.

- tidy:

  If TRUE (default), returns data in a standardized format with
  consistent column names. If FALSE, returns raw column names from SD
  DOE.

- use_cache:

  If TRUE (default), uses locally cached data when available. Set to
  FALSE to force re-download from SD DOE.

## Value

A tibble with school directory data. Columns include:

- `state_district_id`: District identifier (e.g., "06001")

- `state_school_id`: School identifier

- `district_name`: District name

- `school_name`: School name

- `entity_type`: District type description

- `school_type`: School type description

- `grades_served`: Grade span (e.g., "KG05", "0912")

- `address`: Physical street address

- `city`: Physical address city

- `state`: State (always "SD")

- `zip`: Physical address ZIP code

- `phone`: Principal phone number

- `county_name`: Physical county

- `principal_name`: Principal full name (first + last)

- `principal_email`: NA (not available from SD DOE)

- `superintendent_name`: Superintendent full name (first + last)

- `superintendent_email`: NA (not available from SD DOE)

- `website`: District website URL

## Examples

``` r
if (FALSE) { # \dontrun{
# Get school directory data
dir_data <- fetch_directory()

# Get raw format (original SD DOE column names)
dir_raw <- fetch_directory(tidy = FALSE)

# Force fresh download (ignore cache)
dir_fresh <- fetch_directory(use_cache = FALSE)

# Filter to specific district
library(dplyr)
sioux_falls <- dir_data |>
  filter(grepl("Sioux Falls", district_name))
} # }
```
