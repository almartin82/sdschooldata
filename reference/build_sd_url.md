# Build SD DOE enrollment data URL

Constructs the download URL for a specific year and file type. URL
patterns vary by year due to inconsistent naming conventions.

## Usage

``` r
build_sd_url(end_year, file_type = "district")
```

## Arguments

- end_year:

  School year end (e.g., 2025 for 2024-25)

- file_type:

  Type of file: "district", "school_race", "school_gender",
  "school_grade"

## Value

URL string
