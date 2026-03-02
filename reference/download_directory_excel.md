# Download a directory Excel file from SD DOE

Downloads and reads a directory Excel file from the SD DOE website.

## Usage

``` r
download_directory_excel(url, file_type, skip_rows = 0)
```

## Arguments

- url:

  Full URL to the Excel file

- file_type:

  Type identifier for error messages ("principal" or "superintendent")

- skip_rows:

  Number of header rows to skip

## Value

Data frame
