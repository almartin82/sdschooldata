# Download an Excel file from SD DOE

Downloads and reads an Excel file from the SD DOE website.

## Usage

``` r
download_sd_excel(url, end_year, skip_rows = 0)
```

## Arguments

- url:

  Full URL to the Excel file

- end_year:

  School year end (for error messages)

- skip_rows:

  Number of header rows to skip

## Value

Data frame
