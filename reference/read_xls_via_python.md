# Read XLS file using Python xlrd

Fallback function for reading older XLS files that readxl cannot handle.
Requires Python with xlrd package installed.

## Usage

``` r
read_xls_via_python(filepath, skip_rows = 0)
```

## Arguments

- filepath:

  Path to the XLS file

- skip_rows:

  Number of rows to skip

## Value

Data frame with file contents
