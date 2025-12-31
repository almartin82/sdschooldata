# Process raw SD DOE enrollment data

Transforms raw Excel data into a standardized schema combining district
and campus data.

## Usage

``` r
process_enr(raw_data, end_year)
```

## Arguments

- raw_data:

  List containing district and campus data frames from get_raw_enr

- end_year:

  School year end

## Value

Processed data frame with standardized columns
