# sdschooldata

An R package for fetching, processing, and analyzing school enrollment data from South Dakota's Department of Education. It provides a programmatic interface to public school data, enabling researchers, analysts, and education policy professionals to easily access South Dakota public school data across the full historical record.

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("almartin82/sdschooldata")
```

## Quick Start

```r
library(sdschooldata)

# Get 2024 enrollment data (2023-24 school year)
enr_2024 <- fetch_enr(2024)

# View state totals
enr_2024 %>%
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")

# Get data for multiple years
enr_multi <- fetch_enr_multi(2020:2024)

# Track enrollment trends
enr_multi %>%
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, n_students)
```

## Data Availability

### Source Information

| Item | Value |
|------|-------|
| **State Agency** | South Dakota Department of Education (SD DOE) |
| **Primary Data Portal** | https://doe.sd.gov/ofm/enrollment.aspx |
| **Count Date** | Last Friday in September |
| **Data Release** | December (annually) |

### Historical Data Inventory

| Format Era | Years | File Format | URL Pattern | Key Differences |
|------------|-------|-------------|-------------|-----------------|
| Era 1 | 2011-2012 | Excel (.xlsx) | FE{YY}_Psum.xlsx | Simple district-level format |
| Era 2 | 2013-2020 | Excel (.xlsx) | Pubdsgr{YY}.xlsx / variations | Added school-level demographics |
| Era 3 | 2021-2025 | Excel (.xlsx) | Pubdisgr-{YYYY}.xlsx | Consistent 4-digit year format |

**Earliest available year**: 2011
**Most recent available year**: 2025
**Total years of data**: 15 years

### Identifier System

| Identifier | Format | Example | Notes |
|------------|--------|---------|-------|
| **District ID** | 5 digits | 49005 | County code (2) + district number (3) |
| **School ID** | 2 digits | 05 | Unique within district |
| **Campus ID** | 7 digits | 4900505 | District ID + School ID |

### What's Available

- **Years**: 2011 to 2025 (15 years)
- **Aggregation levels**: State, District, Campus (2013+)
- **Demographics**: White, Black, Hispanic, Asian, Native American, Pacific Islander, Multiracial
- **Gender**: Male, Female (2013+)
- **Grade levels**: PK, K, 1-12, Ungraded

### What's NOT Available

- Pre-2011 data (PDFs only, not machine-readable)
- Economic disadvantage status (not in public enrollment files)
- Special education enrollment (separate data system)
- English Learner counts (separate file, not integrated)
- Individual student records (aggregated only)

### Known Caveats

- **2011-2012**: Only district-level data available; campus-level demographics not included
- **File naming**: SD DOE uses inconsistent file naming conventions across years; this package handles the variations automatically
- **Tribal schools**: Tribal/BIE schools are reported separately from public schools
- **Non-public schools**: Available in separate files; this package focuses on public schools
- **Small counts**: Very small enrollments may be suppressed in some files

## Output Schema

### Wide Format (`tidy = FALSE`)

| Column | Type | Description |
|--------|------|-------------|
| end_year | integer | School year end (2024 = 2023-24 school year) |
| district_id | character | 5-digit state district identifier |
| campus_id | character | 7-digit campus identifier (NA for district rows) |
| district_name | character | District name |
| campus_name | character | Campus name (NA for district rows) |
| type | character | "State", "District", or "Campus" |
| row_total | integer | Total enrollment |
| white | integer | White student count |
| black | integer | Black/African American student count |
| hispanic | integer | Hispanic/Latino student count |
| asian | integer | Asian student count |
| native_american | integer | American Indian/Alaska Native count |
| pacific_islander | integer | Native Hawaiian/Pacific Islander count |
| multiracial | integer | Two or more races count |
| male | integer | Male student count |
| female | integer | Female student count |
| grade_pk | integer | Pre-K enrollment |
| grade_k | integer | Kindergarten enrollment |
| grade_01 through grade_12 | integer | Grade-level enrollment |
| grade_ug | integer | Ungraded enrollment |

### Tidy Format (`tidy = TRUE`, default)

| Column | Type | Description |
|--------|------|-------------|
| end_year | integer | School year end |
| district_id | character | District identifier |
| campus_id | character | Campus identifier |
| district_name | character | District name |
| campus_name | character | Campus name |
| type | character | Aggregation level |
| grade_level | character | "TOTAL", "PK", "K", "01"-"12", "UG" |
| subgroup | character | "total_enrollment", "white", "black", etc. |
| n_students | integer | Student count |
| pct | numeric | Percentage of total (0-1 scale) |
| is_state | logical | TRUE for state-level rows |
| is_district | logical | TRUE for district-level rows |
| is_campus | logical | TRUE for campus-level rows |
| is_public | logical | TRUE for public districts |

## Functions

### User-Facing Functions

- `fetch_enr(end_year, tidy = TRUE, use_cache = TRUE)` - Fetch enrollment data for a single year
- `fetch_enr_multi(end_years, tidy = TRUE, use_cache = TRUE)` - Fetch enrollment data for multiple years
- `tidy_enr(df)` - Transform wide data to tidy format
- `get_available_years()` - Get vector of available years
- `enr_grade_aggs(df)` - Create grade-level aggregates (K-8, HS, K-12)

### Caching Functions

- `cache_exists(end_year, type)` - Check if cached data exists
- `read_cache(end_year, type)` - Read data from cache
- `write_cache(df, end_year, type)` - Write data to cache
- `clear_cache(end_year = NULL, type = NULL)` - Clear cached data
- `cache_status()` - Show cache status

## Examples

### Enrollment Trends

```r
library(sdschooldata)
library(dplyr)
library(ggplot2)

# Get 5 years of data
enr <- fetch_enr_multi(2020:2024)

# State enrollment trend
enr %>%
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  ggplot(aes(x = end_year, y = n_students)) +
  geom_line() +
  geom_point() +
  labs(title = "South Dakota Total Enrollment",
       x = "School Year (End)",
       y = "Total Students")
```

### District Comparison

```r
# Largest districts by enrollment
enr_2024 <- fetch_enr(2024)

enr_2024 %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  arrange(desc(n_students)) %>%
  head(10) %>%
  select(district_name, n_students)
```

### Demographic Analysis

```r
# State demographic breakdown
enr_2024 %>%
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "native_american", "hispanic", "black", "asian")) %>%
  select(subgroup, n_students, pct) %>%
  arrange(desc(n_students))
```

## Related Packages

This package is part of a family of state school data packages:

- [caschooldata](https://github.com/almartin82/caschooldata) - California
- [ilschooldata](https://github.com/almartin82/ilschooldata) - Illinois
- [nyschooldata](https://github.com/almartin82/nyschooldata) - New York
- [ohschooldata](https://github.com/almartin82/ohschooldata) - Ohio
- [paschooldata](https://github.com/almartin82/paschooldata) - Pennsylvania
- [txschooldata](https://github.com/almartin82/txschooldata) - Texas

## License

MIT License

## Data Source

Data is sourced from the [South Dakota Department of Education](https://doe.sd.gov/ofm/enrollment.aspx). Enrollment counts reflect students enrolled on the last Friday in September.

For questions about the source data, contact Jake Cummings at the SD DOE: Jake.cummings@state.sd.us or 605-295-3322.
