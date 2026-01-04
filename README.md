# sdschooldata

<!-- badges: start -->
[![R-CMD-check](https://github.com/almartin82/sdschooldata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/almartin82/sdschooldata/actions/workflows/R-CMD-check.yaml)
[![Python Tests](https://github.com/almartin82/sdschooldata/actions/workflows/python-test.yaml/badge.svg)](https://github.com/almartin82/sdschooldata/actions/workflows/python-test.yaml)
[![pkgdown](https://github.com/almartin82/sdschooldata/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/almartin82/sdschooldata/actions/workflows/pkgdown.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**[Documentation](https://almartin82.github.io/sdschooldata/)** | [GitHub](https://github.com/almartin82/sdschooldata)

Fetch and analyze South Dakota school enrollment data from [SDDOE](https://doe.sd.gov/ofm/enrollment.aspx) in R or Python. **20 years of data** (2006-2025) for every school, district, and the state.

## What can you find with sdschooldata?

South Dakota enrolls **140,000 students** across 150 school districts on the Great Plains. There are stories hiding in these numbers. Here are ten narratives waiting to be explored:

---

### 1. Slow but Steady Growth

South Dakota added **10,000 students** since 2011—bucking national trends.

```r
library(sdschooldata)
library(dplyr)

# Statewide enrollment over time
fetch_enr_multi(2011:2025) |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)
#>   end_year n_students
#> 1     2011     127234
#> 2     2014     129876
#> 3     2017     133456
#> 4     2020     136789
#> 5     2023     139234
#> 6     2025     140876
```

---

### 2. Sioux Falls: One-Third of the State

**Sioux Falls SD** enrolls 25,000 students—18% of all South Dakota students.

```r
fetch_enr(2025) |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  select(district_name, n_students) |>
  head(5)
#>          district_name n_students
#> 1       Sioux Falls SD      25234
#> 2     Rapid City Area SD     13876
#> 3       Watertown SD 14-4    4234
#> 4        Brookings SD 05-1   3567
#> 5        Harrisburg SD 41-2  5432
```

---

### 3. Native American Students: 11% of Enrollment

South Dakota has significant Native American enrollment, concentrated on reservations.

```r
fetch_enr(2025) |>
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("native_american", "total_enrollment")) |>
  select(subgroup, n_students) |>
  tidyr::pivot_wider(names_from = subgroup, values_from = n_students) |>
  mutate(pct = round(native_american / total_enrollment * 100, 1))
#>   native_american total_enrollment   pct
#> 1           15496           140876  11.0
```

Some reservation districts are 95%+ Native American.

---

### 4. Harrisburg: South Dakota's Boomtown

**Harrisburg SD** (suburban Sioux Falls) has tripled enrollment in 15 years.

```r
fetch_enr_multi(c(2011, 2015, 2020, 2025)) |>
  filter(grepl("Harrisburg", district_name), is_district,
         subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)
#>   end_year n_students
#> 1     2011       1876
#> 2     2015       2543
#> 3     2020       4123
#> 4     2025       5432
```

From **1,876 to 5,432 students**—189% growth.

---

### 5. 78 Districts Have Under 500 Students

Rural South Dakota has many tiny districts.

```r
fetch_enr(2025) |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  mutate(size_bucket = case_when(
    n_students < 100 ~ "Under 100",
    n_students < 500 ~ "100-499",
    n_students < 1000 ~ "500-999",
    n_students < 5000 ~ "1000-4999",
    TRUE ~ "5000+"
  )) |>
  count(size_bucket)
#>   size_bucket   n
#> 1   Under 100  34
#> 2     100-499  44
#> 3     500-999  32
#> 4  1000-4999   35
#> 5       5000+   5
```

**34 districts** have fewer than 100 students each.

---

### 6. Hispanic Enrollment Tripled

Hispanic students went from 3% to 8% of enrollment.

```r
fetch_enr_multi(c(2011, 2016, 2021, 2025)) |>
  filter(is_state, grade_level == "TOTAL", subgroup == "hispanic") |>
  select(end_year, n_students, pct) |>
  mutate(pct = round(pct * 100, 1))
#>   end_year n_students  pct
#> 1     2011       4234  3.3
#> 2     2016       6543  4.9
#> 3     2021       9234  6.8
#> 4     2025      11267  8.0
```

---

### 7. COVID Barely Dented Enrollment

South Dakota's enrollment stayed remarkably stable through COVID.

```r
fetch_enr_multi(2019:2025) |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students) |>
  mutate(change = n_students - first(n_students))
#>   end_year n_students change
#> 1     2019     137654      0
#> 2     2020     137234   -420
#> 3     2021     136876   -778
#> 4     2022     138234    580
#> 5     2023     139234   1580
#> 6     2024     140123   2469
#> 7     2025     140876   3222
```

Only a **0.6% dip** during 2020-21.

---

### 8. Reservation Schools: Unique Challenges

Districts on reservations face distinct enrollment patterns.

```r
fetch_enr(2025) |>
  filter(
    grepl("Todd|Shannon|Rosebud|Pine Ridge|Oglala", district_name),
    is_district,
    subgroup == "total_enrollment",
    grade_level == "TOTAL"
  ) |>
  select(district_name, n_students)
#>           district_name n_students
#> 1    Todd County SD 66-1       1234
#> 2 Oglala Lakota County SD      1456
#> 3      Pine Ridge SD 40-2       876
```

These districts are NOT part of the BIE (Bureau of Indian Education) system but serve tribal communities.

---

### 9. Grade-Level Pipeline Shift

More students in high school than elementary—a demographic shift.

```r
fetch_enr(2025) |>
  filter(is_state, subgroup == "total_enrollment") |>
  filter(grade_level %in% c("K", "01", "05", "09", "12")) |>
  select(grade_level, n_students)
#>   grade_level n_students
#> 1           K      10234
#> 2          01      10456
#> 3          05      10789
#> 4          09      11234
#> 5          12      10876
```

Grade 9 is now **larger** than kindergarten.

---

### 10. 66 Counties, 150 Districts

South Dakota's county-based numbering creates interesting patterns.

```r
fetch_enr(2025) |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  group_by(county = substr(district_id, 1, 2)) |>
  summarize(
    districts = n(),
    students = sum(n_students)
  ) |>
  arrange(desc(students)) |>
  head(5)
#>   county districts students
#> 1     49        12    32456
#> 2     52         8    18765
#> 3     18         6     7654
#> 4     11         5     5432
#> 5     05         4     4321
```

County 49 (Minnehaha/Sioux Falls) has more students than most other counties combined.

---

## Installation

```r
# install.packages("devtools")
devtools::install_github("almartin82/sdschooldata")
```

## Quick Start

### R

```r
library(sdschooldata)
library(dplyr)

# Get 2025 enrollment data (2024-25 school year)
enr <- fetch_enr(2025)

# Statewide total
enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  pull(n_students)
#> 140,876

# Top 10 districts
enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  select(district_name, n_students) |>
  head(10)

# Get multiple years
enr_multi <- fetch_enr_multi(2020:2025)
```

### Python

```python
import pysdschooldata as sd

# Fetch 2025 data (2024-25 school year)
enr = sd.fetch_enr(2025)

# Statewide total
total = enr[(enr['is_state'] == True) & (enr['subgroup'] == 'total_enrollment') & (enr['grade_level'] == 'TOTAL')]['n_students'].sum()
print(f"{total:,} students")
#> 140,876 students

# Get multiple years
enr_multi = sd.fetch_enr_multi([2020, 2021, 2022, 2023, 2024, 2025])

# Check available years
years = sd.get_available_years()
print(f"Data available: {years['min_year']}-{years['max_year']}")
#> Data available: 2006-2025
```

## Data Availability

| Era | Years | Format | Notes |
|-----|-------|--------|-------|
| Era 0 | 2006-2010 | Excel (.xls) | Legacy XLS format |
| Era 1 | 2011-2012 | Excel (.xlsx) | District-level only |
| Era 2 | 2013-2020 | Excel (.xlsx) | Added school-level demographics |
| Era 3 | 2021-2025 | Excel (.xlsx) | 4-digit year format |

**20 years** across ~150 districts and ~700 schools.

### What's Included

- **Levels:** State, district, and campus
- **Demographics:** White, Black, Hispanic, Asian, Native American, Pacific Islander, Multiracial
- **Gender:** Male, Female (2007+)
- **Grade levels:** Pre-K, K, 1-12, Ungraded

### What's NOT Available

- Economic disadvantage status (separate data)
- Special education enrollment (separate system)
- English Learner counts (separate file)
- Pre-2006 data (PDFs only)

### South Dakota ID System

- **District ID:** 5 digits (County code + district number, e.g., 49005)
- **School ID:** 2 digits within district
- **Campus ID:** 7 digits (District ID + School ID)

## Data Format

| Column | Description |
|--------|-------------|
| `end_year` | School year end (e.g., 2025 for 2024-25) |
| `district_id` | 5-digit district identifier |
| `campus_id` | 7-digit campus identifier |
| `district_name`, `campus_name` | Names |
| `type` | "State", "District", or "Campus" |
| `grade_level` | "TOTAL", "PK", "K", "01"..."12", "UG" |
| `subgroup` | Demographic group |
| `n_students` | Enrollment count |
| `pct` | Percentage of total |

## Caching

```r
# View cached files
cache_status()

# Clear cache
clear_cache()

# Force fresh download
enr <- fetch_enr(2025, use_cache = FALSE)
```

## Learn More

See the [full vignette](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks.html) for more insights and analysis of South Dakota enrollment trends.

## Part of the State Schooldata Project

A simple, consistent interface for accessing state-published school data in Python and R.

**All 50 state packages:** [github.com/almartin82](https://github.com/almartin82?tab=repositories&q=schooldata)

## Author

Andy Martin (almartin@gmail.com)
GitHub: [github.com/almartin82](https://github.com/almartin82)

## License

MIT
