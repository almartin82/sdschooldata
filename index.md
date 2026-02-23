# sdschooldata

**[Documentation](https://almartin82.github.io/sdschooldata/)** \|
[GitHub](https://github.com/almartin82/sdschooldata)

Fetch and analyze South Dakota school enrollment data from
[SDDOE](https://doe.sd.gov/ofm/enrollment.aspx) in R or Python. **20
years of data** (2006-2025) for every school, district, and the state.

## Why sdschooldata?

South Dakota enrolls students across 150 school districts on the Great
Plains. With large reservation lands, a significant Native American
population, and suburban growth around Sioux Falls, the Mount Rushmore
State has unique educational dynamics worth exploring.

This package is part of the
[njschooldata](https://github.com/almartin82/njschooldata) family of
state education data packages, bringing the same simple, consistent
interface to South Dakota school data.

------------------------------------------------------------------------

## Installation

### R

``` r
# install.packages("devtools")
devtools::install_github("almartin82/sdschooldata")
```

### Python

``` bash
pip install git+https://github.com/almartin82/sdschooldata.git#subdirectory=pysdschooldata
```

------------------------------------------------------------------------

## Quick Start

### R

``` r
library(sdschooldata)
library(dplyr)

# Get 2025 enrollment data (2024-25 school year)
enr <- fetch_enr(2025)

# Statewide total
enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  pull(n_students)
#> 138,861

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

``` python
import pysdschooldata as sd

# Fetch 2025 data (2024-25 school year)
enr = sd.fetch_enr(2025)

# Statewide total
total = enr[(enr['is_state'] == True) & (enr['subgroup'] == 'total_enrollment') & (enr['grade_level'] == 'TOTAL')]['n_students'].sum()
print(f"{total:,} students")
#> 138,861 students

# Get multiple years
enr_multi = sd.fetch_enr_multi([2020, 2021, 2022, 2023, 2024, 2025])

# Check available years
years = sd.get_available_years()
print(f"Data available: {years['min_year']}-{years['max_year']}")
#> Data available: 2006-2025
```

------------------------------------------------------------------------

## What can you find with sdschooldata?

Explore enrollment trends, demographic patterns, and regional
differences across 20 years of data (2006-2025). For detailed analysis,
see the [enrollment hooks
vignette](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks.html).

------------------------------------------------------------------------

## 1. South Dakota enrollment peaked in 2022 and is now declining

South Dakota’s public school enrollment grew steadily from 2015 to 2022,
peaking at 141,429, but has dropped 1.8% since then – losing 2,568
students in three years.

``` r
library(sdschooldata)
library(dplyr)
library(tidyr)
library(ggplot2)

theme_set(theme_minimal(base_size = 14))

enr <- fetch_enr_multi(c(2015:2020, 2022:2025), use_cache = TRUE)

state_totals <- enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students) |>
  mutate(change = n_students - lag(n_students),
         pct_change = round(change / lag(n_students) * 100, 2))

stopifnot(nrow(state_totals) > 0)
state_totals
#>    end_year n_students change pct_change
#> 1      2015     134054     NA         NA
#> 2      2016     135811   1757       1.31
#> 3      2017     137251   1440       1.06
#> 4      2018     138428   1177       0.86
#> 5      2019     139442   1014       0.73
#> 6      2020     139154   -288      -0.21
#> 7      2022     141429   2275       1.63
#> 8      2023     141005   -424      -0.30
#> 9      2024     140587   -418      -0.30
#> 10     2025     138861  -1726      -1.23
```

![Statewide enrollment
trend](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks_files/figure-html/statewide-chart-1.png)

Statewide enrollment trend

------------------------------------------------------------------------

## 2. Sioux Falls dominates the state

The Sioux Falls School District is by far the largest in the state, with
more students than the next several districts combined. Rapid City is a
distant second.

``` r
enr_2025 <- fetch_enr(2025, use_cache = TRUE)

top_10 <- enr_2025 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  head(10) |>
  select(district_name, n_students)

stopifnot(nrow(top_10) > 0)
top_10
#>           district_name n_students
#> 1      Sioux Falls 49-5      24841
#> 2  Rapid City Area 51-4      12040
#> 3       Harrisburg 41-2       6398
#> 4   Brandon Valley 49-2       5206
#> 5         Aberdeen 06-1       4134
#> 6        Brookings 05-1       3483
#> 7        Watertown 14-4       3425
#> 8            Huron 02-2       3042
#> 9          Yankton 63-3       2973
#> 10           Meade 46-1       2957
```

![Top 10
districts](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks_files/figure-html/top-districts-chart-1.png)

Top 10 districts

------------------------------------------------------------------------

## 3. Native American students are a significant population

South Dakota has one of the highest percentages of Native American
students in the nation, reflecting the state’s large reservation lands
including Pine Ridge, Rosebud, and Standing Rock. Demographic data is
reported at the campus level; here we aggregate across all campuses
statewide.

``` r
state_total <- enr_2025 |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  pull(n_students)

demographics <- enr_2025 |>
  filter(is_campus, grade_level == "TOTAL",
         subgroup %in% c("white", "native_american", "hispanic", "black", "asian", "multiracial")) |>
  group_by(subgroup) |>
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop") |>
  mutate(pct = round(n_students / state_total * 100, 1)) |>
  arrange(desc(n_students))

stopifnot(nrow(demographics) > 0)
demographics
#> # A tibble: 6 x 3
#>   subgroup        n_students   pct
#>   <chr>                <dbl> <dbl>
#> 1 white                95447  68.7
#> 2 native_american      14283  10.3
#> 3 hispanic             12845   9.3
#> 4 multiracial           8681   6.3
#> 5 black                 5051   3.6
#> 6 asian                 2308   1.7
```

![Demographics
chart](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks_files/figure-html/demographics-chart-1.png)

Demographics chart

------------------------------------------------------------------------

## 4. Sioux Falls grows while Rapid City and rural hubs decline

The state’s largest district continues to grow, but Rapid City lost 12%
of its enrollment since 2015 and other regional hubs are shrinking.
District names vary across years in SD data, so we use district_id to
track districts consistently.

``` r
urban_growth <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Sioux Falls|Rapid City|Aberdeen|Brookings|Watertown", district_name)) |>
  group_by(district_id) |>
  arrange(end_year) |>
  summarize(
    district_name = last(district_name),
    y2015 = n_students[end_year == min(end_year)],
    y2025 = n_students[end_year == max(end_year)],
    pct_change = round((y2025 / y2015 - 1) * 100, 1),
    .groups = "drop"
  ) |>
  arrange(desc(pct_change))

stopifnot(nrow(urban_growth) > 0)
urban_growth
#> # A tibble: 5 x 5
#>   district_id district_name        y2015 y2025 pct_change
#>   <chr>       <chr>                <dbl> <dbl>      <dbl>
#> 1 05001       Brookings 05-1        3351  3483        3.9
#> 2 49005       Sioux Falls 49-5     24216 24841        2.6
#> 3 06001       Aberdeen 06-1         4485  4134       -7.8
#> 4 51004       Rapid City Area 51-4 13743 12040      -12.4
#> 5 14004       Watertown 14-4        4016  3425      -14.7
```

![Urban growth
chart](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks_files/figure-html/regional-chart-1.png)

Urban growth chart

------------------------------------------------------------------------

## 5. Many tiny rural districts

South Dakota has a large number of very small school districts, many
with fewer than 200 students, reflecting the state’s rural character and
sparse population.

``` r
small <- enr_2025 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  filter(n_students < 200) |>
  arrange(n_students) |>
  head(15) |>
  select(district_name, n_students)

stopifnot(nrow(small) > 0)
small
#>         district_name n_students
#> 1   Elk Mountain 16-2         20
#> 2         Bowdle 22-1         45
#> 3  South Central 26-5         52
#> 4          Hoven 53-2        101
#> 5       Edgemont 23-1        106
#> 6       Oelrichs 23-3        117
#> 7          Bison 52-1        118
#> 8     White Lake 01-3        122
#> 9       McIntosh 15-1        141
#> 10        Doland 56-2        146
#> 11        Colome 59-3        153
#> 12       Herreid 10-1        153
#> 13         Henry 14-2        154
#> 14       Wakpala 15-3        159
#> 15 Tripp-Delmont 33-5        160
```

------------------------------------------------------------------------

## 6. Hispanic enrollment is rising fast

Hispanic students are the fastest-growing demographic group in South
Dakota schools, climbing from 7.97% to 9.25% of statewide enrollment in
just four years (2022-2025). Campus-level demographic data is available
starting in 2022.

``` r
hispanic_years <- c(2022:2025)
enr_hispanic <- fetch_enr_multi(hispanic_years, use_cache = TRUE)

hispanic_trend <- enr_hispanic |>
  filter(is_campus, subgroup == "hispanic", grade_level == "TOTAL") |>
  group_by(end_year) |>
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop")

state_totals_hisp <- enr_hispanic |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, total = n_students)

hispanic_trend <- hispanic_trend |>
  left_join(state_totals_hisp, by = "end_year") |>
  mutate(pct = round(n_students / total * 100, 2)) |>
  select(end_year, n_students, pct)

stopifnot(nrow(hispanic_trend) > 0)
hispanic_trend
#> # A tibble: 4 x 3
#>   end_year n_students   pct
#>      <int>      <dbl> <dbl>
#> 1     2022      11265  7.97
#> 2     2023      11983  8.50
#> 3     2024      12751  9.07
#> 4     2025      12845  9.25
```

![Hispanic growth
chart](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks_files/figure-html/growth-chart-1.png)

Hispanic growth chart

------------------------------------------------------------------------

## 7. Sioux Falls suburban growth outpaces the city

Harrisburg and Tea Area have seen explosive growth as Sioux Falls
suburbs boom, with Harrisburg more than doubling its enrollment from
2,724 to 6,398 students in just 15 years.

``` r
suburbs <- fetch_enr_multi(c(2011, 2015, 2020, 2025), use_cache = TRUE)

suburb_trend <- suburbs |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Harrisburg|Tea Area|Brandon Valley", district_name)) |>
  select(end_year, district_name, n_students)

stopifnot(nrow(suburb_trend) > 0)
suburb_trend
#>    end_year                 district_name n_students
#> 1      2011           Brandon Valley 49-2       3364
#> 2      2011               Harrisburg 41-2       2724
#> 3      2011 Tea Area School District 41-5       1383
#> 4      2015               Harrisburg 41-2       3900
#> 5      2015                 Tea Area 41-5       1610
#> 6      2015           Brandon Valley 49-2       3750
#> 7      2020           Brandon Valley 49-2       4682
#> 8      2020               Harrisburg 41-2       5449
#> 9      2020                 Tea Area 41-5       2045
#> 10     2025           Brandon Valley 49-2       5206
#> 11     2025               Harrisburg 41-2       6398
#> 12     2025                 Tea Area 41-5       2514
```

![Suburban growth
chart](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks_files/figure-html/suburban-growth-chart-1.png)

Suburban growth chart

------------------------------------------------------------------------

## 8. Rapid City: West River anchor

Rapid City Area School District anchors western South Dakota, serving as
the only major urban district west of the Missouri River.

``` r
rapid <- fetch_enr_multi(c(2015:2020, 2022:2025), use_cache = TRUE)

rapid_trend <- rapid |>
  filter(is_district, grepl("Rapid City", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)

stopifnot(nrow(rapid_trend) > 0)
rapid_trend
#>    end_year n_students
#> 1      2015      13743
#> 2      2016      13743
#> 3      2017      13760
#> 4      2018      13832
#> 5      2019      13609
#> 6      2020      12809
#> 7      2022      12743
#> 8      2023      12433
#> 9      2024      12313
#> 10     2025      12040
```

![Rapid City
chart](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks_files/figure-html/rapid-city-chart-1.png)

Rapid City chart

------------------------------------------------------------------------

## 9. Reservation schools: Todd County and Pine Ridge

Districts serving reservation communities face unique challenges. Todd
County (Rosebud) and Oglala Lakota County (Pine Ridge) serve
predominantly Native American students.

``` r
reservation <- fetch_enr_multi(c(2015, 2020, 2025), use_cache = TRUE)

res_data <- reservation |>
  filter(is_district,
         grepl("Todd County|Oglala Lakota|Shannon", district_name),
         subgroup == "total_enrollment",
         grade_level == "TOTAL")

stopifnot(nrow(res_data) > 0)
res_data
#>   end_year     type district_id campus_id             district_name campus_name
#> 1     2015 District       65001      <NA> Oglala Lakota County 65-1        <NA>
#> 2     2015 District       66001      <NA>          Todd County 66-1        <NA>
#> 3     2020 District       65001      <NA>        Oglala Lakota 65-1        <NA>
#> 4     2020 District       66001      <NA>          Todd County 66-1        <NA>
#> 5     2025 District       65001      <NA> Oglala Lakota County 65-1        <NA>
#> 6     2025 District       66001      <NA>          Todd County 66-1        <NA>
#>   grade_level         subgroup n_students pct is_state is_district is_campus
#> 1       TOTAL total_enrollment       1532   1    FALSE        TRUE     FALSE
#> 2       TOTAL total_enrollment       2013   1    FALSE        TRUE     FALSE
#> 3       TOTAL total_enrollment       1811   1    FALSE        TRUE     FALSE
#> 4       TOTAL total_enrollment       2156   1    FALSE        TRUE     FALSE
#> 5       TOTAL total_enrollment       1706   1    FALSE        TRUE     FALSE
#> 6       TOTAL total_enrollment       1956   1    FALSE        TRUE     FALSE
#>   is_public district_type_code district_type_name
#> 1      TRUE               <NA>               <NA>
#> 2      TRUE               <NA>               <NA>
#> 3      TRUE               <NA>               <NA>
#> 4      TRUE               <NA>               <NA>
#> 5      TRUE                 10        10 - Public
#> 6      TRUE                 10        10 - Public
```

![Reservation schools
chart](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks_files/figure-html/reservation-chart-1.png)

Reservation schools chart

------------------------------------------------------------------------

## 10. Rural consolidation pressure

Many of South Dakota’s smallest districts face consolidation pressure.
Districts with fewer than 100 students struggle with economies of scale.

``` r
tiny <- fetch_enr(2025, use_cache = TRUE)

tiny_districts <- tiny |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         n_students < 100) |>
  arrange(n_students) |>
  head(20) |>
  select(district_name, n_students)

stopifnot(nrow(tiny_districts) > 0)
tiny_districts
#>        district_name n_students
#> 1  Elk Mountain 16-2         20
#> 2        Bowdle 22-1         45
#> 3 South Central 26-5         52
```

![Rural distribution
chart](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks_files/figure-html/rural-distribution-chart-1.png)

Rural distribution chart

------------------------------------------------------------------------

## 11. High school enrollment surging while elementary stays flat

South Dakota’s high school enrollment grew 12% from 2015 to 2025, while
K-8 enrollment stayed essentially flat. This reflects larger birth
cohorts aging into upper grades.

``` r
hs_elem <- enr |>
  filter(is_state, subgroup == "total_enrollment",
         grade_level %in% c("K", paste0("0", 1:8), "09", "10", "11", "12")) |>
  mutate(level = ifelse(grade_level %in% c("09", "10", "11", "12"), "High School", "K-8")) |>
  group_by(end_year, level) |>
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop")

stopifnot(nrow(hs_elem) > 0)
hs_elem
#> # A tibble: 20 x 3
#>    end_year level       n_students
#>       <int> <chr>            <dbl>
#>  1     2015 High School      37100
#>  2     2015 K-8              93836
#>  3     2016 High School      37306
#>  4     2016 K-8              95214
#>  5     2017 High School      37625
#>  6     2017 K-8              96236
#>  7     2018 High School      37972
#>  8     2018 K-8              97021
#>  9     2019 High School      38825
#>  10    2019 K-8              97308
#>  11    2020 High School      40303
#>  12    2020 K-8              95681
#>  13    2022 High School      41804
#>  14    2022 K-8              96271
#>  15    2023 High School      42063
#>  16    2023 K-8              95696
#>  17    2024 High School      42133
#>  18    2024 K-8              95180
#>  19    2025 High School      41507
#>  20    2025 K-8              94070
```

![K-8 vs High School
chart](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks_files/figure-html/hs-vs-elem-chart-1.png)

K-8 vs High School chart

------------------------------------------------------------------------

## 12. The Black Hills corridor

The Black Hills region forms a distinct educational corridor, with Rapid
City at its center and smaller communities like Spearfish, Custer, and
Belle Fourche serving surrounding areas.

``` r
black_hills <- fetch_enr(2025, use_cache = TRUE)

bh_districts <- black_hills |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Rapid City|Spearfish|Sturgis|Custer|Lead|Deadwood|Belle Fourche", district_name)) |>
  arrange(desc(n_students)) |>
  select(district_name, n_students)

stopifnot(nrow(bh_districts) > 0)
bh_districts
#>          district_name n_students
#> 1 Rapid City Area 51-4      12040
#> 2       Spearfish 40-2       2301
#> 3   Belle Fourche 09-1       1241
#> 4          Custer 16-1        854
#> 5   Lead-Deadwood 40-1        590
```

![Black Hills
chart](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks_files/figure-html/black-hills-chart-1.png)

Black Hills chart

------------------------------------------------------------------------

## 13. Aberdeen and the northeast

Aberdeen School District anchors the northeast, with surrounding
agricultural communities feeding into regional schools.

``` r
northeast <- fetch_enr_multi(c(2015:2020, 2022:2025), use_cache = TRUE)

ne_trend <- northeast |>
  filter(is_district, grepl("Aberdeen", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)

stopifnot(nrow(ne_trend) > 0)
ne_trend
#>    end_year n_students
#> 1      2015       4485
#> 2      2016       4554
#> 3      2017       4517
#> 4      2018       4471
#> 5      2019       4483
#> 6      2020       4477
#> 7      2022       4326
#> 8      2023       4265
#> 9      2024       4237
#> 10     2025       4134
```

![Northeast
chart](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks_files/figure-html/northeast-chart-1.png)

Northeast chart

------------------------------------------------------------------------

## 14. Pre-K enrollment patterns

Pre-kindergarten enrollment varies significantly across districts,
reflecting different local policies and access to early childhood
programs.

``` r
prek <- fetch_enr(2025, use_cache = TRUE)

prek_data <- prek |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "PK") |>
  filter(n_students > 0) |>
  arrange(desc(n_students)) |>
  head(15) |>
  select(district_name, n_students)

stopifnot(nrow(prek_data) > 0)
prek_data
#>                district_name n_students
#> 1           Sioux Falls 49-5        791
#> 2               Yankton 63-3        196
#> 3       Rapid City Area 51-4        165
#> 4      Wagner Community 11-4        105
#> 5  Oglala Lakota County 65-1         79
#> 6                Lennox 41-4         59
#> 7             Watertown 14-4         59
#> 8                Hamlin 28-3         55
#> 9               Douglas 51-1         53
#> 10       Brandon Valley 49-2         51
#> 11           Harrisburg 41-2         43
#> 12       McCook Central 43-7         43
#> 13            Brookings 05-1         40
#> 14      Alcester-Hudson 61-1         37
#> 15            Garretson 49-4         36
```

![Pre-K
chart](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks_files/figure-html/prek-chart-1.png)

Pre-K chart

------------------------------------------------------------------------

## 15. Multiracial students: South Dakota’s growing diversity

Multiracial students grew from 8,129 (5.75%) to 8,681 (6.25%) of
statewide enrollment between 2022 and 2025, reflecting changing family
patterns statewide. Campus-level demographic data is available starting
in 2022.

``` r
multi <- fetch_enr_multi(c(2022, 2023, 2024, 2025), use_cache = TRUE)

multi_trend <- multi |>
  filter(is_campus, subgroup == "multiracial", grade_level == "TOTAL") |>
  group_by(end_year) |>
  summarize(n_students = sum(n_students, na.rm = TRUE), .groups = "drop")

state_totals_multi <- multi |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, total = n_students)

multi_trend <- multi_trend |>
  left_join(state_totals_multi, by = "end_year") |>
  mutate(pct = round(n_students / total * 100, 2)) |>
  select(end_year, n_students, pct)

stopifnot(nrow(multi_trend) > 0)
multi_trend
#> # A tibble: 4 x 3
#>   end_year n_students   pct
#>      <int>      <dbl> <dbl>
#> 1     2022       8129  5.75
#> 2     2023       8370  5.94
#> 3     2024       8576  6.10
#> 4     2025       8681  6.25
```

![Multiracial
chart](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks_files/figure-html/multiracial-chart-1.png)

Multiracial chart

------------------------------------------------------------------------

## Summary

South Dakota’s school enrollment data reveals:

- **Post-peak decline**: Enrollment peaked at 141,429 in 2022 and is now
  dropping
- **Urban concentration**: Sioux Falls and Rapid City dominate
  enrollment
- **Native American presence**: Significant Native American student
  population
- **Suburban boom**: Harrisburg and Tea Area growing rapidly
- **Reservation challenges**: Todd and Oglala Lakota counties serve
  unique populations
- **Rural struggles**: Many tiny districts face consolidation pressure
- **Demographic change**: Hispanic and multiracial enrollment growing
  steadily
- **High school surge**: HS enrollment grew 12% while K-8 stayed flat
- **Regional hubs**: Black Hills and northeast districts anchor their
  regions
- **Early childhood**: Pre-K access varies significantly across
  districts

------------------------------------------------------------------------

## Data Notes

### Data Source

All data comes from the South Dakota Department of Education [Fall
Census](https://doe.sd.gov/ofm/enrollment.aspx).

### Available Years

**2006-2025** (20 years)

| Era   | Years     | Format        | Notes                           |
|-------|-----------|---------------|---------------------------------|
| Era 0 | 2006-2010 | Excel (.xls)  | Legacy XLS format               |
| Era 1 | 2011-2012 | Excel (.xlsx) | District-level only             |
| Era 2 | 2013-2020 | Excel (.xlsx) | Added school-level demographics |
| Era 3 | 2021-2025 | Excel (.xlsx) | 4-digit year format             |

### Census Day

South Dakota collects enrollment data on **Census Day** (typically the
last Friday in September).

### Suppression Rules

- Small counts may be suppressed to protect student privacy
- Suppressed values appear as NA in the data
- Check for NA values when analyzing small subgroups or small districts

### South Dakota ID System

- **District ID:** 5 digits (County code + district number, e.g., 49005)
- **School ID:** 2 digits within district
- **Campus ID:** 7 digits (District ID + School ID)

### What’s Included

- **Levels:** State, district, and campus
- **Demographics:** White, Black, Hispanic, Asian, Native American,
  Pacific Islander, Multiracial (campus-level only, 2022+)
- **Grade levels:** Pre-K, K, 1-12, Ungraded
- **Note:** Demographic subgroups are reported at the campus level and
  must be aggregated for district/state totals

### What’s NOT Available

- Gender (male/female) data
- Economic disadvantage status (separate data)
- Special education enrollment (separate system)
- English Learner counts (separate file)
- Pre-2006 data (PDFs only)

------------------------------------------------------------------------

## Data Format

| Column                         | Description                              |
|--------------------------------|------------------------------------------|
| `end_year`                     | School year end (e.g., 2025 for 2024-25) |
| `district_id`                  | 5-digit district identifier              |
| `campus_id`                    | 7-digit campus identifier                |
| `district_name`, `campus_name` | Names                                    |
| `type`                         | “State”, “District”, or “Campus”         |
| `grade_level`                  | “TOTAL”, “PK”, “K”, “01”…“12”, “UG”      |
| `subgroup`                     | Demographic group                        |
| `n_students`                   | Enrollment count                         |
| `pct`                          | Percentage of total                      |

------------------------------------------------------------------------

## Caching

``` r
# View cached files
cache_status()

# Clear cache
clear_cache()

# Force fresh download
enr <- fetch_enr(2025, use_cache = FALSE)
```

------------------------------------------------------------------------

## Learn More

See the [full
vignette](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks.html)
for more insights and analysis of South Dakota enrollment trends.

------------------------------------------------------------------------

## Part of the State Schooldata Project

A simple, consistent interface for accessing state-published school data
in Python and R.

**All 50 state packages:**
[github.com/almartin82](https://github.com/almartin82?tab=repositories&q=schooldata)

**Original package:**
[njschooldata](https://github.com/almartin82/njschooldata) - the
mothership that started it all.

------------------------------------------------------------------------

## Author

Andy Martin (<almartin@gmail.com>) GitHub:
[github.com/almartin82](https://github.com/almartin82)

## License

MIT
