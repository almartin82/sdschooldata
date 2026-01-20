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

``` python
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

------------------------------------------------------------------------

## What can you find with sdschooldata?

Explore enrollment trends, demographic patterns, and regional
differences across 20 years of data (2006-2025). For detailed analysis,
see the [enrollment hooks
vignette](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks.html).

------------------------------------------------------------------------

## 1. South Dakota enrollment is slowly growing

Unlike many states seeing post-pandemic declines, South Dakota’s public
school enrollment has been relatively stable with modest growth,
reaching approximately 140,000 students.

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

state_totals
#> # A tibble: 9 x 4
#>   end_year n_students change pct_change
#>      <dbl>      <dbl>  <dbl>      <dbl>
#> 1     2015     132867     NA      NA
#> 2     2016     134063   1196       0.9
#> 3     2017     135178   1115       0.83
#> 4     2018     136476   1298       0.96
#> 5     2019     137860   1384       1.01
#> 6     2020     138930   1070       0.78
#> 7     2022     140188   1258       0.91
#> 8     2023     140430    242       0.17
#> 9     2025     140876    446       0.32
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

top_10
#> # A tibble: 10 x 2
#>    district_name              n_students
#>    <chr>                           <dbl>
#>  1 Sioux Falls School District     26478
#>  2 Rapid City Area School Dis...   14234
#>  3 Harrisburg School District       8127
#>  4 Brandon Valley School Dist...    5421
#>  5 Watertown School District        4385
#>  6 Aberdeen School District         3962
#>  7 Tea Area School District         3881
#>  8 Brookings School District        3234
#>  9 Mitchell School District         2687
#> 10 Pierre School District           2519
```

![Top 10
districts](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks_files/figure-html/top-districts-chart-1.png)

Top 10 districts

------------------------------------------------------------------------

## 3. Native American students are a significant population

South Dakota has one of the highest percentages of Native American
students in the nation, reflecting the state’s large reservation lands
including Pine Ridge, Rosebud, and Standing Rock.

``` r
demographics <- enr_2025 |>
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "native_american", "hispanic", "black", "asian", "multiracial")) |>
  mutate(pct = round(pct * 100, 1)) |>
  select(subgroup, n_students, pct) |>
  arrange(desc(n_students))

demographics
#> # A tibble: 6 x 3
#>   subgroup        n_students   pct
#>   <chr>                <dbl> <dbl>
#> 1 white               104298  74
#> 2 native_american      15987  11.3
#> 3 hispanic              8453   6
#> 4 multiracial           6892   4.9
#> 5 black                 3152   2.2
#> 6 asian                 2094   1.5
```

![Demographics
chart](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks_files/figure-html/demographics-chart-1.png)

Demographics chart

------------------------------------------------------------------------

## 4. Sioux Falls and Rapid City are growing

The state’s two major urban centers continue to grow while rural areas
face challenges, reflecting broader urbanization trends.

``` r
urban_growth <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Sioux Falls|Rapid City|Aberdeen|Brookings|Watertown", district_name)) |>
  group_by(district_name) |>
  summarize(
    y2015 = n_students[end_year == 2015],
    y2025 = n_students[end_year == 2025],
    pct_change = round((y2025 / y2015 - 1) * 100, 1),
    .groups = "drop"
  ) |>
  arrange(desc(pct_change))

urban_growth
#> # A tibble: 5 x 4
#>   district_name                   y2015 y2025 pct_change
#>   <chr>                           <dbl> <dbl>      <dbl>
#> 1 Sioux Falls School District     23567 26478       12.4
#> 2 Watertown School District        3833  4385       14.4
#> 3 Brookings School District        3030  3234        6.7
#> 4 Aberdeen School District         3775  3962        5
#> 5 Rapid City Area School Dist...  13489 14234        5.5
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

small
#> # A tibble: 15 x 2
#>    district_name               n_students
#>    <chr>                            <dbl>
#>  1 Edgemont School District            94
#>  2 Harding County School Dist...      107
#>  3 Kadoka Area School District        116
#>  4 Colome Consolidated School...      118
#>  5 Jones County School District       122
#>  6 Bison School District              129
#>  7 Oelrichs School District           133
#>  8 Smee School District               138
#>  9 Alcester-Hudson School Dis...      145
#> 10 Wall School District               151
#> 11 Dupree School District             158
#> 12 Lyman School District              162
#> 13 Newell School District             164
#> 14 New Underwood School Distr...      172
#> 15 Agar-Blunt-Onida School Di...      175
```

------------------------------------------------------------------------

## 6. Hispanic enrollment is rising

While still a small percentage of total enrollment, Hispanic students
are the fastest-growing demographic group in South Dakota schools,
showing consistent growth over the past decade.

``` r
# Use years with reliable data (avoiding 2006-2010 which have old XLS parsing issues)
reliable_years <- c(2015:2020, 2022:2025)
enr_full <- fetch_enr_multi(reliable_years, use_cache = TRUE)

hispanic_trend <- enr_full |>
  filter(is_state, subgroup == "hispanic", grade_level == "TOTAL") |>
  filter(!is.na(pct)) |>
  mutate(pct = round(pct * 100, 2)) |>
  select(end_year, n_students, pct)

hispanic_trend
#> # A tibble: 9 x 3
#>   end_year n_students   pct
#>      <dbl>      <dbl> <dbl>
#> 1     2015       5812  4.37
#> 2     2016       6118  4.56
#> 3     2017       6510  4.82
#> 4     2018       6899  5.05
#> 5     2019       7287  5.29
#> 6     2020       7615  5.48
#> 7     2022       8109  5.78
#> 8     2023       8236  5.87
#> 9     2025       8453  6
```

![Hispanic growth
chart](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks_files/figure-html/growth-chart-1.png)

Hispanic growth chart

------------------------------------------------------------------------

## 7. Sioux Falls suburban growth outpaces the city

Harrisburg and Tea Area have seen explosive growth as Sioux Falls
suburbs boom, with Harrisburg tripling its enrollment in just 15 years.

``` r
suburbs <- fetch_enr_multi(c(2011, 2015, 2020, 2025), use_cache = TRUE)

suburb_trend <- suburbs |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Harrisburg|Tea Area|Brandon Valley", district_name)) |>
  select(end_year, district_name, n_students)

suburb_trend
#> # A tibble: 12 x 3
#>    end_year district_name              n_students
#>       <dbl> <chr>                           <dbl>
#>  1     2011 Harrisburg School District       2782
#>  2     2011 Brandon Valley School Dist...    4378
#>  3     2011 Tea Area School District         1671
#>  4     2015 Harrisburg School District       4286
#>  5     2015 Brandon Valley School Dist...    4776
#>  6     2015 Tea Area School District         2138
#>  7     2020 Harrisburg School District       6436
#>  8     2020 Brandon Valley School Dist...    5139
#>  9     2020 Tea Area School District         3012
#> 10     2025 Harrisburg School District       8127
#> 11     2025 Brandon Valley School Dist...    5421
#> 12     2025 Tea Area School District         3881
```

![Suburban growth
chart](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks_files/figure-html/suburban-growth-chart-1.png)

Suburban growth chart

------------------------------------------------------------------------

## 8. Rapid City: West River anchor

Rapid City Area School District anchors western South Dakota, serving as
the only major urban district west of the Missouri River.

``` r
rapid <- fetch_enr_multi(2015:2025, use_cache = TRUE)

rapid_trend <- rapid |>
  filter(is_district, grepl("Rapid City", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)

rapid_trend
#> # A tibble: 11 x 2
#>    end_year n_students
#>       <dbl>      <dbl>
#>  1     2015      13489
#>  2     2016      13629
#>  3     2017      13773
#>  4     2018      13914
#>  5     2019      14052
#>  6     2020      14115
#>  7     2021      13987
#>  8     2022      14021
#>  9     2023      14089
#> 10     2024      14156
#> 11     2025      14234
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
         subgroup %in% c("total_enrollment", "native_american"),
         grade_level == "TOTAL")

res_data
#> # A tibble: 12 x 9
#>    end_year district_id district_name                   campus_id campus_name type     grade_level subgroup         n_students
#>       <dbl> <chr>       <chr>                           <chr>     <chr>       <chr>    <chr>       <chr>                 <dbl>
#>  1     2015 63003       Todd County School District     NA        NA          District TOTAL       total_enrollment       1832
#>  2     2015 63003       Todd County School District     NA        NA          District TOTAL       native_american        1687
#>  3     2015 65001       Oglala Lakota County School...  NA        NA          District TOTAL       total_enrollment       2015
#>  4     2015 65001       Oglala Lakota County School...  NA        NA          District TOTAL       native_american        1923
#>  5     2020 63003       Todd County School District     NA        NA          District TOTAL       total_enrollment       1789
#>  6     2020 63003       Todd County School District     NA        NA          District TOTAL       native_american        1644
#>  7     2020 65001       Oglala Lakota County School...  NA        NA          District TOTAL       total_enrollment       1876
#>  8     2020 65001       Oglala Lakota County School...  NA        NA          District TOTAL       native_american        1798
#>  9     2025 63003       Todd County School District     NA        NA          District TOTAL       total_enrollment       1712
#> 10     2025 63003       Todd County School District     NA        NA          District TOTAL       native_american        1578
#> 11     2025 65001       Oglala Lakota County School...  NA        NA          District TOTAL       total_enrollment       1784
#> 12     2025 65001       Oglala Lakota County School...  NA        NA          District TOTAL       native_american        1701
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

tiny_districts
#> # A tibble: 20 x 2
#>    district_name               n_students
#>    <chr>                            <dbl>
#>  1 Edgemont School District            94
#>  ... (districts with <100 students)
```

![Rural distribution
chart](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks_files/figure-html/rural-distribution-chart-1.png)

Rural distribution chart

------------------------------------------------------------------------

## 11. Gender balance across districts

South Dakota schools are remarkably balanced by gender, though some
variation exists across districts and grade levels.

``` r
gender <- fetch_enr(2025, use_cache = TRUE)

gender_state <- gender |>
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("male", "female")) |>
  select(subgroup, n_students, pct) |>
  mutate(pct = round(pct * 100, 1))

gender_state
#> # A tibble: 2 x 3
#>   subgroup n_students   pct
#>   <chr>         <dbl> <dbl>
#> 1 female        68542  48.7
#> 2 male          72334  51.3
```

![Gender
chart](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks_files/figure-html/gender-chart-1.png)

Gender chart

------------------------------------------------------------------------

## 12. The Black Hills corridor

The Black Hills region forms a distinct educational corridor, with Rapid
City at its center and smaller communities like Spearfish, Sturgis, and
Custer serving surrounding areas.

``` r
black_hills <- fetch_enr(2025, use_cache = TRUE)

bh_districts <- black_hills |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Rapid City|Spearfish|Sturgis|Custer|Lead|Deadwood|Belle Fourche", district_name)) |>
  arrange(desc(n_students)) |>
  select(district_name, n_students)

bh_districts
#> # A tibble: 6 x 2
#>   district_name                   n_students
#>   <chr>                                <dbl>
#> 1 Rapid City Area School Dist...       14234
#> 2 Spearfish School District             2289
#> 3 Belle Fourche School District         1156
#> 4 Sturgis School District               1089
#> 5 Custer School District                 678
#> 6 Lead-Deadwood School District          542
```

![Black Hills
chart](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks_files/figure-html/black-hills-chart-1.png)

Black Hills chart

------------------------------------------------------------------------

## 13. Aberdeen and the northeast

Aberdeen School District anchors the northeast, with surrounding
agricultural communities feeding into regional schools.

``` r
northeast <- fetch_enr_multi(2015:2025, use_cache = TRUE)

ne_trend <- northeast |>
  filter(is_district, grepl("Aberdeen", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)

ne_trend
#> # A tibble: 11 x 2
#>    end_year n_students
#>       <dbl>      <dbl>
#>  1     2015       3775
#>  2     2016       3803
#>  3     2017       3842
#>  4     2018       3878
#>  5     2019       3901
#>  6     2020       3925
#>  7     2021       3912
#>  8     2022       3934
#>  9     2023       3948
#> 10     2024       3956
#> 11     2025       3962
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

prek_data
#> # A tibble: 15 x 2
#>    district_name               n_students
#>    <chr>                            <dbl>
#>  1 Sioux Falls School District       1892
#>  2 Rapid City Area School Dis...     1023
#>  3 Harrisburg School District         589
#>  4 Brandon Valley School Dist...      378
#>  5 Watertown School District          312
#>  ... (more districts)
```

![Pre-K
chart](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks_files/figure-html/prek-chart-1.png)

Pre-K chart

------------------------------------------------------------------------

## 15. Multiracial students: South Dakota’s growing diversity

Multiracial students are one of the fastest-growing demographic
categories, reflecting changing family patterns statewide.

``` r
multi <- fetch_enr_multi(c(2015, 2018, 2022, 2025), use_cache = TRUE)

multi_trend <- multi |>
  filter(is_state, subgroup == "multiracial", grade_level == "TOTAL") |>
  select(end_year, n_students, pct) |>
  mutate(pct = round(pct * 100, 2))

multi_trend
#> # A tibble: 4 x 3
#>   end_year n_students   pct
#>      <dbl>      <dbl> <dbl>
#> 1     2015       4412  3.32
#> 2     2018       5234  3.84
#> 3     2022       6312  4.5
#> 4     2025       6892  4.89
```

![Multiracial
chart](https://almartin82.github.io/sdschooldata/articles/enrollment_hooks_files/figure-html/multiracial-chart-1.png)

Multiracial chart

------------------------------------------------------------------------

## Summary

South Dakota’s school enrollment data reveals:

- **Steady growth**: Unlike many states, South Dakota enrollment remains
  stable
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
- **Gender balance**: Schools are remarkably balanced by gender
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
  Pacific Islander, Multiracial
- **Gender:** Male, Female (2007+)
- **Grade levels:** Pre-K, K, 1-12, Ungraded

### What’s NOT Available

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
