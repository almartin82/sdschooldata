# State Schooldata Packages

This section contains universal instructions that apply to all 49 state
schooldata packages.

------------------------------------------------------------------------

## CRITICAL DATA SOURCE RULES

**NEVER use Urban Institute API, NCES CCD, or ANY federal data source**
— the entire point of these packages is to provide STATE-LEVEL data
directly from state DOEs. Federal sources aggregate/transform data
differently and lose state-specific details. If a state DOE source is
broken, FIX IT or find an alternative STATE source — do not fall back to
federal data.

**NEVER fabricate data in ANY form.** This is the single most important
rule in the entire project. Violations include but are not limited to:

- **Random generation:**
  [`rnorm()`](https://rdrr.io/r/stats/Normal.html),
  [`runif()`](https://rdrr.io/r/stats/Uniform.html),
  [`set.seed()`](https://rdrr.io/r/base/Random.html),
  [`sample()`](https://rdrr.io/r/base/sample.html),
  [`rlnorm()`](https://rdrr.io/r/stats/Lognormal.html),
  [`rgamma()`](https://rdrr.io/r/stats/GammaDist.html), or any random
  number generation
- **Hardcoded numbers:** Hand-typing enrollment counts in `tribble()`,
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html), `tibble()`,
  or any other data structure. If a human typed the number instead of
  downloading it from a state DOE, it is fabricated.
- **“Plausible-looking” fake data:** Creating numbers that look real but
  aren’t — smooth monotonic trends, round numbers, demographically
  “reasonable” percentages applied uniformly. This is the WORST form of
  fabrication because it is designed to deceive.
- **`create_example_data()` functions:** Helper functions that generate
  fake datasets, regardless of how realistic they look
- **Fixed demographic percentages:** Applying constant demographic
  ratios across all years/districts (real demographics change year to
  year)
- **Uniform grade distributions:** Using the same grade-level
  percentages for every district (real districts vary significantly)

**The test is simple: can you trace every number back to a downloaded
file from a state DOE website?** If not, it is fabricated. There is no
gray area. If the data source is unavailable, the package MUST use Under
Construction status — not fake data.

------------------------------------------------------------------------

## Git Commits and PRs

- NEVER reference Claude, Claude Code, or AI assistance in commit
  messages
- NEVER reference Claude, Claude Code, or AI assistance in PR
  descriptions
- NEVER add Co-Authored-By lines mentioning Claude or Anthropic
- Keep commit messages focused on what changed, not how it was written

------------------------------------------------------------------------

## Git Workflow (REQUIRED)

### Feature Branch + PR + Auto-Merge Policy

**NEVER push directly to main.** All changes must go through PRs with
auto-merge:

``` bash
# 1. Create feature branch
git checkout -b fix/description-of-change

# 2. Make changes, commit
git add -A
git commit -m "Fix: description of change"

# 3. Push and create PR with auto-merge
git push -u origin fix/description-of-change
gh pr create --title "Fix: description" --body "Description of changes"
gh pr merge --auto --squash

# 4. Clean up stale branches after PR merges
git checkout main && git pull && git fetch --prune origin
```

### Branch Cleanup (REQUIRED)

**Clean up stale branches every time you touch this package:**

``` bash
# Delete local branches merged to main
git branch --merged main | grep -v main | xargs -r git branch -d

# Prune remote tracking branches
git fetch --prune origin
```

### Auto-Merge Requirements

PRs auto-merge when ALL CI checks pass: - R-CMD-check (0 errors, 0
warnings) - Python tests (if py{st}schooldata exists) - pkgdown build
(vignettes must render)

If CI fails, fix the issue and push - auto-merge triggers when checks
pass.

------------------------------------------------------------------------

## Local Testing Before PRs (REQUIRED)

**PRs will not be merged until CI passes.** Run these checks locally
BEFORE opening a PR:

### CI Checks That Must Pass

| Check        | Local Command                                                                  | What It Tests                                  |
|--------------|--------------------------------------------------------------------------------|------------------------------------------------|
| R-CMD-check  | `devtools::check()`                                                            | Package builds, tests pass, no errors/warnings |
| Python tests | `pytest tests/test_py{st}schooldata.py -v`                                     | Python wrapper works correctly                 |
| pkgdown      | [`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html) | Documentation and vignettes render             |

### Quick Commands

``` r
# R package check (required)
devtools::check()

# Python tests (required)
system("pip install -e ./py{st}schooldata && pytest tests/test_py{st}schooldata.py -v")

# pkgdown build (required)
pkgdown::build_site()
```

### Pre-PR Checklist

Before opening a PR, verify: - \[ \] `devtools::check()` — 0 errors, 0
warnings - \[ \] `pytest tests/test_py{st}schooldata.py` — all tests
pass - \[ \]
[`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)
— builds without errors - \[ \] Vignettes render (no `eval=FALSE` hacks)

------------------------------------------------------------------------

## LIVE Pipeline Testing

This package includes `tests/testthat/test-pipeline-live.R` with LIVE
network tests.

### Test Categories:

1.  URL Availability - HTTP 200 checks
2.  File Download - Verify actual file (not HTML error)
3.  File Parsing - readxl/readr succeeds
4.  Column Structure - Expected columns exist
5.  get_raw_enr() - Raw data function works
6.  Data Quality - No Inf/NaN, non-negative counts
7.  Aggregation - State total \> 0
8.  Output Fidelity - tidy=TRUE matches raw

### Running Tests:

``` r
devtools::test(filter = "pipeline-live")
```

------------------------------------------------------------------------

## Valid Filter Values (tidy enrollment via `fetch_enr(tidy = TRUE)`)

### subgroup

`total_enrollment`, `white`, `black`, `hispanic`, `asian`,
`native_american`, `pacific_islander`, `multiracial`, `male`, `female`

**NOT in tidy enrollment:** No special population subgroups
(economically disadvantaged, ELL, special education) are available in SD
enrollment data. Gender data comes from a separate campus-level file and
may not be available at the district level. Demographics come from a
campus race/ethnicity file.

### grade_level

`PK`, `K`, `01`-`12`, `UG`, `TOTAL`

Grade aggregates from
[`enr_grade_aggs()`](https://almartin82.github.io/sdschooldata/reference/enr_grade_aggs.md):
`K8`, `HS`, `K12`

**Common trap:** SD raw data uses `KG` for kindergarten and `UG` for
ungraded.
[`process_enr()`](https://almartin82.github.io/sdschooldata/reference/process_enr.md)
maps `KG` to `grade_k` which becomes `K` in tidy output. `UG` (ungraded)
is preserved. The K12 aggregate excludes both `PK` and `UG`.

### entity flags

`is_state`, `is_district`, `is_campus`, `is_public`

**Note:** SD uses `is_campus` (not `is_school`). `is_public` is derived
from `district_type_code` (code `10` = public). Historical data
(pre-2011) may not have `district_type_code`, in which case `is_public`
defaults to TRUE.

------------------------------------------------------------------------

## Fidelity Requirement

**tidy=TRUE MUST maintain fidelity to raw, unprocessed data:** -
Enrollment counts in tidy format must exactly match the wide format - No
rounding or transformation of counts during tidying - Percentages are
calculated fresh but counts are preserved - State aggregates are sums of
school-level data

------------------------------------------------------------------------

## README Images from Vignettes (REQUIRED)

**NEVER use `man/figures/` or `generate_readme_figs.R` for README
images.**

README images MUST come from pkgdown-generated vignette output so they
auto-update on merge:

``` markdown
![Chart name](https://almartin82.github.io/{package}/articles/{vignette}_files/figure-html/{chunk-name}-1.png)
```

**Why:** Vignette figures regenerate automatically when pkgdown builds.
Manual `man/figures/` requires running a separate script and is easy to
forget, causing stale/broken images.

------------------------------------------------------------------------

## README and Vignette Code Matching (REQUIRED)

**CRITICAL RULE (as of 2026-01-08):** ALL code blocks in the README MUST
match code in a vignette EXACTLY (1:1 correspondence).

### Why This Matters

The Idaho fix revealed critical bugs when README code didn’t match
vignettes: - Wrong district names (lowercase vs ALL CAPS) - Text claims
that contradicted actual data - Missing data output in examples

### README Story Structure (REQUIRED)

Every story/section in the README MUST follow this structure:

1.  **Claim**: A factual statement about the data
2.  **Explication**: Brief explanation of why this matters
3.  **Code**: R code that fetches and analyzes the data (MUST exist in a
    vignette)
4.  **Code Output**: Data table/print statement showing actual values
    (REQUIRED)
5.  **Visualization**: Chart from vignette (auto-generated from pkgdown)

### Enforcement

The `state-deploy` skill verifies this before deployment: - Extracts all
README code blocks - Searches vignettes for EXACT matches - Fails
deployment if code not found in vignettes - Randomly audits packages for
claim accuracy

### What This Prevents

- ❌ Wrong district/entity names (case sensitivity, typos)
- ❌ Text claims that contradict data
- ❌ Broken code that fails silently
- ❌ Missing data output
- ✅ Verified, accurate, reproducible examples

### Example

``` markdown
### 1. State enrollment grew 28% since 2002

State added 68,000 students from 2002 to 2026, bucking national trends.

```r
library(arschooldata)
library(dplyr)

enr <- fetch_enr_multi(2002:2026)

enr %>%
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, n_students) %>%
  filter(end_year %in% c(2002, 2026)) %>%
  mutate(change = n_students - lag(n_students),
         pct_change = round((n_students / lag(n_students) - 1) * 100, 1))
# Prints: 2002=XXX, 2026=YYY, change=ZZZ, pct=PP.P%
```

![Chart](https://almartin82.github.io/arschooldata/articles/...)

Chart

    ---

    # sdschooldata Package

    This section contains South Dakota-specific instructions and information.

    ---

    ## Data Availability

    **Available Years:** 2006-2026

    **Data Source:** South Dakota Department of Education
    - URL: [Add your state's URL here]
    - Files: [Add file descriptions here]

    ---

    ## Python Testing

    Python wrapper test file: `tests/test_pysdschooldata.py`

    ```bash
    pip install -e ./pysdschooldata && pytest tests/test_pysdschooldata.py -v

------------------------------------------------------------------------

## Known Data Issues

\[Add any state-specific data issues or quirks here\]
