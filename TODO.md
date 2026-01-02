# TODO: pkgdown Build Issues

## Network Connectivity Issues (2026-01-01)

The pkgdown build is failing due to network connectivity issues:

### Issue 1: CRAN connectivity timeout
- **Error**: `Timeout was reached [cloud.r-project.org]: Connection timed out after 10000 milliseconds`
- **Location**: `pkgdown:::cran_link()` - pkgdown checks if package is on CRAN for sidebar links
- **Impact**: Build fails during `build_home()` phase

### Issue 2: South Dakota DOE website unreachable
- **Error**: `Failed to connect to doe.sd.gov port 443 after 75021 ms: Could not connect to server`
- **Location**: Vignette `enrollment_hooks.Rmd` line 39 - `fetch_enr_multi(c(2015:2020, 2022:2025))`
- **Impact**: Vignette cannot fetch enrollment data to render examples

### Diagnosis
- General internet connectivity is working (google.com responds)
- doe.sd.gov shows 100% packet loss on ping
- cloud.r-project.org is timing out on HTTPS connections

### Possible Solutions
1. Wait for network/site issues to resolve
2. Consider adding pre-computed vignettes (`eval = FALSE` with static output)
3. Add cached sample data for vignette examples
4. Configure pkgdown to skip CRAN link check (may require pkgdown update)

### To Retry
```r
pkgdown::build_site()
```
