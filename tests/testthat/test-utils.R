# Tests for utility functions

test_that("safe_numeric handles NULL and empty input", {
  expect_equal(length(safe_numeric(NULL)), 0)
  expect_equal(length(safe_numeric(character(0))), 0)
})

test_that("safe_numeric handles numeric input", {
  expect_equal(safe_numeric(42), 42)
  expect_equal(safe_numeric(c(1, 2, 3)), c(1, 2, 3))
  expect_equal(safe_numeric(c(1.5, 2.5)), c(1.5, 2.5))
})

test_that("safe_numeric handles character input", {
  expect_equal(safe_numeric("42"), 42)
  expect_equal(safe_numeric("1,234"), 1234)
  expect_equal(safe_numeric("1,234,567"), 1234567)
})

test_that("safe_numeric handles suppression markers", {
  expect_true(is.na(safe_numeric("*")))
  expect_true(is.na(safe_numeric(".")))
  expect_true(is.na(safe_numeric("-")))
  expect_true(is.na(safe_numeric("-1")))
  expect_true(is.na(safe_numeric("<5")))
  expect_true(is.na(safe_numeric("N/A")))
  expect_true(is.na(safe_numeric("NA")))
  expect_true(is.na(safe_numeric("n/a")))
  expect_true(is.na(safe_numeric("")))
})

test_that("safe_numeric trims whitespace", {
  expect_equal(safe_numeric("  42  "), 42)
  expect_equal(safe_numeric("\t100\n"), 100)
})

test_that("get_available_years returns expected range", {
  years <- get_available_years()
  expect_true(2006 %in% years)  # Earliest year with Excel data
  expect_true(2011 %in% years)
  expect_true(2025 %in% years)
  expect_false(2005 %in% years)  # 2005 and earlier only have PDF
  expect_equal(years, 2006:2025)
})

test_that("build_sd_url constructs valid district URLs", {
  # Era 0 (2006-2010)
  url_2010 <- build_sd_url(2010, "district")
  expect_true(grepl("FE10_Psum\\.xls$", url_2010))

  # Era 1 (2011-2012)
  url_2011 <- build_sd_url(2011, "district")
  expect_true(grepl("FE11_Psum\\.xlsx$", url_2011))

  # Era 2 (2013-2020)
  url_2015 <- build_sd_url(2015, "district")
  expect_true(grepl("Pubdsgr15\\.xlsx$", url_2015))

  # Era 3 (2021+)
  url_2025 <- build_sd_url(2025, "district")
  expect_true(grepl("Pubdisgr-2025\\.xlsx$", url_2025))
})

test_that("build_sd_url errors on unknown file type", {
  expect_error(build_sd_url(2024, "unknown_type"), "Unknown file_type")
})

test_that("get_district_filename handles all format eras", {
  # Era 0 (2006-2010) - XLS format
  expect_equal(get_district_filename(2006), "2006_PublicPk-12.xls")
  expect_equal(get_district_filename(2007), "Public_pk-12_totals07.xls")
  expect_equal(get_district_filename(2008), "WEBPublicbydistrictPK-12.xls")
  expect_equal(get_district_filename(2009), "Public_district.xls")
  expect_equal(get_district_filename(2010), "FE10_Psum.xls")

  # Era 1 (2011-2012) - XLSX format
  expect_equal(get_district_filename(2011), "FE11_Psum.xlsx")
  expect_equal(get_district_filename(2012), "FE12_Pscgr.xlsx")

  # Era 2 (2013-2020)
  expect_equal(get_district_filename(2013), "Pubdsgr13.xlsx")
  expect_equal(get_district_filename(2016), "Pubdsgr16.xlsx")
  expect_equal(get_district_filename(2017), "Pubdsgr17b.xlsx")
  expect_equal(get_district_filename(2018), "PSgrade-18a.xlsx")
  expect_equal(get_district_filename(2019), "Pubdisgr-19.xlsx")
  expect_equal(get_district_filename(2020), "Pubdisgr-20f.xlsx")

  # Era 3 (2021+) - Note: 2021 uses 2-digit year, 2022+ uses 4-digit
  expect_equal(get_district_filename(2021), "Pubdisgr-21.xlsx")
  expect_equal(get_district_filename(2022), "Pubdisgr-2022.xlsx")
  expect_equal(get_district_filename(2024), "Pubdisgr-2024.xlsx")
  expect_equal(get_district_filename(2025), "Pubdisgr-2025.xlsx")
})

test_that("get_district_filename errors on unsupported years", {
  expect_error(get_district_filename(2005), "not supported")  # Only PDF available
  expect_error(get_district_filename(2000), "not supported")
})

test_that("URL base path is correct", {
  url <- build_sd_url(2024, "district")
  expect_true(grepl("^https://doe\\.sd\\.gov/ofm/documents/", url))
})
