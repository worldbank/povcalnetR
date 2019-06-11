test_that("Basic data formatting works as expected", {
  df <- read.csv("../testdata/country_level_national.csv",
                 stringsAsFactors = FALSE)
  out <- format_data_cl(df, coverage = "national")
  expected_names <- c("country_code", "country_name", "region_code", "coverage_type", "request_year",
                      "data_year", "data_type", "is_interpolated", "use_microdata", "ppp",
                      "poverty_line", "mean", "headcount", "poverty_gap", "poverty_gap_sq",
                      "watts", "gini", "median", "mld", "polarization",
                      "population", "decile1", "decile2", "decile3", "decile4",
                      "decile5", "decile6", "decile7", "decile8", "decile9", "decile10")
  expect_equal(names(out), expected_names)

  expected_data_types <- c("consumption", "income", "mixed")

  expect_true(all(unique(out$data_type) %in% expected_data_types))
}
)



