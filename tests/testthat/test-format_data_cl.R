test_that("Basic data formatting works as expected", {
  df <- read.csv("../testdata/country_level_national.csv",
                 stringsAsFactors = FALSE)
  out <- format_data_cl(df, format = "csv")
  out <- as.data.frame(out)
  expected_names <- c("countrycode", "countryname", "regioncode", "coveragetype", "year",
                      "datayear", "datatype", "isinterpolated", "usemicrodata", "ppp",
                      "povertyline", "mean", "headcount", "povertygap", "povertygapsq",
                      "watts", "gini", "median", "mld", "polarization",
                      "population", "decile1", "decile2", "decile3", "decile4",
                      "decile5", "decile6", "decile7", "decile8", "decile9", "decile10")
  expect_equal(names(out), expected_names)

  expected_data_types <- c("consumption", "income", "mixed")

  expect_true(all(unique(out$data_type) %in% expected_data_types))
}
)
