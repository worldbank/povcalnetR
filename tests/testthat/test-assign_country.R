

test_that("country = 'all' returns the expected output", {
  expect_equal(assign_country(country = "all"), all_countries)
})

test_that("Arguments other than 'all' are returned as is", {
  country <- c("ALB", "CHN")
  expect_equal(assign_country(country), country)
})
