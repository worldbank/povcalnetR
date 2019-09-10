
test_that("coverage = 'all' is handled correctly", {
  country <- c("ARG", "CHN")
  expected_output <- unname(all_coverage[names(all_coverage) %in% country])
  function_output <- assign_coverage(coverage = "all", country = country)
  expect_equal(expected_output, function_output)
})

test_that("coverage = 'national' is handled correctly", {
  country <- c("XXX", "CHN", "ALB")
  coverage_codes <- national_coverage_lkup[country]
  expected_output <- c("CHN_5", "ALB_3")
  function_output <- assign_coverage(coverage = "national", country = country)
  expect_equal(expected_output, function_output)
})

test_that("coverage = 'rural' is handled correctly", {
  country <- c("XXX", "CHN", "ALB")
  expected_output <- c("XXX_1", "CHN_1", "ALB_1")
  function_output <- assign_coverage(coverage = "rural", country = country)
  expect_equal(expected_output, function_output)
})

test_that("coverage = 'urban' is handled correctly", {
  country <- c("XXX", "CHN", "ALB")
  expected_output <- c("XXX_2", "CHN_2", "ALB_2")
  function_output <- assign_coverage(coverage = "urban", country = country)
  expect_equal(expected_output, function_output)
})
