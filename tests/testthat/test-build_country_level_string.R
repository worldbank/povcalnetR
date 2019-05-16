country = c("ALB", "CHN")
poverty_line = c(1.9, 2.0)
survey_year = c(2002, 2002)
ppp = c(100, 50)
coverage_type = c("national", "rural")

test_that("Incorrect inputs trigger errors", {

  expect_error(build_country_level_string(country = "ALB",
                                          poverty_line = poverty_line,
                                          survey_year = survey_year)
  )

  expect_error(build_country_level_string(country = country,
                                          poverty_line = 2.0,
                                          survey_year = survey_year)
  )

  expect_error(build_country_level_string(country = country,
                                          poverty_line = poverty_line,
                                          survey_year = 2000)
  )

  expect_error(build_country_level_string(country = country,
                                          poverty_line = poverty_line,
                                          survey_year = survey_year,
                                          ppp = 100)
  )

  expect_error(build_country_level_string(country = country,
                                          poverty_line = poverty_line,
                                          survey_year = survey_year,
                                          coverage_type = "urban")
  )

})

test_that("Country level queries are built correctly", {
  query <- build_country_level_string(country = country,
                                      poverty_line = poverty_line,
                                      survey_year = survey_year)
  expect_equal(query, "C0=ALB&PL0=1.9&Y0=2002&C1=CHN&PL1=2&Y1=2002")

  query <- build_country_level_string(country = country,
                                      poverty_line = poverty_line,
                                      survey_year = survey_year,
                                      ppp = ppp)
  expect_equal(query, "C0=ALB&PL0=1.9&Y0=2002&PPP0=100&C1=CHN&PL1=2&Y1=2002&PPP1=50")

  query <- build_country_level_string(country = country,
                                      poverty_line = poverty_line,
                                      survey_year = survey_year,
                                      ppp = ppp,
                                      coverage_type = coverage_type)
  expect_equal(query, "C0=ALB_3&PL0=1.9&Y0=2002&PPP0=100&C1=CHN_1&PL1=2&Y1=2002&PPP1=50")
})
