country = c("ALB", "CHN")
poverty_line = 1.9
year = c(2002, 2012)
ppp = c(100, 50)
aggregate = FALSE
coverage = "national"

test_that("Incorrect inputs trigger errors", {

  expect_error(build_query_string(country = country,
                                     poverty_line = c(1.9, 2.0),
                                     year = year)
  )

  expect_error(build_query_string(country = country,
                                     poverty_line = poverty_line,
                                     year = year,
                                     ppp = 100)
  )

  expect_error(build_query_string(country = country,
                                     poverty_line = poverty_line,
                                     year = year,
                                     display = "urban")
  )

})

test_that("Country level queries are built correctly", {
  query <- build_query_string(country = country,
                              poverty_line = poverty_line,
                              year = year)
  expect_equal(query, "SurveyYears=2002,2012&Countries=ALB_3,CHN_5&PovertyLine=1.9&display=C&format=json")

  query <- build_query_string(country = country,
                                 poverty_line = poverty_line,
                                 year = year,
                                 ppp = ppp)
  expect_equal(query, "SurveyYears=2002,2012&Countries=ALB_3,CHN_5&PovertyLine=1.9&PPP0=100&PPP1=50&display=C&format=json")

  query <- build_query_string(country = country,
                                 poverty_line = poverty_line,
                                 year = year,
                                 ppp = ppp,
                                 aggregate = TRUE)
  expect_equal(query, "YearSelected=2002,2012&Countries=ALB_3,CHN_5&PovertyLine=1.9&PPP0=100&PPP1=50&display=Regional&format=json")

  query <- build_query_string(country = "all",
                              poverty_line = poverty_line,
                              year = year)
  expect_equal(query, "SurveyYears=2002,2012&Countries=all&PovertyLine=1.9&display=C&format=json")

  query <- build_query_string(country = country,
                              poverty_line = poverty_line,
                              year = year,
                              coverage = coverage)
  expect_equal(query, "SurveyYears=2002,2012&Countries=ALB_3,CHN_5&PovertyLine=1.9&display=C&format=json")
})


