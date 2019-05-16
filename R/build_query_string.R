#' build_query_string
#' Create simplified povcalnet query string
#'
#' @param country character: list of country iso3 code (accepts multiple) or `all`. Use \href{https://www.iban.com/country-codes}{iso3 codes }
#' @param poverty_line numeric: poverty line (in 2011 PPP-adjusted USD) to calculate poverty measures
#' @param year numeric:  list of years, or `all`, or `last`.
#' @param ppp numeric: Optional - Allows the selection of custom PPP (Purchasing Power Parity) exchange rates
#' @param display character: Specify whether the results should be displayed at
#' the country level, or as regional aggregates. Choose one of two options: "country_level",
#' "regional_aggregate"
#'
#' @return character
#' @export
#'
#' @examples
#'
#' build_query_string(country = c("ALB", "CHN"),
#' poverty_line = 1.9,
#' year = c(2002, 2012))

build_query_string <- function(country,
                               poverty_line,
                               year,
                               aggregate = FALSE,
                               interpolate = FALSE,
                               coverage = "national",
                               ppp = NULL,
                               format = "json") {
  # CHECK inputs
  check_build_query_string_inputs(country,
                                  poverty_line,
                                  year,
                                  aggregate,
                                  interpolate,
                                  coverage,
                                  ppp )


  i <- seq_along(country) - 1


  if (aggregate == TRUE) {
    interpolate <- TRUE
    coverage = "national"
  }

  if ((!is.null(coverage)) & (all(country != "all"))) {
    coverage <- unname(coverage_lkup[coverage])
    country <- paste(country, coverage, sep = "_")
  }

  if (interpolate == TRUE) {
    year_str <- "YearSelected="
  } else {
    year_str <- "SurveyYears="
  }
  year <- paste0(year_str, paste(year, collapse = ","))
  poverty_line <- paste0("PovertyLine=", poverty_line)
  country <- paste0("Countries=", paste(country, collapse = ","))
  format <- paste0("format=", format)
  display <- if (aggregate == TRUE) {
    display <- paste0("display=Regional")
  } else {
    display <- paste0("display=C")
  }

  if (!is.null(ppp)) {
    ppp <- purrr::map2_chr(i, ppp, function(x, y) {paste0("PPP", x, "=", y)})
    ppp <- paste(ppp, collapse = "&")
    out <- paste(year, country, poverty_line, ppp, display, format, collapse = "&")
    out <- stringr::str_replace_all(out, pattern = " ", replacement = "&")
  } else {
    out <- paste(year, country, poverty_line, display, format, collapse = "&")
    out <- stringr::str_replace_all(out, pattern = " ", replacement = "&")
  }

  return(out)
}


check_build_query_string_inputs <- function(country,
                                            poverty_line,
                                            year,
                                            aggregate,
                                            interpolate,
                                            coverage,
                                            ppp)
{

  assertthat::assert_that(length(country) > 0,
                          msg = "Please submit at least ONE country")
  assertthat::assert_that(length(year) > 0,
                          msg = "Please submit at least ONE year")
  assertthat::assert_that(length(poverty_line) > 0,
                          msg = "Please submit ONE poverty liner")
  assertthat::assert_that(length(poverty_line) == 1,
                          msg = "Please submit only one poverty_line,
                          for instance: poverty_line = 1.9")
  assertthat::assert_that(coverage %in% names(coverage_lkup),
                          msg = paste0("The 'coverage' argument only accepts one of the following values:\n",
                                       names(coverage_lkup)))
  assertthat::assert_that(is.null(ppp) | length(ppp) == length(country),
                          msg = "When using custom ppp, please ensure you submit
                          one ppp per country")
  if (aggregate == TRUE & interpolate == FALSE) {
    message("You specified `aggregate = TRUE`. Aggregation is only possible
            over a common reference year: The `interpolate` paramater will be
            forced to `TRUE`")}

  if (aggregate == TRUE & coverage != "national") {
    message("You specified `aggregate = TRUE`. Aggregation is only possible
  for national coverage The `coverage` paramater will be
  forced to `national`")}
}
