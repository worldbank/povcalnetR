#' build_query_string
#' Create simplified povcalnet query string
#'
#' @param country character: list of country iso3 code (accepts multiple) or `all`. Use \href{https://www.iban.com/country-codes}{iso3 codes }
#' @param poverty_line numeric: poverty line (in 2011 PPP-adjusted USD) to calculate poverty measures
#' @param year numeric:  list of years, or `all`, or `last`.
#' @param aggregate logical: `TRUE` will return aggregate results,
#' `FALSE` country-level results.
#' @param fill_gaps logical: `TRUE` will interpolate / extrapolate values when
#' surveys are not available for a specific year.
#' @param coverage character: Can take one of three values: `national`, `urban`, `rural`
#' @param ppp numeric: Optional - Allows the selection of custom PPP (Purchasing Power Parity) exchange rates
#' @param format character: Response format to be requested from the API:
#' `csv` or `json`
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
                               fill_gaps = FALSE,
                               coverage = "national",
                               ppp = NULL,
                               format = "json") {
  # CHECK inputs
  check_build_query_string_inputs(country,
                                  poverty_line,
                                  year,
                                  aggregate,
                                  fill_gaps,
                                  coverage,
                                  ppp )


  i <- seq_along(country) - 1

  # Aggregation only possible for national coverage and common reference years
  # setting function arguments accordingly
  if (aggregate == TRUE) {
    fill_gaps <- TRUE
    coverage = "national"
  }

  # Add correct coverage suffix to country code
  if ((!is.null(coverage)) & (all(country != "all"))) {
    if (coverage == "national") {
      coverage_codes <- national_coverage_lkup[country]
      country <- paste(country, coverage_codes, sep = "_")
    } else {
      coverage <- unname(coverage_lkup[coverage])
      country <- paste(country, coverage, sep = "_")
    }
  }

  # Build year section
  # Year parameter is different when using "survey year" or "reference year"
  if (fill_gaps == TRUE) {
    year_str <- "YearSelected="
  } else {
    year_str <- "SurveyYears="
  }
  year <- paste0(year_str, paste(year, collapse = ","))

  # Build poverty line section
  poverty_line <- paste0("PovertyLine=", poverty_line)

  # Build country section
  country <- paste0("Countries=", paste(country, collapse = ","))

  # Add requested format
  format <- paste0("format=", format)

  # Add display mode: "aggregate" or "country level"
  display <- if (aggregate == TRUE) {
    display <- paste0("display=Regional")
  } else {
    display <- paste0("display=C")
  }

  # Paste all query elements together
  # Query string will be built differently whether ppp argument is empty or not
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
                                            fill_gaps,
                                            coverage,
                                            ppp)
{

  assertthat::assert_that(length(country) > 0,
                          msg = "Please submit at least ONE country")
  assertthat::assert_that(length(year) > 0,
                          msg = "Please submit at least ONE year")
  assertthat::assert_that(length(poverty_line) > 0,
                          msg = "Please submit ONE poverty line")
  assertthat::assert_that(length(poverty_line) == 1,
                          msg = "Please submit only one poverty_line,
                          for instance: poverty_line = 1.9")
  assertthat::assert_that(coverage %in% names(coverage_lkup),
                          msg = paste0("The 'coverage' argument only accepts one of the following values:\n",
                                       names(coverage_lkup)))
  assertthat::assert_that(is.null(ppp) | length(ppp) == length(country),
                          msg = "When using custom ppp, please ensure you submit
                          one ppp per country")
  if (aggregate == TRUE & fill_gaps == FALSE) {
    message("You specified `aggregate = TRUE`. Aggregation is only possible
            over a common reference year: The `fill_gaps` paramater will be
            forced to `TRUE`")}

  if (aggregate == TRUE & coverage != "national") {
    message("You specified `aggregate = TRUE`. Aggregation is only possible
  for national coverage The `coverage` paramater will be
  forced to `national`")}
}
