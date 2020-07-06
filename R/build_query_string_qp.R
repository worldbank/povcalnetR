#' build_query_string_qp
#' Create simplified povcalnet_qp query string
#'
#' @param country character: list of country iso3 code (accepts multiple) or `all`. Use \href{https://www.iban.com/country-codes}{iso3 codes }
#' @param popshare numeric: Percentage of the population who live below the poverty line
#' @param year numeric:  list of years, or `all`, or `last`.
#' @param aggregate logical: `TRUE` will return aggregate results,
#' `FALSE` country-level results.
#' @param fill_gaps logical: `TRUE` will interpolate / extrapolate values when
#' surveys are not available for a specific year.
#' @param coverage character: Can take one of four values: `all` (default), `national`, `urban`, `rural`.
#' @param ppp numeric: Optional - Allows the selection of custom PPP (Purchasing Power Parity) exchange rates
#' @param format character: Response format to be requested from the API:
#' `csv` or `json`
#'
#' @return character
#' @export
#'
#' @examples
#'
#' build_query_string_qp(country = c("ALB", "CHN"),
#' popshare = 1.9,
#' year = c(2002, 2012))

build_query_string_qp <- function(country,
                                  popshare,
                                  year,
                                  aggregate = FALSE,
                                  fill_gaps = FALSE,
                                  coverage = "all",
                                  ppp = NULL,
                                  format = "json") {
  # CHECK inputs
  check_build_query_string_qp_inputs(country,
                                     popshare,
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
  }

  # Assign list of country codes
  country <- assign_country(country)

  # Add correct coverage suffix to country code
  country <- assign_coverage(country = country, coverage = coverage)

  # Build year section
  # Year parameter is different when using "survey year" or "reference year"
  if (fill_gaps == TRUE) {
    year_str <- "YearSelected="
  } else {
    year_str <- "SurveyYears="
  }
  year <- paste0(year_str, paste(year, collapse = ","))

  # Build poverty line section
  popshare <- paste0("QP=", popshare)

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
    out <- paste(year, country, popshare, ppp, display, format, collapse = "&")
    out <- stringr::str_replace_all(out, pattern = " ", replacement = "&")
  } else {
    out <- paste(year, country, popshare, display, format, collapse = "&")
    out <- stringr::str_replace_all(out, pattern = " ", replacement = "&")
  }

  return(out)
}


check_build_query_string_qp_inputs <- function(country,
                                               popshare,
                                               year,
                                               aggregate,
                                               fill_gaps,
                                               coverage,
                                               ppp)
{

  accepted_coverage <- c(names(coverage_lkup), "all")

  assertthat::assert_that(length(country) > 0,
                          msg = "Please submit at least ONE country")
  assertthat::assert_that(length(year) > 0,
                          msg = "Please submit at least ONE year")
  assertthat::assert_that(length(popshare) > 0,
                          msg = "Please submit ONE quantile")
  assertthat::assert_that(length(popshare) == 1,
                          msg = "Please submit only one popshare,
                          for instance: popshare = 0.5")
  assertthat::assert_that(popshare > 0,
                          msg = "popshare must be superior to 0,
                          for instance: popshare = 0.5")
  assertthat::assert_that(popshare <= 1,
                          msg = "popshare must less or equal to 1,
                          for instance: popshare = 0.9")
  assertthat::assert_that(coverage %in% accepted_coverage,
                          msg = paste0("The 'coverage' argument only accepts one of the following values:\n",
                                       accepted_coverage))
  assertthat::assert_that(length(coverage) == 1,
                          msg = "Please submit only ONE coverage")
  assertthat::assert_that(is.null(ppp) | length(ppp) == length(country),
                          msg = "When using custom ppp, please ensure you submit
                          one ppp per country")
  assertthat::assert_that(is.null(ppp) | coverage != "all",
                          msg = "Custom ppp are not allowed with `coverage` = `all`")
  if (aggregate == TRUE & fill_gaps == FALSE) {
    message("You specified `aggregate = TRUE`. Aggregation is only possible
            over a common reference year: The `fill_gaps` parameter will be
            forced to `TRUE`")}
  #
  #   if (aggregate == TRUE & coverage != "national") {
  #     message("You specified `aggregate = TRUE`. Aggregation is only possible
  #   for national coverage The `coverage` parameter will be
  #   forced to `national`")}
}
