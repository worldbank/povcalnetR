#' build_country_level_string
#' Create povcalnet query string for country level, survey year specification
#'
#' @param country character: Requested country. Use \href{https://www.iban.com/country-codes}{iso3 codes }
#' @param poverty_line numeric: Requested poverty lines in international dollars
#' @param survey_year numeric: Requested survey years
#' @param ppp numeric: Optional - Custom PPP exchange rates (Purchasing Power parity)
#' @param coverage_type character: Optional - Type of data coverage. Take one to the following values
#' rural; urban; national; special_cpi; aggregated_distribution, invalid_ppp
#'
#' @return character
#' @export
#'
#' @examples
#'
#' build_country_level_string(country = c("ALB", "CHN"),
#' poverty_line = c(1.9, 2.0),
#' survey_year = c(2002, 2002))

build_country_level_string <- function(country,
                                       poverty_line,
                                       survey_year,
                                       ppp = NULL,
                                       coverage_type = NULL) {
  # CHECK inputs
  assertthat::assert_that(length(unique(purrr::map_int(list(country, poverty_line, survey_year), length))) == 1,
                          msg = "When parameter_specification = 'country_level',
                          please ensure that you submit the same number of:
                          country; poverty_line; and survey_year")
  assertthat::assert_that(is.null(ppp) | length(ppp) == length(country),
                          msg = "When using custom ppp, please ensure you submit
                          one ppp per country")
  assertthat::assert_that(is.null(coverage_type) | length(coverage_type) == length(country),
                          msg = "When using custom ppp, please ensure you submit
                          one coverage_type per country")


  i <- seq_along(country) - 1

  if (!is.null(coverage_type)) {
    coverage_type <- unname(coverage_lkup[coverage_type])
    country <- paste(country, coverage_type, sep = "_")
  }

  country <- purrr::map2_chr(i, country, function(x, y) {paste0("C", x, "=", y)})
  poverty_line <- purrr::map2_chr(i, poverty_line, function(x, y) {paste0("PL", x, "=", y)})
  survey_year <- purrr::map2_chr(i, survey_year, function(x, y) {paste0("Y", x, "=", y)})

  if (!is.null(ppp)) {
    ppp <- purrr::map2_chr(i, ppp, function(x, y) {paste0("PPP", x, "=", y)})
    out <- paste(country, poverty_line, survey_year, ppp, collapse = "&")
    out <- stringr::str_replace_all(out, pattern = " ", replacement = "&")
  } else {
    out <- paste(country, poverty_line, survey_year, collapse = "&")
    out <- stringr::str_replace_all(out, pattern = " ", replacement = "&")
  }

  return(out)
}
