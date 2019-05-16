#' build_ref_year_string
#' Create povcalnet query string for queries specifying reference years
#'
#' @param country character: Requested country. Use \href{https://www.iban.com/country-codes}{iso3 codes }
#' @param poverty_line numeric: Requested poverty line in international dollars
#' @param reference_year numeric: Requested survey years
#' @param ppp numeric: Optional - Custom PPP exchange rates (Purchasing Power parity)
#' @param display character: Specify whether the results should be displayed at
#' the country level, or as regional aggregates. Choose one of two options: "country_level",
#' "regional_aggregate"
#'
#' @return character
#' @export
#'
#' @examples
#'
#' build_ref_year_string(country = c("ALB", "CHN"),
#' poverty_line = 1.9,
#' reference_year = c(2002, 2012))

build_ref_year_string <- function(country,
                                  poverty_line,
                                  reference_year,
                                  ppp = NULL,
                                  display = "regional_aggregate") {
  # CHECK inputs
  assertthat::assert_that(length(reference_year) > 0,
                          msg = "Please submit at least ONE reference year")
  assertthat::assert_that(length(country) > 0,
                          msg = "Please submit at least ONE country")
  assertthat::assert_that(length(poverty_line) == 1,
                          msg = "When using the reference_year parameter, please submit only one poverty_line,
                          for instance: poverty_line = 1.9")
  assertthat::assert_that(display %in% c("regional_aggregate", "country_level"),
                          msg = "The 'display' argument only accepts one of two value:\n
                          - 'regional_aggregate'\n
                          - 'country_level")
  assertthat::assert_that(is.null(ppp) | length(ppp) == length(country),
                          msg = "When using custom ppp, please ensure you submit
                          one ppp per country")

  i <- seq_along(country) - 1

  reference_year <- paste0("YearSelected=", paste(reference_year, collapse = ","))
  poverty_line <- paste0("PovertyLine=", poverty_line)
  country <- paste0("country=", paste(country, collapse = ","))
  display <- if (display == "regional_aggregate") {
    display <- paste0("display=Regional")
  } else {
    display <- paste0("display=C")
  }

  if (!is.null(ppp)) {
    ppp <- purrr::map2_chr(i, ppp, function(x, y) {paste0("PPP", x, "=", y)})
    ppp <- paste(ppp, collapse = "&")
    out <- paste(reference_year, country, poverty_line, ppp, display, collapse = "&")
    out <- stringr::str_replace_all(out, pattern = " ", replacement = "&")
  } else {
    out <- paste(reference_year, country, poverty_line, display, collapse = "&")
    out <- stringr::str_replace_all(out, pattern = " ", replacement = "&")
  }

  return(out)
}
