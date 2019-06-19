#' povcalnet
#' Facilitate interaction with the PovcalNet API
#'
#' @param country character: list of country iso3 code (accepts multiple) or
#' `all`. Use \code{\link{povcalnet_info}} for full list of countries.
#' @param povline numeric: poverty line (in 2011 PPP-adjusted USD) to
#' calculate poverty measures
#' @param year numeric:  list of years, or `all`.
#' @param aggregate logical: `TRUE` will return aggregate results,
#' `FALSE` country-level results.
#' @param fill_gaps logical: `TRUE` will interpolate / extrapolate values when
#' surveys are not available for a specific year.
#' @param coverage character: Can take one of three values: `national`, `urban`, `rural`
#' @param ppp numeric: Optional - Allows the selection of custom PPP (Purchasing Power Parity) exchange rates
#' @param url character: API root URL. For testing purposes only, should not be
#' changed for 99 percent of users.
#' @param format character: Response format to be requested from the API:
#' `csv` or `json`
#'
#' @return data.frame
#' @export
#'
#'
#' @examples
#' \dontrun{
#' povcalnet(country = "ALB")
#' }
#'
povcalnet <- function(country = "all",
                      povline = 1.9,
                      year = "all",
                      aggregate = FALSE,
                      fill_gaps = FALSE,
                      coverage = "all",
                      ppp = NULL,
                      url = "http://iresearch.worldbank.org",
                      format = "csv") {

  # STEP 1: build query string
  query <- build_query_string(
    country = country,
    povline = povline,
    year = year,
    aggregate = aggregate,
    fill_gaps = fill_gaps,
    coverage = coverage,
    ppp = ppp,
    format = format
  )

  # Special case handling WORLD level aggregate
  # This is necessary because of the behavior of the PovcalNet API
  # Should ideally be removed. Breaks the logic of the package
  if (length(country) == 1 & "all" %in% country & aggregate == TRUE) {
    out <- povcalnet_wb(povline = povline,
                        year = year,
                        url = url,
                        format = format)
    out <- out[out$regioncode == "WLD", ]

    return(out)
  }

  # STEP 2: build URL
  url <- httr::modify_url(url, path = api_handle(), query = query)

  # STEP 3: retrieve data
  res <- httr::GET(url = url)
  res <- httr::content(res, as = "text", encoding = "UTF-8" )

  # STEP 4: parse response
  if (res == "") {
    out <- handle_empty_response(res, aggregate = aggregate)
  } else {

    if (format == "json") {
      out <- tibble::as_tibble(jsonlite::fromJSON(res, simplifyDataFrame = TRUE))
      out <- out$PovResult
    } else {
      out <- readr::read_csv(res)
    }
  }

  # STEP 5: format output
  out <- format_data(out,
                     coverage = coverage,
                     aggregate = aggregate)

  return(out)
}

