#' povcalnet_cl
#' Facilitate interaction with the PovcalNet API. All parameters are specified
#' at the country level (hence the _cl suffix), so if you send a request for two
#' countries, you must also pass two values to the `year` argument, two values
#' to the `povline` argument, etc.
#'
#' @param country character: list of country iso3 code (accepts multiple).
#' Use \code{\link{povcalnet_info}} for full list of countries.
#' @param povline numeric: poverty line (in 2011 PPP-adjusted USD) to
#' calculate poverty measures
#' @param year numeric:  list of years, or `all`, or `last`.
#' @param coverage character: Can take one of three values: `national`, `urban`, `rural`
#' @param ppp numeric: Optional - Allows the selection of custom PPP (Purchasing Power Parity) exchange rates
#' @param server character: Key for API root URL. For testing purposes only, should not be
#' changed for 99 percent of users.
#' @param format character: Response format to be requested from the API:
#' `csv` or `json`
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \donttest{
#' povcalnet_cl(country = c("ALB", "USA"), povline = c(3.2, 5.5), year = c(2012, 2010))
#' }

povcalnet_cl <- function(country,
                         povline,
                         year,
                         coverage  = NULL,
                         ppp       = NULL,
                         server    = NULL,
                         format    = "csv") {
  # Get URL
  url <- pt_geturl(server = server)

  # STEP 1: build query string
  query <- build_query_string_cl(
    country = country,
    povline = povline,
    year = year,
    coverage_type = coverage,
    ppp = ppp,
    format = format
  )

  # STEP 2: build URL
  url <- httr::modify_url(url, path = api_handle(server), query = query)

  # STEP 3: retrieve data
  res <- httr::GET(url = url)
  res <- httr::content(res, as = "text", encoding = "UTF-8" )

  # STEP 4: parse data
  if (format == "json") {
    out <- tibble::as_tibble(jsonlite::fromJSON(res, simplifyDataFrame = TRUE))
    out <- out$PovResult
  } else {
    out <- readr::read_csv(res)
  }

  # STEP 5: format output
  out <- format_data(out,
                     coverage = coverage,
                     aggregate = FALSE)

  return(out)

}
