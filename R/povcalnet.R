#' povcalnet
#' Facilitate interaction with the PovcalNet API
#'
#' @param country character: list of country iso3 code (accepts multiple) or
#' `all`. Use \code{\link{povcalnet_info}} for full list of countries.
#' @param povline numeric: poverty line (in 2011 PPP-adjusted USD) to
#' calculate poverty measures
#' @param popshare numeric: Share of population to calculate poverty line
#' @param year numeric:  list of years, or `all`.
#' @param aggregate logical: `TRUE` will return aggregate results,
#' `FALSE` country-level results.
#' @param fill_gaps logical: `TRUE` will interpolate / extrapolate values when
#' surveys are not available for a specific year.
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
#'
#' @examples
#' \donttest{
#' povcalnet(country = "ALB")
#' }
#'
povcalnet <- function(country   = "all",
                      povline   = NULL,
                      popshare  = NULL,
                      year      = "all",
                      aggregate = FALSE,
                      fill_gaps = FALSE,
                      coverage  = "all",
                      ppp       = NULL,
                      server    = NULL,
                      format    = "csv") {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   Conditions   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # condition if povline and pop share are null
  if(is.null(povline) & is.null(popshare)) {
    povline <- 1.9
    message(paste("default poverty line is", povline))
  }

  # If povline and popshare are determined
  if(!is.null(povline) & !is.null(popshare)) {
    stop("You must select either `povline` or `popshare` but no both")
  }

  if(aggregate == TRUE & !is.null(popshare)) {
    msg     <- "`aggegate` can't be TRUE at the same time that `popshare` is defined"
    problem <- paste("you specified `aggregate` as TRUE and `popshare` as", popshare)
    rlang::abort(c(
                  msg,
                  x = problem
                  ),
                  class = "error_class"
                  )
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   Steps   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # STEP 1: build query string

  if (!is.null(popshare)) {
    query <- build_query_string_qp(
      country   = country,
      popshare  = popshare,
      year      = year,
      aggregate = aggregate,
      fill_gaps = fill_gaps,
      coverage  = coverage,
      ppp       = ppp,
      format    = format
    )

  } else {
    query <- build_query_string(
      country   = country,
      povline   = povline,
      year      = year,
      aggregate = aggregate,
      fill_gaps = fill_gaps,
      coverage  = coverage,
      ppp       = ppp,
      format    = format
    )
  }


  # Special case handling WORLD level aggregate
  # This is necessary because of the behavior of the PovcalNet API
  # Should ideally be removed. Breaks the logic of the package
  if (length(country) == 1 & "all" %in% country & aggregate == TRUE) {
    out <- povcalnet_wb(povline  = povline,
                        year     = year,
                        server   = server,
                        format   = format)

    out <- out[out$regioncode == "WLD", ]

    return(out)
  }

  # STEP 2: build URL
  url <- pt_geturl(server = server)
  url <- httr::modify_url(url, path = api_handle(server), query = query)

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
                     aggregate = aggregate,
                     format = format)

  return(out)
}
