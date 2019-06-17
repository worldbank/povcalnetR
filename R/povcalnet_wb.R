#' povcalnet_wb
#' Returns the official WB regional estimates
#'
#' @param povline numeric: poverty line (in 2011 PPP-adjusted USD) to
#' calculate poverty measures
#' @param year numeric:  list of years, or `all`.
#' @param url character: API root URL. For testing purposes only, should not be
#' changed for 99 percent of users.
#' @param format character: Response format to be requested from the API:
#' `csv` or `json`
#'
#' @return
#' @export
#'
#' @examples
povcalnet_wb <- function(povline = 1.9,
                         year = "all",
                         url = "http://iresearch.worldbank.org",
                         format = "csv") {

  # STEP 1: Build query
  query <- paste0("GroupedBy=WB&YearSelected=", year, "&PovertyLine=", povline, "&Countries=all&format=", format)

  # STEP 2: build URL
  url <- httr::modify_url(url, path = api_handle(), query = query)

  # STEP 3: retrieve data
  res <- httr::GET(url = url)
  res <- httr::content(res, as = "text", encoding = "UTF-8" )

  # STEP 4: parse response
  if (res == "") {
    out <- handle_empty_response(res, aggregate = TRUE)
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
                     coverage = "all",
                     aggregate = TRUE)

  return(out)
}
