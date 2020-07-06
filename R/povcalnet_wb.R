#' povcalnet_wb
#' Returns the official WB regional estimates
#'
#' @param povline numeric: poverty line (in 2011 PPP-adjusted USD) to
#' calculate poverty measures
#' @param popshare numeric: Share of population to calculate poverty line
#' @param year numeric:  list of years, or `all`.
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
#' povcalnet_wb(year = 2015)
#' }
povcalnet_wb <- function(povline   = NULL,
                         popshare  = NULL,
                         year      = "all",
                         server    = NULL,
                         format    = "csv") {
  # condition if povline and pop share are null
  if(is.null(povline) & is.null(popshare)) {
    povline <- 1.9
    message(paste("default poverty line is", povline))
  }


  # If povline and popshare are determined
  if(!is.null(povline) & !is.null(popshare)) {
    stop("You must select either `povline` or `popshare` but no both")
  }


  #----------------------------------------------------------
  # if popshare selected.
  #----------------------------------------------------------

  if(!is.null(popshare)) {
    df <- povcalnet_wb_qp(popshare  = popshare,
                          year      = year,
                          server    = server,
                          format    = format)
    return(df)
  }


  #----------------------------------------------------------
  #   if povline is selected
  #----------------------------------------------------------



  # Get URL
  url <- pt_geturl(server = server)

  # STEP 1: Build query
  query <- paste0("GroupedBy=WB&YearSelected=", year, "&PovertyLine=", povline, "&Countries=all&format=", format)

  # STEP 2: build URL
  url <- httr::modify_url(url, path = api_handle(server), query = query)

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
                     aggregate = TRUE,
                     format = format)

  return(out)
}
