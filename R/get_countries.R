#' get_countries
#'
#' Helper function to quickly retrieve the list of iso3 country codes for a
#' specific World Bank region or income group
#'
#' @param region_code character: A region or income group code. Only accepts one
#' code
#'
#' @return a vector of country code
#' @export
#'
#' @examples
#' get_countries("ECA")
get_countries <- function(region_code) {

  assertthat::assert_that(length(region_code) == 1,
                          msg = "Please only use one region code at a time")

  if (region_code %in% wbregion) {
    out <- wbregion_lkup$country_code[wbregion_lkup$wb_region == region_code]
  } else if (region_code %in% income_region) {
    out <- income_lkup$country_code[income_lkup$income_region == region_code]
  } else {
    stop(paste0(region_code, " was not found. Please make sure you use one of the following codes:
                   - WB regions: ", paste(wbregion, collapse = "; "), "
                   - Income groups: ", paste(income_region, collapse = "; ")))
  }

  return(out)

}
