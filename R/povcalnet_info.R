#' povcalnet_info()
#'
#' Download updated information on available countries, regions, and surveys from the PovcalNet API
#'
#' @param url character: This parameter should not be modified.
#'
#' @return data frame
#' @export
#'
#' \dontrun{
#' @examples
#' povcalnet_info()
#' }
povcalnet_info <- function(url = "http://iresearch.worldbank.org") {

  url <- paste0(url, "/PovcalNet/js/initCItem2014.js")

  x <- readLines(con = url)
  x <- js::esprima_parse(x)
  x <- jsonlite::fromJSON(x, simplifyVector = TRUE)
  x <- x[["body"]][["declarations"]][[1]][["init"]][["elements"]][[1]][["arguments"]]
  x <- purrr::map(x, "value")
  x <- purrr::map(x, function(x) as.data.frame(matrix(x, ncol = 12, byrow = TRUE), stringsAsFactors = FALSE))
  x <- dplyr::bind_rows(x)
  names(x) <- c("country_code",
                "wb_region",
                "un_region",
                "income_region",
                "country_name",
                "coverage_type",
                "n1",
                "n2",
                "n3",
                "n4",
                "n5",
                "year"
  )

  x <- tidyr::separate_(x, "country_code", into = c("country_code", "coverage_code"))
  x$country_code <- stringr::str_sub(x$country_code, start = 1L, end = 3L)
  x$coverage_level <- coverage_level_lkup[x$coverage_type]
  x$coverage_type <- coverage_type_lkup[x$coverage_type]

  x <- x[, c("country_code",
             "country_name",
             "wb_region",
             "un_region",
             "income_region",
             "coverage_level",
             "coverage_type",
             "coverage_code",
             "year")]


  x$year <- lapply(x$year, function(y) floor(as.numeric(strsplit(y, split =",", fixed = TRUE)[[1]])))

  return(x)

}
