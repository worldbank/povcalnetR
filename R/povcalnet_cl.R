povcalnet_cl <- function(country,
                         poverty_line,
                         year,
                         coverage = NULL,
                         ppp = NULL,
                         url = "http://iresearch.worldbank.org",
                         format = "csv") {

  # STEP 1: build query string
  query <- build_country_level_string(
    country = country,
    poverty_line = poverty_line,
    year = year,
    coverage_type = coverage,
    ppp = ppp,
    format = format
  )

  # STEP 2: build URL
  url <- httr::modify_url(url, path = api_handle(), query = query)

  # STEP 3: retrieve data
  res <- httr::GET(url = url)
  res <- httr::content(res,as = "text", encoding = "UTF-8" )

  # STEP 4: parse data
  if (format == "json") {
    out <- tibble::as_tibble(jsonlite::fromJSON(res, simplifyDataFrame = TRUE))
    out <- out$PovResult
  } else {
    out <- readr::read_csv(res)
  }

  # STEP 5: format output
  out <- format_data(out, coverage = coverage, aggregate = FALSE)

  return(out)

}

