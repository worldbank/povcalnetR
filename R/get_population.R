#' get_population
#'
#' @param server character: Key for API root URL. For testing purposes only, should not be
#' changed for 99 percent of users.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \donttest{
#' get_population()
#' }

get_population <- function(server = NULL) {

  # STEP 1: Build request URL
  pop_url <- pt_geturl(server = server)
  tmp_handle <- api_handle(server = server)
  tmp_handle <- stringr::str_replace(tmp_handle,
                                     pattern = "povcalnetapi.ashx",
                                     replacement = "js/population.js")
  pop_url <- paste0(pop_url, "/", tmp_handle)

  # STEP 2 Read file from server
  pop <- readr::read_lines(file = url(pop_url))

  # STEP 3: clean input file
  # Remove extra lines, columns and characters
  pop <- pop[2:(length(pop) - 1)]
  pop <- purrr::map(pop, stringr::str_replace_all, pattern = "\\[", replacement = "")
  pop <- purrr::map(pop, stringr::str_replace_all, pattern = "\\]", replacement = "")
  pop <- purrr::map(pop, stringr::str_replace_all, pattern = ",$", replacement = "")
  # Remove country names to avoid parsing issues
  pop <- purrr::map(pop, stringr::str_replace, pattern = "\".+?\",", replacement = "")
  pop <- purrr::map(pop, stringr::str_replace_all, pattern = "\"", replacement = "")
  # Parse string into a vector
  pop <- purrr::map(pop, stringr::str_split, pattern = ",", simplify = FALSE)
  pop <- purrr::flatten(pop)
  # remove rows storing region names
  keep <- purrr::map_lgl(pop, ~ length(.x) > 1)
  pop <- pop[keep]

  # STEP 4: Combine into a single data.frame
  # Extract column names
  column_names <- pop[[1]]
  column_names[1] <- "countrycode"
  pop <- pop[-1]
  pop <- purrr::map(pop, function(x) {
    names(x) <- column_names
    return(x)})
  # Combine into a single table
  out <- dplyr::bind_rows(pop)
  out[, 2:ncol(out)] <- lapply(out[, 2:ncol(out)], as.numeric)

  return(out)
}
