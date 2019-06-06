# Helpers
api_handle <- function() "povcalnetapi.ashx"

format_data <- function(x, country, coverage, aggregate) {

  if (aggregate == FALSE){
    x <- format_data_cl(x = x,
                        country = country,
                        coverage = coverage)
  } else {
    x <- format_data_aggregate(x = x)
  }
  return(x)
}



format_data_cl <- function(x, country, coverage) {
  # CHECK
  assertthat::assert_that(
    all.equal(names(x), povcal_col_names)
  )

  x <- dplyr::select(x,
                     "country_code" = "CountryCode",
                     "country_name" = "CountryName",
                     "region_code" = "RegionCode",
                     "coverage_type" = "CoverageType",
                     "request_year" = "RequestYear",
                     "data_year" = "DataYear",
                     "data_type" = "DataType",
                     "is_interpolated" = "isInterpolated",
                     "use_microdata" = "useMicroData",
                     "ppp" = "PPP",
                     "poverty_line" = "PovertyLine",
                     "mean" = "Mean",
                     "headcount" = "HeadCount",
                     "poverty_gap" = "PovGap",
                     "poverty_gap_sq" = "PovGapSqr",
                     "watts" = "Watts",
                     "gini" = "Gini",
                     "median" = "Median",
                     "mld" = "pr.mld",
                     "polarization" = "Polarization",
                     "population" = "ReqYearPopulation",
                     "decile1" = "Decile1",
                     "decile2" = "Decile2",
                     "decile3" = "Decile3",
                     "decile4" = "Decile4",
                     "decile5" = "Decile5",
                     "decile6" = "Decile6",
                     "decile7" = "Decile7",
                     "decile8" = "Decile8",
                     "decile9" = "Decile9",
                     "decile10" = "Decile10"
  )

  # rename data_type to be more explicit
  x$data_type <- datatype_lkup[x$data_type]

  # Filter out coverage level that were not requested
  # Needed when country = "all" is specified (returns all coverage level by default)
  if (all(!is.null(coverage) & length(coverage) == 1 & country == "all")) {
    x <- x[x$coverage_type %in% names(coverage_level_lkup[coverage_level_lkup == coverage]), ]
  }

  # replace invalid values to missing
  rvars <-
    c("median", "polarization", "gini", "mld",
      stringr::str_subset(names(x), "^decile"))

  x <- x %>%
    naniar::replace_with_na_at(.vars = rvars,
                               condition = ~.x  %in% c(-1, 0))

  return(x)
}


format_data_aggregate <- function(x) {
  # CHECK
  assertthat::assert_that(
    all.equal(names(x), povcal_col_names_agg)
  )

  x <- dplyr::select(x,
                     "region_title" = "regionTitle",
                     "region_code" = "regionCID",
                     "request_year" = "requestYear",
                     "poverty_line" = "povertyLine",
                     "mean" = "mean",
                     "headcount" = "hc",
                     "poverty_gap" = "pg",
                     "poverty_gap_sq" = "p2",
                     "population" = "population"
  )

  return(x)
}


# format_data_dist <- function(x) {
#   # CHECK
#   assertthat::assert_that(
#     all.equal(names(x), c("i", "P", "L", "X4"))
#   )
#
#   x <- x[, c("i", "P", "L")]
#
# }

assign_countries <- function(country) {

  if (country == "all") {
    country <- all_countries
  } else {
    country
  }
}
