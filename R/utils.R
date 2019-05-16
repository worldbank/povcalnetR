# Helpers
api_handle <- function() "povcalnet/povcalnetapi.ashx"


format_data <- function(x) {
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
                     "req_year_population" = "ReqYearPopulation",
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

  x$data_type <- datatype_lkup[x$data_type]

  return(x)
}


