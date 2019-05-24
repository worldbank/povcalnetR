library(tidyverse)

coverage_lkup <- c(1, 2, 3, 4, 5, 6)
names(coverage_lkup) <- c("rural", "urban", "national", "special_cpi", "aggregated_distribution", "invalid_ppp")

coverage_type_lkup <- c("rural", "urban", "national", "national aggregate")
names(coverage_type_lkup) <- c("R", "U", "N", "A")

coverage_level_lkup <- c("rural", "urban", "national", "national")
names(coverage_level_lkup) <- c("R", "U", "N", "A")

datatype_lkup <- c("consumption", "income", "mixed")
names(datatype_lkup) <- c("X", "Y", "Z")

povcal_info <- povcalnet_info()
df <- povcal_info %>%
  filter(coverage_level == "national") %>%
  select(country_code, coverage_code) %>%
  distinct
national_coverage_lkup <- df$coverage_code
names(national_coverage_lkup) <- df$country_code

wbregion_lkup <- povcal_info %>%
  select(country_code, wb_region) %>%
  distinct()
wbregion <- unique(wbregion_lkup$wb_region)

income_lkup <- povcal_info %>%
  select(country_code, income_region) %>%
  distinct()
income_region <- unique(income_lkup$income_region)

# povcal_col_names <- c("interpolated", "useMicroData", "CountryCode", "RegionCID",
#                       "CoverageType", "RequestYear",  "DataType", "PPP",
#                       "PovertyLine", "Mean", "HC", "pg", "Gini", "Median",
#                       "ReqYearPopulation", "DataYear", "SvyInfoID", "Decile")

povcal_col_names <- c("isInterpolated", "useMicroData", "CountryCode", "CountryName",
                      "RegionCode", "CoverageType", "RequestYear", "DataYear",
                      "DataType", "PPP", "PovertyLine", "Mean", "HeadCount",
                      "PovGap", "PovGapSqr", "Watts", "Gini", "Median", "pr.mld",
                      "Polarization", "ReqYearPopulation", "SvyInfoID", "Decile1",
                      "Decile2", "Decile3", "Decile4", "Decile5", "Decile6",
                      "Decile7", "Decile8", "Decile9", "Decile10")

povcal_col_names_agg <- c("requestYear", "regionTitle", "regionCID", "povertyLine",
                          "mean", "hc", "pg", "p2", "population")

# Save data ---------------------------------------------------------------

usethis::use_data(
  coverage_lkup,
  povcal_col_names,
  povcal_col_names_agg,
  datatype_lkup,
  coverage_type_lkup,
  coverage_level_lkup,
  national_coverage_lkup,
  wbregion_lkup,
  wbregion,
  income_lkup,
  income_region,

  internal = TRUE,
  overwrite = TRUE
)
