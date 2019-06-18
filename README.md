
<!-- README.md is generated from README.Rmd. Please edit that file -->

# povcalnetR

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/povcalnetR)](https://cran.r-project.org/package=povcalnetR)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.org/tonyfujs/povcalnetR.svg?branch=master)](https://travis-ci.org/tonyfujs/povcalnetR)
<!-- badges: end -->

The `povcalnetR` package allows R users to compute poverty and
inequality indicators for more than 160 countries and regions from the
World Bank’s database of household surveys. It has the same
functionality as the [PovcalNet
website](http://iresearch.worldbank.org/PovcalNet/home.aspx). PovcalNet
is a computational tool that allows users to estimate poverty rates for
regions, sets of countries or individual countries, over time and at any
poverty line.

PovcalNet is managed jointly by the Data and Research Group in the World
Bank’s Development Economics Division. It draws heavily upon a strong
collaboration with the Poverty and Equity Global Practice, which is
responsible for the gathering and harmonization of the underlying survey
data.

## Installation

The development version can be installed from
[GitHub](https://github.com/) with:

``` r
install.packages(c("devtools", "httr"))
devtools::install_github("tonyfujs/povcalnetR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(povcalnetR)

## basic example code
povcalnet(country = c("ALB", "CHN"))
#> # A tibble: 53 x 31
#>    country_code country_name region_code coverage_type request_year
#>    <chr>        <chr>        <chr>       <chr>                <dbl>
#>  1 ALB          Albania      ECA         N                     1996
#>  2 ALB          Albania      ECA         N                     2002
#>  3 ALB          Albania      ECA         N                     2005
#>  4 ALB          Albania      ECA         N                     2008
#>  5 ALB          Albania      ECA         N                     2012
#>  6 CHN          China        EAP         A                     1981
#>  7 CHN          China        EAP         A                     1984
#>  8 CHN          China        EAP         A                     1987
#>  9 CHN          China        EAP         A                     1990
#> 10 CHN          China        EAP         A                     1993
#> # ... with 43 more rows, and 26 more variables: data_year <dbl>,
#> #   data_type <chr>, is_interpolated <dbl>, use_microdata <dbl>,
#> #   ppp <dbl>, poverty_line <dbl>, mean <dbl>, headcount <dbl>,
#> #   poverty_gap <dbl>, poverty_gap_sq <dbl>, watts <dbl>, gini <dbl>,
#> #   median <dbl>, mld <dbl>, polarization <dbl>, population <dbl>,
#> #   decile1 <dbl>, decile2 <dbl>, decile3 <dbl>, decile4 <dbl>,
#> #   decile5 <dbl>, decile6 <dbl>, decile7 <dbl>, decile8 <dbl>,
#> #   decile9 <dbl>, decile10 <dbl>
```
