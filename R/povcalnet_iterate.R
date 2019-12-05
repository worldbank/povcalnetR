# ==================================================
# project:       Iterate to find moentary threshold of percentile
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2011-11-25
# Modification Date:
# Script version:    01
# References:
#
#
# Output:          Data frame
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("tidyverse")
library("povcalnetR")


#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------

modul <- function(x, y) {
  x - y * floor(x / y)
}

povcalnet_iterate <- function(country = NULL,
                              region = NULL,
                              goal = 0.5,
                              year = 2015,
                              coverage = "national",
                              pl =  1,
                              tolerance = 5,
                              ni = 40,
                              delta = 3,
                              aggregate = FALSE,
                              fill_gaps = TRUE) {
  #----------------------------------------------------------
  #   initial conditions
  #----------------------------------------------------------

  #--------- region and country


  if ((length(country) & length(region)) |
      (!length(country) & !length(region))) {
    stop("you must select either a `region` or a `country`")
  }

  wb_regions <- c("ECA", "MNA", "SSA", "LAC", "OHI", "SAS", "EAP", "WLD", "SAR")
  in_regions <- c("UMC", "LMC", "HIC", "LIC")


  if (length(region) > 0) {
    if ((region  %in% wb_regions) | (region  %in% in_regions)) {
      wb <- 1
      ccc <- region
      if (region == "SAR") {
        region <- "SAS"
      }
    } else {
      stop(paste0(region, " was not found. Please make sure you use one of the following codes:\n
                   - WB regions: ECA; MNA; SSA; LAC; OHI; SAS; EAP: WLD\n
                   - Income groups: UMC; LMC; HIC; LIC"))
    }

  } else {
    wb <- 0
    ccc <-  country

    if (country == "ARG") {
      coverage <- "urban"
    }
  }


  #--------- parameters


  s          <- 0    # iteration stage counter
  num        <- 1    # numerator
  i          <- 0    # general counter
  status     <- "OK"

  #----------------------------------------------------------
  #   main calculations
  #----------------------------------------------------------

  #--------- handling errors

  tryCatch(
    expr = {
      # Your code...
      #--------- First call

      if (wb == 1) {
        attempt <- povcalnet_wb(
          povline = pl,
          year = year
        ) %>%
          filter(regioncode == region) %>%
          select(headcount) %>% pull
      } else {
        attempt <- povcalnet(
          country = country,
          povline = pl,
          year = year,
          coverage = coverage,
          aggregate = aggregate,
          fill_gaps = fill_gaps
        ) %>%
          select(headcount) %>% pull
      }


      #--------- in case there is no data for requested year

      if (length(attempt) == 0) {
        s <- ni + 1 # avoid the while loop
        attempt <- 0
        goal <-  NA
        pl <-  NA
        status <- "No data"
      }

      #----------------------------------------------------------
      #   start looping
      #----------------------------------------------------------

      while (round(attempt, digits = tolerance) != goal & s < ni) {
        i <-  i + 1

        if (attempt < goal) {
          # before crossing goal
          while (pl + delta < 0) {
            delta <-  delta * 2
          }
          pl <- pl + delta
          below <- 1
        }

        if (attempt > goal) {
          # after crossing goal
          while (pl - delta < 0) {
            delta <- delta / 2
          }
          pl <- pl - delta
          below <-  0
        }

        # call data
        if (wb == 1) {
          attempt <- povcalnet_wb(
            povline = pl,
            year = year
          ) %>%
            filter(regioncode == region) %>%
            select(headcount) %>% pull
        } else {
          attempt <- povcalnet(
            country = country,
            povline = pl,
            year = year,
            coverage = coverage,
            aggregate = aggregate,
            fill_gaps = fill_gaps
          ) %>%
            select(headcount) %>% pull
        }

        # assess if the value of delta has to chanbe
        if ((attempt > goal & below == 1) |
            (attempt < goal & below == 0)) {
          s <- s + 1

          if (modul(s, 2)) {
            one <- -1
          } else {
            one = 1
          }


          num <- (2 * num) + one
          den <- 2 ^ s
          delta <- (num / den) * delta

        }  # end of condition to change the value of delta
      }  # end of while

      #----------------------------------------------------------
      #   final datafrane
      #---------------------------------------------------------


      fdf  <- tibble(
        countrycode = ccc,
        year = year,
        goal = goal*100,
        threshold = pl,
        status = status
      )

      return(fdf)
    },

    error = function(e) {
      # Do this if an error is caught...
      fdf  <- tibble(
        countrycode = ccc,
        year = year,
        goal = NA,
        threshold = NA,
        status = paste("Error:",e$message)
      )
      return(fdf)
    },
    finally = {
      # Do this at the end before quitting the tryCatch structure...
      print(paste("done with", ccc, year))
    }
  ) # End of tryCatch


}  # End of function povcalnet_iterate
