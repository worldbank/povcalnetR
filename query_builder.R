##### Query builder function
##### To fetch data from the PovcalNet API

##### Author: Ratnadeep Mitra

##### Parameters

#####       Common values 
#####       -------------
##### ***** poverty_line (string) - Can be one single value
##### ***** countries (string) - Can be one or multiple comma-separated values entered as one single string
##### ***** year_selected (string) - Can be one or multiple comma-separated values entered as one single string

#####       Specific values (For specific countries and poverty lines)
#####       ---------------
##### ***** poverty_line_list(string) - List of comma-separated values entered as one single string (can be one or more)
##### ***** countries_list(string) - List of comma-separated values entered as one single string (can be one or more)
##### ***** year_selected_list(string) - List of semicolon-separated values entered as one single string (can be one or more).
#####                                   Each country/PL can have multiple comma-separated years


##### Sample Queries

##### query_builder(poverty_line="1.9",
#####               year_selected="2010",
#####               countries="BGD,BTN,IND,MDV,NPL,PAK,LKA",
#####               year_selected_list="",
#####               poverty_line_list="",
#####               countries_list="")


##### query_builder(poverty_line="",
#####               year_selected="",
#####               countries="",
#####               year_selected_list="2002,2010;2010",
#####               poverty_line_list="1.25,1.25",
#####               countries_list="ALB,IND")



query_builder <- function(poverty_line,
                          countries,
                          year_selected,
                          poverty_line_list,
                          countries_list,
                          year_selected_list) {
  
  url <- "http://iResearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx?"
  query_string <- ""
  err_flag <- FALSE
  
  tryCatch({
    
    ####################### POVERTY LINE
    if (err_flag == FALSE) {
      if (is.na(poverty_line) || poverty_line == ""){
        if (is.na(poverty_line_list) || poverty_line_list == "") {
          err_flag <- TRUE
        }
        else {
          PL <- unlist(strsplit(poverty_line_list, split = ","))
          pstr <- ""
          for (i in 1:length(PL)){
            pstr <- paste0(pstr, paste0("PL", i-1, "=", PL[[i]], "&"))
          }
        }
        query_string <- paste0(query_string, pstr)
      }
      else {
        query_string <- paste0(query_string, paste0("PovertyLine=", poverty_line, "&"))
      }
    }
    
    ####################### COUNTRIES
    if (err_flag == FALSE) {
      if (is.na(countries) || countries == ""){
        if (is.na(countries_list) || countries_list == "") {
          err_flag <- TRUE
        }
        else {
          C <- unlist(strsplit(countries_list, split = ","))
          cstr <- ""
          for (i in 1:length(C)){
            cstr <- paste0(cstr, paste0("C", i-1, "=", C[[i]], "&"))
          }
        }
        query_string <- paste0(query_string, cstr)
      }
      else {
        query_string <- paste0(query_string, paste0("Countries=", countries, "&"))
      }
    }
    
    ####################### YEAR SELECTED
    if (err_flag == FALSE) {
      if (is.na(year_selected) || year_selected == ""){
        if (is.na(year_selected_list) || year_selected_list == "") {
          err_flag <- TRUE
        }
        else {
          Y <- unlist(strsplit(year_selected_list, split = ";"))
          ystr <- ""
          for (i in 1:length(Y)){
            ystr <- paste0(ystr, paste0("Y", i-1, "=", Y[[i]], "&"))
          }
        }
        query_string <- paste0(query_string, ystr)
      }
      else {
        query_string <- paste0(query_string, paste0("YearSelected=", year_selected, "&"))
      }
    }
    
    if (err_flag == FALSE) {
      query_string <- substr(query_string, 0, nchar(query_string) - 1)
      url <- paste0(url, query_string)
      print(url)
      df <- read.csv(url(url))
      write.csv(df, file = "api_data.csv")
    }
    else {
      print("Could not fetch data: Check parameters.")
    }
    
    
  }, error = function(err){
    stop("Execution stopped")
  })
  
  
}