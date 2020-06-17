country <- c("ALB", "CHN")
povline <- 1.9
year <- c(2002, 2012)
ppp <- c(100, 50)
aggregate <- FALSE
coverage <- "national"

test_that("Incorrect inputs trigger errors", {

  expect_error(build_query_string(country = country,
                                     povline = c(1.9, 2.0),
                                     year = year)
  )

  expect_error(build_query_string(country = country,
                                     povline = povline,
                                     year = year,
                                     ppp = 100)
  )

  expect_error(build_query_string(country = country,
                                     povline = povline,
                                     year = year,
                                     display = "urban")
  )

  expect_error(build_query_string(country = country,
                                  povline = povline,
                                  year = year,
                                  ppp = ppp,
                                  coverage = "all")
  )

})

test_that("Country level queries are built correctly", {
  query <- build_query_string(country = country,
                              povline = povline,
                              year = year)
  expect_equal(query, "SurveyYears=2002,2012&Countries=ALB_3,CHN_5,CHN_1,CHN_2&PovertyLine=1.9&display=C&format=json")

  query <- build_query_string(country = country,
                              povline = povline,
                              year = year,
                              ppp = ppp,
                              coverage = coverage)
  expect_equal(query, "SurveyYears=2002,2012&Countries=ALB_3,CHN_5&PovertyLine=1.9&PPP0=100&PPP1=50&display=C&format=json")

  query <- build_query_string(country = country,
                              povline = povline,
                              year = year,
                              ppp = ppp,
                              coverage = coverage,
                              aggregate = TRUE,
                              fill_gaps = TRUE)
  expect_equal(query, "YearSelected=2002,2012&Countries=ALB_3,CHN_5&PovertyLine=1.9&PPP0=100&PPP1=50&display=Regional&format=json")

  query <- build_query_string(country = "all",
                              povline = povline,
                              year = year)
  expect_equal(query, "SurveyYears=2002,2012&Countries=ALB_3,DZA_3,AGO_3,ARG_2,ARM_3,AUS_3,AUT_3,AZE_3,BGD_3,BLR_3,BEL_3,BLZ_3,BEN_3,BTN_3,BOL_3,BOL_2,BIH_3,BWA_3,BRA_3,BGR_3,BFA_3,BDI_3,CPV_3,CMR_3,CAN_3,CAF_3,TCD_3,CHL_3,CHN_5,CHN_1,CHN_2,COL_3,COL_2,COM_3,COD_3,COG_3,CRI_3,CIV_3,HRV_3,CYP_3,CZE_3,DNK_3,DJI_3,DOM_3,ECU_3,ECU_2,EGY_3,SLV_3,EST_3,SWZ_3,ETH_3,ETH_1,FJI_3,FIN_3,FRA_3,GAB_3,GMB_3,GEO_3,DEU_3,GHA_3,GRC_3,GTM_3,GIN_3,GNB_3,GUY_3,HTI_3,HND_3,HND_2,HUN_3,ISL_3,IND_5,IND_1,IND_2,IDN_5,IDN_1,IDN_2,IRN_3,IRQ_3,IRL_3,ISR_3,ITA_3,JAM_3,JPN_3,JOR_3,KAZ_3,KEN_3,KIR_3,KOR_3,XKX_3,KGZ_3,LAO_3,LVA_3,LBN_3,LSO_3,LBR_3,LTU_3,LUX_3,MDG_3,MWI_3,MYS_3,MDV_3,MLI_3,MLT_3,MRT_3,MUS_3,MEX_3,FSM_3,FSM_2,MDA_3,MNG_3,MNE_3,MAR_3,MOZ_3,MMR_3,NAM_3,NPL_3,NLD_3,NIC_3,NER_3,NGA_3,MKD_3,NOR_3,PAK_3,PAN_3,PNG_3,PRY_3,PER_3,PHL_3,POL_3,PRT_3,ROU_3,RUS_3,RWA_3,RWA_1,WSM_3,STP_3,SEN_3,SRB_3,SYC_3,SLE_3,SVK_3,SVN_3,SLB_3,ZAF_3,SSD_3,ESP_3,LKA_3,LCA_3,SDN_3,SUR_3,SWE_3,CHE_3,SYR_3,TWN_3,TJK_3,TZA_3,THA_3,TLS_3,TGO_3,TON_3,TTO_3,TUN_3,TUR_3,TKM_3,TUV_3,UGA_3,UKR_3,ARE_3,GBR_3,USA_3,URY_3,URY_2,UZB_3,VUT_3,VEN_3,VNM_3,PSE_3,YEM_3,ZMB_3,ZWE_3&PovertyLine=1.9&display=C&format=json")

  query <- build_query_string(country = country,
                              povline = povline,
                              year = year,
                              coverage = coverage)
  expect_equal(query, "SurveyYears=2002,2012&Countries=ALB_3,CHN_5&PovertyLine=1.9&display=C&format=json")
})
