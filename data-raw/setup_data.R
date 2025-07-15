# Content Area ----
content_area_sites <- c("NY_D9", "NY_D12", "NY_D13", "NY_D16")
usethis::use_data(content_area_sites, overwrite = TRUE)

# Subsites ----
# Maps qualtrics variable names to L&R site names
subsites <- current_subsites |>
  setNames(c("AR_Osceola School District",
             "CA_Santa Ana Unified School District",
             "IL_CPS",
             "NY_D6", "NY_D6 Other",
             "NY_D9", "NY_D9 Other",
             "NY_D11", "NY_D11 Other",
             "NY_D12", "NY_D12 Other",
             "NY_D13", "NY_D13 Other",
             "NY_D16", "NY_D16 Other",
             "NY_D17", "NY_D17 Other",
             "NY_D25", "NY_D25 Other",
             "NY_D75", "NY_D75 Other",
             "MA_Boston",
             "MA_Dennis-Yarmouth",
             "MA_Milford",
             "MA_Plymouth",
             "MA_Uxbridge",
             "MA_West Springfield",
             "MS_Kemper County School District",
             "NJ_Great Oaks Legacy Charter Schools",
             "NY_Ascend Charter Schools",
             "NY_CUNY/UA",
             "NY_LION Charter Schools",
             "NY_Transfer High Schools",
             "TN_Acceleration for All",
             "TN_Acceleration for All",
             "TX_El Paso Leadership Academy",
             "TX_La Joya ISD",
             "WI_Milwaukee Public Schools"))
usethis::use_data(subsites, overwrite = TRUE)
