
test_that("Check that subsites are up to date", {

  subsite_list <- qualtRics::survey_questions("SV_5bBw9H3DUZeBuTA") |>
    dplyr::select(qname)
  subsites_check <- subsite_list |>
    dplyr::slice(6:43) |>
    dplyr::pull()

  current_subsites <- c("ar_osceola",
                        "ca_santa_ana",
                        "il_cps",
                        "district6",
                        "district6_other",
                        "district9",
                        "district9_other",
                        "district11",
                        "district11_other",
                        "district12",
                        "district12_other",
                        "district13",
                        "district13_other",
                        "district16",
                        "district16_other",
                        "district17",
                        "district17_other",
                        "district25",
                        "district25_other",
                        "district75",
                        "district75_other",
                        "ma_boston",
                        "ma_dennis_yarm",
                        "ma_milford",
                        "ma_plymouth",
                        "ma_uxbridge",
                        "ma_westspring",
                        "ms_kemper",
                        "nj_great_oaks",
                        "ny_ascend",
                        "ny_cuny",
                        "ny_lion",
                        "ny_transfer",
                        "tn_acceleration",
                        "tn_acceleration2",
                        "tx_elpaso",
                        "tx_la_joya",
                        "wi_milwaukee")

  expect_true(length(intersect(subsites_check, current_subsites)) == length(current_subsites))
  # setdiff(subsites_check, current_subsites)
  # subsites <- current_subsites |>
  #   setNames(c("AR_Osceola School District",
  #              "CA_Santa Ana Unified School District",
  #              "IL_CPS",
  #              "NY_D6", "NY_D6 Other",
  #              "NY_D9", "NY_D9 Other",
  #              "NY_D11", "NY_D11 Other",
  #              "NY_D12", "NY_D12 Other",
  #              "NY_D13", "NY_D13 Other",
  #              "NY_D16", "NY_D16 Other",
  #              "NY_D17", "NY_D17 Other",
  #              "NY_D25", "NY_D25 Other",
  #              "NY_D75", "NY_D75 Other",
  #              "MA_Boston",
  #              "MA_Dennis-Yarmouth",
  #              "MA_Milford",
  #              "MA_Plymouth",
  #              "MA_Uxbridge",
  #              "MA_West Springfield",
  #              "MS_Kemper County School District",
  #              "NJ_Great Oaks Legacy Charter Schools",
  #              "NY_Ascend Charter Schools",
  #              "NY_CUNY/UA",
  #              "NY_LION Charter Schools",
  #              "NY_Transfer High Schools",
  #              "TN_Acceleration for All",
  #              "TN_Acceleration for All",
  #              "TX_El Paso Leadership Academy",
  #              "TX_La Joya ISD",
  #              "WI_Milwaukee Public Schools"))
  # usethis::use_data(subsites, overwrite = TRUE)
})


