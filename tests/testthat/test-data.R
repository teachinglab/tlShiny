
test_that("Check that subsites are up to date", {

  subsite_list <- qualtRics::survey_questions("SV_djt8w6zgigaNq0C") |>
    tidytable::select(qname)
  subsites_check <- subsite_list |>
    tidytable::slice(8:47) |>
    tidytable::pull()

  current_subsites <- c("ar_blytheville",
                        "ar_friendship",
                        "ar_hope",
                        "ar_osceola",
                        "district6",
                        "district7",
                        "district7_other",
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
                        "district25",
                        "district25_other",
                        "district27",
                        "district27_other",
                        "district75",
                        "district75_other",
                        "east_harlem",
                        "la_pointe_coupee",
                        "ma_dese",
                        "ms_kemper",
                        "nc_charlotte",
                        "network4",
                        "network7",
                        "network12",
                        "nj_great_oaks",
                        "nm_nm_ped",
                        "ny_ascend",
                        "ny_cuny",
                        "ny_transfer",
                        "oh_cleveland",
                        "rochester",
                        "tx_elpaso",
                        "wi_milwaukee")

  expect_true(length(setdiff(subsites_check, current_subsites)) == 0)
  # setdiff(subsites_check, current_subsites)
  # subsites <- current_subsites |>
  #   setNames(c("AR_Blytheville School District", "AR_Friendship Aspire Academy", "AR_Hope Public Schools", "AR_Osceola School District",
  #              "NY_D6",
  #              "NY_D7", "NY_D7 Other",
  #              "NY_D9", "NY_D9 Other",
  #              "NY_D11", "NY_D11 Other",
  #              "NY_D12", "NY_D12 Other",
  #              "NY_D13", "NY_D13 Other",
  #              "NY_D16", "NY_D16 Other",
  #              "NY_D25", "NY_D25 Other",
  #              "NY_D27", "NY_D27 Other",
  #              "NY_D75", "NY_D75 Other",
  #              "NY_East Harlem Scholar Academies (EHTP)",
  #              "LA_Pointe Coupee Parish",
  #              "MA_DESE",
  #              "MS_Kemper County School District",
  #              "NC_Charlotte-Mecklenburg Schools",
  #              "IL_Chicago Public Schools_Network 4",
  #              "IL_Chicago Public Schools_Network 7",
  #              "IL_Chicago Public Schools_Network 12",
  #              "NJ_Great Oaks Legacy Charter Schools",
  #              "NM_NM Public Education Department",
  #              "NY_Ascend Charter Schools",
  #              "NY_CUNY/UA",
  #              "NY_Transfer High Schools",
  #              "OH_Cleveland Metro School District",
  #              "NY_Rochester City Schools",
  #              "TX_El Paso Leadership Academy",
  #              "WI_Milwaukee Public Schools"))
  # usethis::use_data(subsites, overwrite = TRUE)
})


