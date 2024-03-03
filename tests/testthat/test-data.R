
test_that("Check that subsites are up to date", {

  subsite_list <- qualtRics::survey_questions("SV_djt8w6zgigaNq0C") |>
    tidytable::select(qname)
  subsites_check <- subsite_list |>
    tidytable::slice(8:40) |>
    tidytable::pull()

  current_subsites <- c("ar_blytheville",
                        "ar_friendship",
                        "ar_hope",
                        "ar_osceola",
                        "district6",
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
                        "district27",
                        "district27_other",
                        "district75",
                        "district75_other",
                        "la_pointe_coupee",
                        "ma_dese",
                        "ms_kemper",
                        "nc_charlotte",
                        "network4",
                        "network7",
                        "network12",
                        "nm_nm_ped",
                        "ny_ascend",
                        "ny_cuny",
                        "ny_transfer",
                        "rochester",
                        "tx_elpaso",
                        "wi_milwaukee")

  expect_true(identical(current_subsites, subsites_check))
})


