library(dplyr)

make_exp_df <- function(exp_lev){
  if(exp_lev == 1){
    lev6 <- rev(c(5, 10, 10, 25, 25, 25))
  }
  if(exp_lev == 2){
    lev6 <- rev(c(10, 12, 30, 25, 10, 5))
  }
  if(exp_lev == 3){
    lev6 <- c(5, 10, 10, 25, 25, 25)
  }

  exp_df_out <- tribble(
    ~scenario_name, ~MAT_1, ~MAT_2, ~MAT_3, ~MAT_4, ~MAT_5, ~MAT_6, ~CMD_1, ~CMD_2, ~CMD_3, ~CMD_4, ~CMD_5, ~CMD_6, ~CCEI_1, ~CCEI_2, ~CCEI_3, ~CCEI_4, ~perc_non_breed_not_over_ccei, ~HTN_1, ~HTN_2, ~HTN_3, ~HTN_4, ~PTN, ~MAP_max, ~MAP_min, ~range_change, ~range_overlap, ~range_size,
    "Scn1", lev6[1], lev6[2], lev6[3], lev6[4], lev6[5], lev6[6], lev6[1], lev6[2], lev6[3], lev6[4], lev6[5], lev6[6], 40.8,	31.0,	23.3, 4.9, 0,	NA,	NA,	NA,	NA,	0,	8073,	216, NA,	NA,	3.16572E+12
  )

  exp_df_out
}



test_that("exp table works", {
  exp_tbl <- make_exp_df(1)

  exp_tbl <- apply_spat_tholds(exp_tbl, cave = FALSE)

  clim_read <- data.frame(Scenario_name = "Scn1", MAT = "1;2;3;4;5;6",
                          MAT_brks = "1: -2;2: -1;3: 0;4: 1;5: 2;6: 3")

  tab <- get_exposure_table(exp_tbl, "MAT", clim_read, clim_read$MAT_brks)

  expect_s3_class(tab, "gt_tbl")
})
