
make_exp_df <- function(exp_lev){
  if(exp_lev == 1){
    lev6 <- c(5, 10, 10, 25, 25, 25)
  }
  if(exp_lev == 2){
    lev6 <- c(10, 12, 30, 25, 10, 5)
  }
  if(exp_lev == 3){
    lev6 <- rev(c(5, 10, 10, 25, 25, 25))
  }

  exp_df_out <- tribble(
    ~scenario_name, ~MAT_1, ~MAT_2, ~MAT_3, ~MAT_4, ~MAT_5, ~MAT_6, ~CMD_1, ~CMD_2, ~CMD_3, ~CMD_4, ~CMD_5, ~CMD_6, ~CCEI_1, ~CCEI_2, ~CCEI_3, ~CCEI_4, ~perc_non_breed_not_over_ccei, ~HTN_1, ~HTN_2, ~HTN_3, ~HTN_4, ~PTN, ~MAP_max, ~MAP_min, ~range_change, ~range_overlap, ~range_size,
    "Scn1", lev6[1], lev6[2], lev6[3], lev6[4], lev6[5], lev6[6], lev6[1], lev6[2], lev6[3], lev6[4], lev6[5], lev6[6], 40.8,	31.0,	23.3, 4.9, 0,	NA,	NA,	NA,	NA,	0,	8073,	216, NA,	NA,	3.16572E+12
  )

  exp_df_out
}

test_that("plot_conf_score works for single", {
  expect_s3_class(plot_conf_score(calc_vulnerability(make_exp_df(1), make_vuln_df("nm", 0),
                                                      tax_grp = "Bird")),
                  "ggplot")
})

test_that("plot_conf_score multi plot works", {
  exp_df2 <- bind_rows(make_exp_df(1), make_exp_df(2))
  exp_df2[2,1] <- "Scn2"
  exp_df2$range_change <- c(NA, 50)
  exp_df2$range_overlap <- c(NA, 20)

  expect_s3_class(plot_conf_score(calc_vulnerability(exp_df2,
                                                      make_vuln_df("nm", 1),
                                                      tax_grp = "Bird")),
                  "ggplot")

  expect_s3_class(plot_conf_score(calc_vulnerability(exp_df2,
                                                      make_vuln_df("nm", rep(c(0,1), length.out = 27),
                                                                   rep(c(3,2), length.out = 27)),
                                                      tax_grp = "Bird")),
                  "ggplot")

})
