test_that("exp table works", {
  exp_tbl <- test_exp_tbl(1)

  exp_tbl <- apply_spat_tholds(exp_tbl, cave = FALSE)

  clim_read <- data.frame(Scenario_name = "Scn1", MAT = "1;2;3;4;5;6",
                          MAT_brks = "1: -2;2: -1;3: 0;4: 1;5: 2;6: 3")

  tab <- get_exposure_table(exp_tbl, "MAT", clim_read, clim_read$MAT_brks)

  expect_s3_class(tab, "gt_tbl")
})
