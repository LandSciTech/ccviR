
d <- test_data(protected = FALSE)

test_that("calc_prop_raster()", {

  expect_silent(m <- calc_prop_raster(d$clim_vars$mat, d$rng_poly, "MAT"))
  expect_silent(c <- calc_prop_raster(d$clim_vars$cmd, d$rng_poly, "CMD"))
  expect_silent(h <- calc_prop_raster(d$clim_vars$htn, d$rng_clim, "HTN",
                                      val_range = 1:4))

  # TODO: Add CCEI tests
  #ccei_classes <- calc_prop_raster(d$clim_vars$ccei, d$non_breed_poly, "CCEI",
  #                                 val_range = 1:4, check_overlap = 0,
  #                                 return_overlap_as = "prop_non_breed_over_ccei")

  expect_s3_class(m, "data.frame")
  expect_length(m, 6)
  expect_length(h, 4)

  # Cannot be run interactively
  expect_snapshot_value(m, "json2")
  expect_snapshot_value(c, "json2")
  expect_snapshot_value(h, "json2")
})
