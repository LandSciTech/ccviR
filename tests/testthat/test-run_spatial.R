
expect_silent(d <- test_data())

test_that("spatial runs with all data or optional data",{
  expect_message(
    res <- analyze_spatial(
      d$rng_poly, d$assess_poly, d$clim_vars, NULL, d$ptn_poly, d$rng_chg_rast_1,
      hs_rcl = d$rng_chg_mat,
      scenario_names = d$scn_nms)
  ) |> suppressMessages()
  expect_type(res, "list")
  expect_false(anyNA(res$spat_table %>% dplyr::select(-matches("CCEI|protected"))))

  # with only required data
  expect_message(
    res2 <- analyze_spatial(d$rng_poly, d$assess_poly, d$clim_vars[c(1:2, 6)],
                            scenario_names = d$scn_nms)
  ) |> suppressMessages()
  expect_type(res2, "list")
  expect_true(anyNA(res2$spat_table))

  # Cannot be run interactively
  expect_snapshot_value(res, "json2")
  expect_snapshot_value(res2, "json2")
})

test_that("Nonoverlaping poly and raster",{
  # TODO: nonbreed not created yet for demo so shift rng_high
  nonbreed <- sf::st_as_sf(data.frame(geometry = sf::st_geometry(d$rng_poly) - 1000000)) %>%
    sf::st_set_crs(sf::st_crs(d$rng_poly))

  # use nonbreed as range - no overlap - should not be allowed
  expect_error(analyze_spatial(nonbreed, d$assess_poly, d$clim_vars[c(1:2, 6)],
                               scenario_names = d$scn_nms),
               "does not overlap") %>%
    suppressMessages()

  # use nonbreed as assess - no overlap - should not be allowed
  # error
  expect_error(analyze_spatial(d$rng_poly, nonbreed, d$clim_vars[c(1:2, 6)],
                               scenario_names = d$scn_nms),
               "does not overlap") %>%
    suppressMessages()

  # nonbreed is allowed to only partially overlap CCEI but should there be a
  # warning if below a certain threshold based on what Sarah O did 40%
  nonbreed_lt40 <- mutate(nonbreed, geometry = geometry + 800000) %>%
    sf::st_set_crs(sf::st_crs(nonbreed))

  clim_vars <- d$clim_vars
  clim_vars$ccei <- clim_vars$mat[[1]]

  expect_warning(analyze_spatial(d$rng_poly, d$assess_poly, clim_vars[c(1:2, 4, 6)],
              non_breed_poly = nonbreed_lt40,
              scenario_names = d$scn_nms),
              "does not overlap") %>%
    suppressMessages()

  # not allowed to not overlap at all
  expect_error(analyze_spatial(d$rng_poly, d$assess_poly, clim_vars[c(1:2, 4, 6)],
                               non_breed_poly = nonbreed,
                               scenario_names = d$scn_nms),
               "does not overlap") %>%
    suppressMessages()

  # ptn should not be allowed to be not overlapping assess
  expect_error(analyze_spatial(d$rng_poly, d$assess_poly, clim_vars[c(1:2, 4, 6)],
                               ptn_poly = nonbreed,
                               scenario_names = d$scn_nms),
               "does not overlap") %>%
    suppressMessages()

})

test_that("Non matching crs are handled reasonably", {
  # The crs is different and as a result they don't overlap
  expect_warning({
    rng_high_lccset <- sf::st_set_crs(d$rng_poly, value = "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45")
  })

  expect_error(analyze_spatial(rng_high_lccset, d$assess_poly, d$clim_vars[c(1:2, 6)],
                               scenario_names = d$scn_nms),
               "does not overlap") %>%
    suppressMessages()

  # the crs is different but they do overlap
  rng_high_lcctrans <- sf::st_transform(d$rng_poly, crs = "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45")
  expect_s3_class(res <- analyze_spatial(rng_high_lcctrans, d$assess_poly, d$clim_vars[c(1:2, 6)],
                                   scenario_names = d$scn_nms)$spat_table,
                 "data.frame") %>%
    suppressMessages()

  # make sure results are the same after transformed
  expect_message(
    res2 <- analyze_spatial(d$rng_poly, d$assess_poly, d$clim_vars[c(1:2, 6)],
                            scenario_names = d$scn_nms)$spat_table
  ) %>% suppressMessages()
  expect_equal(as.numeric(res[1,-c(1,27)]), as.numeric(res2[1,-c(1,27)]))
  # the range size is different after transforming but I think that is expected

  # if crs of a variable is missing give error that explains
  rng_high_ncrs <- sf::st_set_crs(d$rng_poly, NA)
  expect_error(analyze_spatial(rng_high_ncrs, d$assess_poly, d$clim_vars[c(1:2, 6)],
                               scenario_names = d$scn_nms),
               "does not have a CRS") %>%
    suppressMessages()

  # if crs of hs_rast does not match clim data and therefore polys this is ok bc
  # polys are transformed on the fly if needed but if it doesn't overlap should
  # be error
  hs2 <- terra::`crs<-`(d$rng_chg_rast_1, "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45")

  expect_error(
    expect_message(
      analyze_spatial(d$rng_poly, d$assess_poly, d$clim_vars, NULL, d$ptn_poly, hs2,
                      hs_rcl = d$rng_chg_mat,
                      scenario_names = d$scn_nms),
      "Polygons were transformed"),
    "does not fully overlap") %>%
    suppressMessages()

  # no crs in hs_rast should be error
  hs3 <- terra::`crs<-`(d$rng_chg_rast_1, NA)

  expect_error(
    analyze_spatial(d$rng_poly, d$assess_poly, d$clim_vars, NULL, d$ptn_poly, hs3,
                    hs_rcl = d$rng_chg_mat,
                    scenario_names = d$scn_nms),
    "does not have a CRS") %>%
    suppressMessages()

})

test_that("Multiple polygons are merged", {
  rng_high2 <- sf::st_buffer(d$rng_poly, -10000) %>% bind_rows(d$rng_poly)

  expect_message(
    res1 <- analyze_spatial(d$rng_poly, d$assess_poly, d$clim_vars[c(1:2, 6)],
                            scenario_names = d$scn_nms)$spat_table
  ) %>% suppressMessages()
  expect_message(
    res2 <- analyze_spatial(rng_high2, d$assess_poly, d$clim_vars[c(1:2, 6)],
                            scenario_names = d$scn_nms)$spat_table
  ) %>% suppressMessages()

  res1$range_size - res2$range_size

  expect_equal(as.numeric(res1[1,-c(1,27)]), as.numeric(res2[1,-c(1,27)]))
  # the range size is different but I think it is just an issue with my
  # polygons being made from scratch

})

test_that("gain_mod works", {
  expect_message(
    res <- analyze_spatial(d$rng_poly, d$assess_poly, d$clim_vars, hs_rast = d$rng_chg_rast_1,
                           hs_rcl = d$rng_chg_mat,
                           scenario_names = d$scn_nms)
  ) %>% suppressMessages()

  expect_message(
    res2 <- analyze_spatial(d$rng_poly, d$assess_poly, d$clim_vars, hs_rast = d$rng_chg_rast_1,
                            hs_rcl = d$rng_chg_mat,
                            gain_mod = 0.5,
                            scenario_names = d$scn_nms)
  ) %>% suppressMessages()

  expect_lt(res$spat_table$range_change[1], res2$spat_table$range_change[1])
})

test_that("works with mulitple clim scenarios",{
  # Should work with just multiple HS rasts or clim_vars too
  expect_message({
    res2 <- analyze_spatial(
      d$rng_poly, d$assess_poly,
      purrr::map(d$clim_vars, ~if(is(.x, "SpatRaster")){.x[[1]]}else{.x}),
      non_breed_poly = NULL, hs_rast = d$rng_chg_rast,
      hs_rcl = d$rng_chg_mat,
      scenario_names = c("RCP 4.5", "RCP 8.5"))
  }) %>%
    suppressMessages()

  expect_true(nrow(res2$spat_table) == 2)

  # error when length scenario names does not match #rasters
  expect_error(analyze_spatial(d$rng_poly, d$assess_poly, d$clim_vars,
                           non_breed_poly = nonbreed, hs_rast = d$rng_chg_rast_1,
                           hs_rcl = d$rng_chg_mat,
                           scenario_names = c("RCP 4.5", "RCP 8.5", "asdkjf")),
               "rasters must have one layer or")  %>%
    suppressMessages()

  expect_message(
    res3 <- analyze_spatial(d$rng_poly, d$assess_poly, d$clim_vars,
                            non_breed_poly = NULL, hs_rast = d$rng_chg_rast_1,
                            hs_rcl = d$rng_chg_mat,
                            scenario_names = c("RCP 4.5", "RCP 8.5"))
  ) %>% suppressMessages()

  expect_true(nrow(res3$spat_table) == 2)
})

test_that("gives error for points early", {
  rng_high_pts <- d$rng_poly %>% sf::st_make_grid(what = "centers")

  expect_error(analyze_spatial(rng_high_pts, d$assess_poly, d$clim_vars[c(1:2, 6)],
                               scenario_names = d$scn_nms),
               "geometry type")  %>%
    suppressMessages()

  # make sure geometry collection that includes a polygon works
  rng_high_geo <- bind_rows(d$rng_poly, st_centroid(d$rng_poly)) %>%
    st_set_agr("constant")

  expect_message(analyze_spatial(rng_high_geo, d$assess_poly, d$clim_vars[c(1:2, 6)],
                  scenario_names = d$scn_nms),
                 "Point")  %>%
    suppressMessages()

})


test_that("protected areas", {
  skip_if_not(!is.null(d$protected_poly))

  expect_message(
    res <- analyze_spatial(
      range_poly = d$rng_poly, scale_poly = d$assess_poly,
      clim_vars_lst = d$clim_vars,
      non_breed_poly = NULL,
      ptn_poly = d$ptn_poly,
      hs_rast = d$rng_chg_rast,
      hs_rcl = d$rng_chg_mat,
      protected_poly = d$protected_poly,
      scenario_names = d$scn_nms)
  ) |> suppressMessages()
  expect_type(res, "list")
  expect_false(anyNA(res$spat_table %>% dplyr::select(-contains("CCEI"))))
  expect_true("protected" %in% names(res$spat_table))

  # Cannot be run interactively
  expect_snapshot_value(res$spat_table, "json2")

})
