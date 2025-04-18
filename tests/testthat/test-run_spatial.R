# test the spatial process
# load the demo data
file_dir <- system.file("extdata", package = "ccviR")

# scenario names
scn_nms <- c("RCP 4.5", "RCP 8.5")

clim_vars <- get_clim_vars(file.path(file_dir, "clim_files/processed"),
                           scn_nms)

rng_chg_mat <- matrix(c(-1:1,NA, 1:3,0), ncol = 2)

# make the crs's match to avoid warning it has to be verbatim the same
# nonbreed <- st_read(file.path(file_dir, "nonbreed_poly.shp"), agr = "constant",
#                     quiet = TRUE)
assess <- sf::st_read(file.path(file_dir, "assess_poly.shp"), agr = "constant",
                  quiet = TRUE)
rng_high <- sf::st_read(file.path(file_dir, "rng_poly.shp"), agr = "constant",
                    quiet = TRUE)
ptn <- sf::st_read(file.path(file_dir, "PTN_poly.shp"), agr = "constant",
                    quiet = TRUE)
hs <- raster::raster(file.path(file_dir, "rng_chg_45.tif"))

hs_terra <- terra::rast(file.path(file_dir, "rng_chg_45.tif"))

# hs2 less CC in same area
hs2 <- raster::raster(file.path(file_dir, "rng_chg_85.tif"))
hs1 <- raster::raster(file.path(file_dir, "rng_chg_45.tif"))

rng_high_pts <- rng_high %>% sf::st_make_grid(what = "centers")


test_that("spatial runs with all data or optional data",{
  res <- analyze_spatial(rng_high, assess, clim_vars, NULL, ptn, hs_terra,
                     hs_rcl = rng_chg_mat,
                     scenario_names = scn_nms)
  expect_false(anyNA(res$spat_table %>% dplyr::select(-contains("CCEI"))))

  # with only required data
  res2 <- analyze_spatial(rng_high, assess, clim_vars[c(1:2, 6)],
                          scenario_names = scn_nms)
  expect_true(anyNA(res2$spat_table))
})

test_that("Nonoverlaping poly and raster",{
  # nonbreed not created yet for demo so shift rng_high
  nonbreed <- sf::st_as_sf(data.frame(geometry = sf::st_geometry(rng_high) - 1000000)) %>%
    sf::st_set_crs(sf::st_crs(rng_high))

  # use nonbreed as range - no overlap - should not be allowed
  expect_error(analyze_spatial(nonbreed, assess, clim_vars[c(1:2, 6)],
                               scenario_names = scn_nms),
               "does not overlap")

  # use nonbreed as assess - no overlap - should not be allowed
  # error
  expect_error(analyze_spatial(rng_high, nonbreed, clim_vars[c(1:2, 6)],
                               scenario_names = scn_nms),
               "does not overlap")

  # nonbreed is allowed to only partially overlap CCEI but should there be a
  # warning if below a certain threshold based on what Sarah O did 40%
  nonbreed_lt40 <- mutate(nonbreed, geometry = geometry + 800000) %>%
    sf::st_set_crs(sf::st_crs(nonbreed))

  clim_vars$ccei <- clim_vars$mat[[1]]

  expect_warning(analyze_spatial(rng_high, assess, clim_vars[c(1:2, 4, 6)],
              non_breed_poly = nonbreed_lt40,
              scenario_names = scn_nms),
              "does not overlap")

  # not allowed to not overlap at all
  expect_error(analyze_spatial(rng_high, assess, clim_vars[c(1:2, 4, 6)],
                               non_breed_poly = nonbreed,
                               scenario_names = scn_nms),
               "does not overlap")

  # ptn should not be allowed to be not overlapping assess
  expect_error(analyze_spatial(rng_high, assess, clim_vars[c(1:2, 4, 6)],
                               ptn_poly = nonbreed,
                               scenario_names = scn_nms),
               "does not overlap")

})

test_that("Non matching crs are handled reasonably", {
  # The crs is different and as a result they don't overlap
  expect_warning({
    rng_high_lccset <- sf::st_set_crs(rng_high, value = "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45")
  })

  expect_error(analyze_spatial(rng_high_lccset, assess, clim_vars[c(1:2, 6)],
                               scenario_names = scn_nms),
               "does not overlap")

  # the crs is different but they do overlap
  rng_high_lcctrans <- sf::st_transform(rng_high, crs = "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45")
  expect_s3_class(res <- analyze_spatial(rng_high_lcctrans, assess, clim_vars[c(1:2, 6)],
                                   scenario_names = scn_nms)$spat_table,
                 "data.frame")

  # make sure results are the same after transformed
  res2 <- analyze_spatial(rng_high, assess, clim_vars[c(1:2, 6)],
                          scenario_names = scn_nms)$spat_table
  expect_equal(as.numeric(res[1,-c(1,27)]), as.numeric(res2[1,-c(1,27)]))
  # the range size is different after transforming but I think that is expected

  # if crs of a variable is missing give error that explains
  rng_high_ncrs <- sf::st_set_crs(rng_high, NA)
  expect_error(analyze_spatial(rng_high_ncrs, assess, clim_vars[c(1:2, 6)],
                               scenario_names = scn_nms),
               "does not have a CRS")

  # if crs of hs_rast does not match clim data and therefore polys this is ok bc
  # polys are transformed on the fly if needed but if it doesn't overlap should
  # be error
  hs_terra2 <- terra::`crs<-`(hs_terra, "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45")

  expect_error(
    expect_message(
      analyze_spatial(rng_high, assess, clim_vars, NULL, ptn, hs_terra2,
                      hs_rcl = rng_chg_mat,
                      scenario_names = scn_nms),
      "Polygons were transformed"),
    "does not fully overlap")

  # no crs in hs_rast should be error
  hs_terra3 <- terra::`crs<-`(hs_terra, NA)

  expect_error(
    analyze_spatial(rng_high, assess, clim_vars, NULL, ptn, hs_terra3,
                    hs_rcl = rng_chg_mat,
                    scenario_names = scn_nms),
    "does not have a CRS")


})

test_that("Multiple polygons are merged", {
  rng_high2 <- sf::st_buffer(rng_high, -10000) %>% bind_rows(rng_high)

  res1 <- analyze_spatial(rng_high, assess, clim_vars[c(1:2, 6)],
                          scenario_names = scn_nms)$spat_table
  res2 <- analyze_spatial(rng_high2, assess, clim_vars[c(1:2, 6)],
                          scenario_names = scn_nms)$spat_table

  res1$range_size - res2$range_size

  expect_equal(as.numeric(res1[1,-c(1,27)]), as.numeric(res2[1,-c(1,27)]))
  # the range size is different but I think it is just an issue with my
  # polygons being made from scratch

})

test_that("gain_mod works", {
  res <- analyze_spatial(rng_high, assess, clim_vars,hs_rast =  hs,
                     hs_rcl = rng_chg_mat,
                     scenario_names = scn_nms)

  res2 <- analyze_spatial(rng_high, assess, clim_vars, hs_rast = hs,
                      hs_rcl = rng_chg_mat,
                      gain_mod = 0.5,
                      scenario_names = scn_nms)

  expect_lt(res$spat_table$range_change[1], res2$spat_table$range_change[1])
})

test_that("works with mulitple clim scenarios",{
  # Should work with just multiple HS rasts or clim_vars too
  res2 <- analyze_spatial(rng_high, assess,
                          purrr::map(clim_vars, ~if(is(.x, "SpatRaster")){.x[[1]]}else{.x}),
                          non_breed_poly = NULL, hs_rast = raster::stack(hs1, hs2),
                          hs_rcl = rng_chg_mat,
                          scenario_names = c("RCP 4.5", "RCP 8.5"))

  expect_true(nrow(res2$spat_table) == 2)

  # error when length scenario names does not match #rasters
  expect_error(analyze_spatial(rng_high, assess, clim_vars,
                           non_breed_poly = nonbreed, hs_rast = hs,
                           hs_rcl = rng_chg_mat,
                           scenario_names = c("RCP 4.5", "RCP 8.5", "asdkjf")),
               "rasters must have one layer or")

  res3 <- analyze_spatial(rng_high, assess, clim_vars,
                     non_breed_poly = NULL, hs_rast = hs,
                     hs_rcl = rng_chg_mat,
                     scenario_names = c("RCP 4.5", "RCP 8.5"))

  expect_true(nrow(res3$spat_table) == 2)
})

test_that("gives error for points early", {
  rng_high_pts <- rng_high %>% sf::st_make_grid(what = "centers")

  expect_error(analyze_spatial(rng_high_pts, assess, clim_vars[c(1:2, 6)],
                               scenario_names = scn_nms),
               "geometry type")

  # make sure geometry collection that includes a polygon works
  rng_high_geo <- bind_rows(rng_high, st_centroid(rng_high)) %>%
    st_set_agr("constant")

  expect_message(analyze_spatial(rng_high_geo, assess, clim_vars[c(1:2, 6)],
                  scenario_names = scn_nms),
                 "Point")

})
