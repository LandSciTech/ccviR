context("test the spatial process")
library("sf", quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
library("raster", quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
# load the demo data
file_dir <- system.file("extdata", package = "ccviR")
clim_vars <- get_clim_vars(file.path(file_dir, "clim_files/processed"))

# make the crs's match to avoid warning it has to be verbatim the same
nonbreed <- st_read(file.path(file_dir, "nonbreed_poly.shp"), agr = "constant",
                    quiet = TRUE)
assess <- st_read(file.path(file_dir, "assess_poly.shp"), agr = "constant",
                  quiet = TRUE)
rng_high <- st_read(file.path(file_dir, "rng_poly_high.shp"), agr = "constant",
                    quiet = TRUE)
ptn <- st_read(file.path(file_dir, "PTN_poly.shp"), agr = "constant",
                    quiet = TRUE)
hs <- raster(file.path(file_dir, "HS_rast_high.tif"))

# hs2 less CC in same area
hs2 <- raster(file.path(file_dir, "HS_rast_RCP4.5.tif"))
hs1 <- raster(file.path(file_dir, "HS_rast_RCP2.6.tif"))

test_that("spatial runs with all data or optional data",{
  res <- run_spatial(rng_high, assess, clim_vars, nonbreed, ptn, hs,
                     hs_rcl = matrix(c(0:7, c(0,1,2,2,2,2,2,3)), ncol = 2))
  expect_false(anyNA(res$spat_table))

  # with only required data
  res2 <- run_spatial(rng_high, assess, clim_vars[c(1:2, 6)])
  expect_true(anyNA(res2$spat_table))
})

test_that("Nonoverlaping poly and raster",{
  # use nonbreed as range - no overlap - should not be allowed
  expect_error(run_spatial(nonbreed, assess, clim_vars[c(1:2, 6)]),
               "does not overlap")

  # use nonbreed as assess - no overlap - should not be allowed
  # error
  expect_error(run_spatial(rng_high, nonbreed, clim_vars[c(1:2, 6)]),
               "does not overlap")

  # nonbreed is allowed to only partially overlap CCEI but should there be a
  # warning if below a certain threshold based on what Sarah O did 40%
  nonbreed_lt40 <- mutate(nonbreed, geometry = geometry + 200) %>%
    st_set_crs(st_crs(nonbreed))

  expect_warning(run_spatial(rng_high, assess, clim_vars[c(1:2, 4, 6)],
              non_breed_poly = nonbreed_lt40),
              "does not overlap")

  # not allowed to not overlap at all
  expect_error(run_spatial(rng_high, assess, clim_vars[c(1:2, 4, 6)],
                           non_breed_poly = rng_high),
               "does not overlap")
})

test_that("Non matching crs are handled reasonably", {
  # The crs is different and as a result they don't overlap
  expect_warning({
    rng_high_lccset <- st_set_crs(rng_high, value = "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45")
  })

  expect_error(run_spatial(rng_high_lccset, assess, clim_vars[c(1:2, 6)]),
               "does not overlap")

  # the crs is different but they do overlap
  rng_high_lcctrans <- st_transform(rng_high, crs = "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45")
  expect_is(res <- run_spatial(rng_high_lcctrans, assess, clim_vars[c(1:2, 6)])$spat_table,
                 "data.frame")

  # make sure results are the same after transformed
  res2 <- run_spatial(rng_high, assess, clim_vars[c(1:2, 6)])$spat_table
  expect_equal(as.numeric(res[1,-c(1,27)]), as.numeric(res2[1,-c(1,27)]))
  # the range size is different after transforming but I think that is expected

})

test_that("Multiple polygons are merged", {
  rng_high2 <- st_sfc(st_polygon(list(matrix(c(0, 0.75, 0.6, 0.75, 0.6,
                                        1, 0, 1, 0, 0.75)*1000,
                                      ncol = 2, byrow = TRUE))),
                      st_polygon(list(matrix(c(0.5, 0.75, 1, 0.75, 1,
                                               1, 0.5, 1, 0.5, 0.75)*1000,
                                             ncol = 2, byrow = TRUE)))) %>%
    st_sf() %>% st_set_crs(3162)

  res1 <- run_spatial(rng_high, assess, clim_vars[c(1:2, 6)])$spat_table
  res2 <- run_spatial(rng_high2, assess, clim_vars[c(1:2, 6)])$spat_table

  res1$range_size - res2$range_size

  expect_equal(as.numeric(res1[1,-c(1,27)]), as.numeric(res2[1,-c(1,27)]))
  # the range size is different but I think it is just an issue with my
  # polygons being made from scratch

})

test_that("gain_mod works", {
  res <- run_spatial(rng_high, assess, clim_vars,hs_rast =  hs,
                     hs_rcl = matrix(c(0:7, c(0,1,2,2,2,2,2,3)), ncol = 2))

  res2 <- run_spatial(rng_high, assess, clim_vars, hs_rast = hs,
                      hs_rcl = matrix(c(0:7, c(0,1,2,2,2,2,2,3)), ncol = 2),
                      gain_mod = 0.5)

  expect_lt(res$spat_table$range_change, res2$spat_table$range_change)
})

test_that("works with mulitple clim scenarios",{
  clim_vars_multi <- get_clim_vars(file.path(file_dir,
                                             "clim_files/processed/multi_scenario"),
                                   scenario_names = c("RCP2.6", "RCP4.5", "RCP8.5"))

  res <- run_spatial(rng_high, assess, clim_vars_multi,
                     non_breed_poly = nonbreed, hs_rast = stack(hs1, hs2, hs),
                     hs_rcl = matrix(c(0:7, c(0,1,2,2,2,2,2,3)), ncol = 2),
                     scenario_names = c("RCP2.6", "RCP4.5", "RCP8.5"))

  expect_true(nrow(res$spat_table) == 3)

  # Should work with just multiple HS rasts or clim_vars too
  res2 <- run_spatial(rng_high, assess, clim_vars,
                     non_breed_poly = nonbreed, hs_rast = stack(hs1, hs2, hs),
                     hs_rcl = matrix(c(0:7, c(0,1,2,2,2,2,2,3)), ncol = 2),
                     scenario_names = c("RCP2.6", "RCP4.5", "RCP8.5"))

  expect_true(nrow(res2$spat_table) == 3)

  # error when length scenario names does not match #rasters
  expect_error(run_spatial(rng_high, assess, clim_vars_multi,
                           non_breed_poly = nonbreed, hs_rast = hs,
                           hs_rcl = matrix(c(0:7, c(0,1,2,2,2,2,2,3)), ncol = 2),
                           scenario_names = c("RCP4.5", "RCP8.5")),
               "rasters must have one layer or")

  res3 <- run_spatial(rng_high, assess, clim_vars_multi,
                     non_breed_poly = nonbreed, hs_rast = hs,
                     hs_rcl = matrix(c(0:7, c(0,1,2,2,2,2,2,3)), ncol = 2),
                     scenario_names = c("RCP2.6", "RCP4.5", "RCP8.5"))

  expect_true(nrow(res3$spat_table) == 3)
})

