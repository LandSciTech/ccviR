context("test the spatial process")
library("sf", quietly = TRUE)
library("raster", quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
# load the demo data
file_dir <- system.file("extdata", package = "ccviR")
clim_vars <- get_clim_vars(file.path(file_dir, "clim_files"))

# make the crs's match to avoid warning it has to be verbatim the same
nonbreed <- st_read(file.path(file_dir, "nonbreed_poly.shp"), agr = "constant",
                    quiet = TRUE) %>% st_set_crs(4326)
assess <- st_read(file.path(file_dir, "assess_poly.shp"), agr = "constant",
                  quiet = TRUE) %>% st_set_crs(4326)
rng_high <- st_read(file.path(file_dir, "rng_poly_high.shp"), agr = "constant",
                    quiet = TRUE) %>% st_set_crs(4326)
ptn <- st_read(file.path(file_dir, "PTN_poly.shp"), agr = "constant",
                    quiet = TRUE) %>% st_set_crs(4326)
hs <- raster(file.path(file_dir, "HS_rast.tif")) %>%
  `crs<-`(value = "+proj=longlat +datum=WGS84 +no_defs")

clim_vars <- purrr::map(clim_vars,
                        ~`crs<-`(.x, value = "+proj=longlat +datum=WGS84 +no_defs"))


test_that("spatial runs with all data or optional data",{
  res <- run_spatial(rng_high, assess, clim_vars, nonbreed, ptn, hs)
  expect_false(anyNA(res))

  # with only required data
  res2 <- run_spatial(rng_high, assess, clim_vars[1:2])
  expect_true(anyNA(res2))
})

test_that("Nonoverlaping poly and raster",{
  # use nonbreed as range - no overlap - should not be allowed
  expect_error(run_spatial(nonbreed, assess, clim_vars[1:2]),
               "does not fully overlap")

  # use nonbreed as assess - no overlap - should not be allowed
  # assess area does not overlap range doesn't affect calcs but is not expected.
  # Is it worth testing for that? Does affect hs calcs
  # No error
  expect_is(run_spatial(rng_high, nonbreed, clim_vars[1:2]), "data.frame")
  # error
  expect_error(run_spatial(rng_high, nonbreed, clim_vars[1:2], hs_rast = hs),
               "does not overlap")

  # nonbreed is allowed to only partially overlap CCEI but should there be a
  # warning if below a certain threshold based on what Sarah O did 40%
  nonbreed_lt40 <- mutate(nonbreed, geometry = geometry + 0.2) %>%
    st_set_crs(st_crs(nonbreed))

  expect_warning(run_spatial(rng_high, assess, clim_vars[c(1:2, 4)],
              non_breed_poly = nonbreed_lt40),
              "does not overlap")

  # not allowed to not overlap at all
  expect_error(run_spatial(rng_high, assess, clim_vars[c(1:2, 4)],
                           non_breed_poly = rng_high),
               "does not overlap")
})

test_that("Non matching crs are handled reasonably", {
  # The crs is different and as a result they don't overlap
  rng_high_lccset <- st_set_crs(rng_high, value = "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45")
  expect_error(run_spatial(rng_high_lccset, assess, clim_vars[1:2]),
               "does not fully overlap")

  # the crs is different but they do overlap
  rng_high_lcctrans <- st_transform(rng_high, crs = "+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45")
  expect_message(res <- run_spatial(rng_high_lcctrans, assess, clim_vars[1:2]),
                 "Polygons were transformed")

  # make sure results are the same after transformed
  res2 <- run_spatial(rng_high, assess, clim_vars[1:2])
  expect_equal(as.numeric(res[1,-28]), as.numeric(res2[1,-28]))
  # the range size is different after transforming but I think that is expected

})

test_that("Multiple polygons are merged", {
  rng_high2 <- st_sfc(st_polygon(list(matrix(c(0, 0.75, 0.6, 0.75, 0.6,
                                        1, 0, 1, 0, 0.75),
                                      ncol = 2, byrow = TRUE))),
                      st_polygon(list(matrix(c(0.5, 0.75, 1, 0.75, 1,
                                               1, 0.5, 1, 0.5, 0.75),
                                             ncol = 2, byrow = TRUE)))) %>%
    st_sf() %>% st_set_crs(4326)

  res1 <- run_spatial(rng_high, assess, clim_vars[1:2])
  res2 <- run_spatial(rng_high2, assess, clim_vars[1:2])

  res1$range_size - res2$range_size

  expect_equal(as.numeric(res1[1,-28]), as.numeric(res2[1,-28]))
  # the range size is different but I think it is just an issue with my
  # polygons being made from scratch

})



