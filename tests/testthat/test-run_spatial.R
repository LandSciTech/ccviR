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
hs <- raster(file.path(file_dir, "HS_rast.tif")) %>%
  `crs<-`(value = "+proj=longlat +datum=WGS84 +no_defs")

clim_vars[1:5] <- purrr::map(clim_vars[1:5],
                              ~`crs<-`(.x, value = "+proj=longlat +datum=WGS84 +no_defs"))
clim_vars[[6]] <- st_set_crs(clim_vars[[6]], 4326)

test_that("spatial runs with all data or optional data",{
  res <- run_spatial(rng_high, assess, clim_vars, nonbreed, hs)
  expect_false(anyNA(res))

  # with only required data
  res2 <- run_spatial(rng_high, assess, clim_vars[1:2])
  expect_true(anyNA(res2))
})

test_that("Nonoverlaping poly and raster",{
  # use nonbreed as assessment area
  res <- run_spatial(nonbreed, assess, clim_vars[1:2])

  calc_vulnerability(res, vuln, "Bird")

})
