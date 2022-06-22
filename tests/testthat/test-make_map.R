
library("sf", quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
library("raster", quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
# load the demo data
file_dir <- system.file("extdata", package = "ccviR")
clim_vars <- get_clim_vars(file.path(file_dir, "clim_files/processed"))

assess <- st_read(file.path(file_dir, "assess_poly.shp"), agr = "constant",
                  quiet = TRUE)
rng_high <- st_read(file.path(file_dir, "rng_poly_high.shp"), agr = "constant",
                    quiet = TRUE)

hs <- raster(file.path(file_dir, "HS_rast_high.tif"))

# hs2 less CC in same area
hs_stack <- stack(file.path(file_dir, "HS_rast_RCP4.5.tif"),
                  file.path(file_dir, "HS_rast_high.tif"))


map2 <- make_map(rng_high, clim_vars$mat, rast_nm = "mat", poly2 = assess,
                 poly2_nm = "assess_poly", rast_grp = "group1")

map1 <- make_map(rng_high, hs_stack, rast_nm = "mat", poly2 = assess,
                 poly2_nm = "assess_poly",
                 rast_grp = c("Temperature RCP4.5", "Temperature RCP8.5"))


# Issue sent to tmap about these warnings https://github.com/r-tmap/tmap/issues/630
# group does not work at the moment
test_that("map gets made",
          expect_s3_class(map1, "tmap"))

# Visually inspect the maps
if(interactive()){
  cur_mode <- tmap::tmap_mode("view")

  map2
  map1

  tmap::tmap_mode(cur_mode)
}