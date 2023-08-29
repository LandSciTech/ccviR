
# load the demo data
file_dir <- system.file("extdata", package = "ccviR")

# scenario names
scn_nms <- c("RCP 4.5", "RCP 8.5")

clim_vars <- get_clim_vars(file.path(file_dir, "clim_files/processed"),
                           scn_nms)

mat <- clim_vars$mat$RCP_4.5
cmd <- clim_vars$cmd$RCP_4.5

assess <- st_read(file.path(file_dir, "assess_poly.shp"), agr = "constant",
                  quiet = TRUE)
rng_high <- st_read(file.path(file_dir, "rng_poly.shp"), agr = "constant",
                    quiet = TRUE)

# based on pals::stevens.purplegold looks nice, not as easy to follow
# col_mat <- colmat(6, bottomleft = "#e8e8e8", bottomright = "#c8b35a",
#                   upperleft = "#9972af", upperright = "#804d36", do_plot = F)

# Ilona's suggested colours
col_mat <- colmat(6, bottomleft = "green", bottomright = "blue",
                  upperleft = "orange", upperright = "magenta", do_plot = F)

test_that("color matrix works", {
  expect_true(inherits(col_mat, "matrix"))
  # corner should be lightgreen
  expect_true(col_mat[1,1] == "#00FF00")


})


test_that("creating raster works",{
  bivar_map <- bivar_map(cmd, mat)

  expect_s4_class(bivar_map, "SpatRaster")
})


test_that("map looks right",{
  spat_res_map <- plot_bivar_exp(mat, cmd, assess, rng_high)

  expect_s3_class(spat_res_map$plot, "ggplot")
})

test_that("works for multi", {
  expect_s3_class(plot_bivar_exp(clim_vars$mat, clim_vars$cmd,
                                 assess, rng_high)$plot,
                  "ggplot")
})


# test with full size data
# # load the data
# file_dir <- "../CCVI_analysis/data"
#
# # scenario names
# scn_nms <- c("RCP 4.5", "RCP 8.5")
#
# clim_vars <- get_clim_vars(file.path(file_dir, "CMIP5_ENSEMBLE_rcp45_rcp85_2050_NORM_6190"),
#                            scn_nms)
#
# mat <- clim_vars$mat$RCP_4.5
# cmd <- clim_vars$cmd$RCP_4.5
#
# assess <- sf::st_read(file.path(file_dir, "scale_files/CAN.shp"), agr = "constant",
#                   quiet = TRUE)
# rng_high <- sf::st_read(file.path(file_dir, "species_files/AWPE/CAN/AWPE_CAN.shp"),
#                     agr = "constant",
#                     quiet = TRUE)
#
# spat_res_map <- plot_bivar_exp(clim_vars$mat, clim_vars$cmd, assess, rng_high)
