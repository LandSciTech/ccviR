
library("raster", quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
# load the demo data
file_dir <- system.file("extdata", package = "ccviR")

# scenario names
scn_nms <- c("RCP 4.5", "RCP 8.5")

clim_vars <- get_clim_vars(file.path(file_dir, "clim_files/processed"),
                           scn_nms)

mat <- clim_vars$mat$RCP_4.5
cmd <- clim_vars$cmd$RCP_4.5

# based on pals::stevens.purplegold looks nice, not as easy to follow
# col_mat <- colmat(6, bottomleft = "#e8e8e8", bottomright = "#c8b35a",
#                   upperleft = "#9972af", upperright = "#804d36", do_plot = F)

# Ilona's suggested colours
col_mat <- colmat(6, bottomleft = "green", bottomright = "blue",
                  upperleft = "orange", upperright = "magenta", do_plot = T)

test_that("color matrix works", {
  expect_true(inherits(col_mat, "matrix"))
  # corner should be lightgreen
  expect_true(col_mat[1,1] == "#00FF00")


})


test_that("creating raster works",{
  bivar_map <- bivar_map(cmd, mat)

  expect_s4_class(bivar_map, "Raster")
})


test_that("map looks right",{
  spat_res_map <- plot_bivar_exp(mat, cmd)

  expect_s3_class(spat_res_map, "ggplot")
})
