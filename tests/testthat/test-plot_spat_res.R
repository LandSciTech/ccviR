
library("raster", quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
# load the demo data
file_dir <- system.file("extdata", package = "ccviR")

# scenario names
scn_nms <- c("RCP 4.5", "RCP 8.5")

clim_vars <- get_clim_vars(file.path(file_dir, "clim_files/processed"),
                           scn_nms)

mat <- clim_vars$mat$RCP.4.5
cmd <- clim_vars$cmd$RCP.8.5

# based on pals::stevens.purplegold looks nice, not as easy to follow
col_mat <- colmat(6, bottomleft = "#e8e8e8", bottomright = "#c8b35a",
                  upperleft = "#9972af", upperright = "#804d36", do_plot = F)

# wider range of colours, less attractive
col_mat <- colmat(6, bottomleft = "green", bottomright = "blue",
                  upperleft = "orange", upperright = "magenta", do_plot = F)

test_that("color matrix works", {
  expect_true(inherits(col_mat, "matrix"))
  # corner should be lightgreen
  expect_true(col_mat[1,1] == "#00FF00")


})


test_that("creating raster works",{
  bivar_map <- bivar_map(cmd, mat)

  expect_s4_class(bivar_map, "Raster")

  # plot(bivar_map, col = as.vector(col_mat), legend = FALSE, axes = FALSE, box = FALSE)
  #
  # raster(matrix(1:36, nrow = 6)) %>% raster::flip() %>%
  #   raster::plot(col = as.vector(col_mat),
  #                xlab = expression(Moisture ~ Exposure~symbol('\256')),
  #                ylab =  expression(Temperature ~ Exposure~symbol('\256')), box =F,
  #                add =F, legend = F, axes = FALSE, mgp = c(0,0,0))
})
