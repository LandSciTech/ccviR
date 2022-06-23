context("Test the prep_clim_data function")
pth_base <- system.file("extdata/clim_files", package = "ccviR")

test_that("works with demo data",{
  # to start delete contents of processed data
  file.remove(list.files(file.path(pth_base, "processed"), "\\.[^c]", full.names = TRUE))

  # using in_folder and reproject FALSE
  in_folder <- file.path(pth_base, "raw")

  pth_out <- file.path(pth_base, "processed")

  # use first scenario to set breaks
  brks_out <- prep_clim_data(mat_norm = file.path(in_folder, "NB_norm_MAT.tif"),
                             mat_fut = file.path(in_folder, "NB_RCP.4.5_MAT.tif"),
                             cmd_norm = file.path(in_folder, "NB_norm_CMD.tif"),
                             cmd_fut = file.path(in_folder, "NB_RCP.4.5_CMD.tif"),
                             map = file.path(in_folder, "NB_norm_MAP.tif"),
                             mwmt = file.path(in_folder, "NB_norm_MWMT.tif"),
                             mcmt = file.path(in_folder, "NB_norm_MCMT.tif"),
                             out_folder = pth_out,
                             clim_poly = file.path(system.file("extdata", package = "ccviR"),
                                                   "assess_poly.shp"),
                             overwrite = TRUE,
                             scenario_name = "RCP 4.5")

  prep_clim_data(mat_norm = file.path(in_folder, "NB_norm_MAT.tif"),
                 mat_fut = file.path(in_folder, "NB_RCP.8.5_MAT.tif"),
                 cmd_norm = file.path(in_folder, "NB_norm_CMD.tif"),
                 cmd_fut = file.path(in_folder, "NB_RCP.8.5_CMD.tif"),
                 map = file.path(in_folder, "NB_norm_MAP.tif"),
                 mwmt = file.path(in_folder, "NB_norm_MWMT.tif"),
                 mcmt = file.path(in_folder, "NB_norm_MCMT.tif"),
                 out_folder = pth_out,
                 clim_poly = file.path(system.file("extdata", package = "ccviR"),
                                       "assess_poly.shp"),
                 overwrite = TRUE,
                 scenario_name = "RCP 8.5",
                 brks_mat = brks_out$brks_mat, brks_cmd = brks_out$brks_cmd,
                 brks_ccei = brks_out$brks_ccei)

  expect_length(list.files(file.path(pth_base, "processed")), 11)

  # check that the scn2 data is higher
  mat_45 <- raster::raster(file.path(pth_base,
                                     "processed/MAT_reclassRCP 4.5.tif"))

  mat_85 <- raster::raster(file.path(pth_base,
                                     "processed/MAT_reclassRCP 8.5.tif"))

  expect_gt(raster::cellStats(mat_85, mean), raster::cellStats(mat_45, mean))
  # # to start delete contents of processed data
  # file.remove(list.files(file.path(pth_base, "processed"), full.names = TRUE))
  #
  # # using in_folder and reproject TRUE
  # prep_clim_data(in_folder = file.path(pth_base, "raw"),
  #               out_folder = file.path(pth_base, "processed"),
  #               reproject = TRUE)
  #
  # expect_length(list.files(file.path(pth_base, "processed")), 9)

})

