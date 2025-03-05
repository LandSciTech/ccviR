# Test the prep_clim_data function


test_that("works with demo data", {

  # Copy raw files to temp
  t <- fs::dir_copy(
    fs::path_package("extdata", "clim_files", package = "ccviR"),
    fs::path_temp("clim_files"), overwrite = TRUE)

  fs::path(t, "processed") %>%
    fs::dir_ls() %>%  # Keep empty folder for newly created data
    fs::file_delete()

  # using in_folder and reproject FALSE
  in_folder <- fs::path(t, "raw")
  pth_out <- fs::path(t, "processed")

  # use first scenario to set breaks
  brks_out <- prep_clim_data(
    mat_norm = fs::path(in_folder, "NB_norm_MAT.tif"),
    mat_fut = fs::path(in_folder, "NB_RCP.4.5_MAT.tif"),
    cmd_norm = fs::path(in_folder, "NB_norm_CMD.tif"),
    cmd_fut = fs::path(in_folder, "NB_RCP.4.5_CMD.tif"),
    map = fs::path(in_folder, "NB_norm_MAP.tif"),
    mwmt = fs::path(in_folder, "NB_norm_MWMT.tif"),
    mcmt = fs::path(in_folder, "NB_norm_MCMT.tif"),
    out_folder = pth_out,
    clim_poly = fs::path_package("extdata", "assess_poly.shp", package = "ccviR"),
    overwrite = TRUE,
    scenario_name = "RCP 4.5") %>%
    suppressMessages()

  prep_clim_data(
    mat_norm = fs::path(in_folder, "NB_norm_MAT.tif"),
    mat_fut = fs::path(in_folder, "NB_RCP.8.5_MAT.tif"),
    cmd_norm = fs::path(in_folder, "NB_norm_CMD.tif"),
    cmd_fut = fs::path(in_folder, "NB_RCP.8.5_CMD.tif"),
    map = fs::path(in_folder, "NB_norm_MAP.tif"),
    mwmt = fs::path(in_folder, "NB_norm_MWMT.tif"),
    mcmt = fs::path(in_folder, "NB_norm_MCMT.tif"),
    out_folder = pth_out,
    clim_poly = fs::path_package("extdata", "assess_poly.shp", package = "ccviR"),
    overwrite = TRUE,
    scenario_name = "RCP 8.5",
    brks_mat = brks_out$brks_mat, brks_cmd = brks_out$brks_cmd,
    brks_ccei = brks_out$brks_ccei) %>%
    suppressMessages()

  expect_length(list.files(fs::path(t, "processed")), 10) # no readme here

  # check that the scn2 data is higher
  mat_45 <- terra::rast(fs::path(t, "processed/MAT_reclassRCP_4.5.tif"))
  mat_85 <- terra::rast(fs::path(t, "processed/MAT_reclassRCP_8.5.tif"))

  expect_gt(terra::global(mat_85, "mean", na.rm = TRUE)[1,1],
            terra::global(mat_45, "mean", na.rm = TRUE)[1,1])
  # # to start delete contents of processed data
  # file.remove(list.files(fs::path(pth_base, "processed"), full.names = TRUE))
  #
  # # using in_folder and reproject TRUE
  # prep_clim_data(in_folder = fs::path(pth_base, "raw"),
  #               out_folder = fs::path(pth_base, "processed"),
  #               reproject = TRUE)
  #
  # expect_length(list.files(fs::path(pth_base, "processed")), 9)

  tf <- fs::file_temp(ext = "tif")
  expect_type(
    prep_exp(
      rast_norm = terra::rast(fs::path(in_folder, "NB_norm_MAT.tif")),
      rast_fut = terra::rast(fs::path(in_folder, "NB_RCP.8.5_MAT.tif")),
      file_nm = tf, type = "sd"),
    "double")

  # Clean up
  fs::dir_delete(t)
  fs::file_delete(tf)
})

