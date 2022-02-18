context("Test the run_prep_data function")
pth_base <- system.file("extdata/clim_files", package = "ccviR")

test_that("works with demo data",{
  # to start delete contents of processed data
  file.remove(list.files(file.path(pth_base, "processed"), "\\.[^c]", full.names = TRUE))

  # using in_folder and reproject FALSE
  run_prep_data(in_folder = file.path(pth_base, "raw"),
                out_folder = file.path(pth_base, "processed"),
                reproject = FALSE)

  expect_length(list.files(file.path(pth_base, "processed")), 10)

  # # to start delete contents of processed data
  # file.remove(list.files(file.path(pth_base, "processed"), full.names = TRUE))
  #
  # # using in_folder and reproject TRUE
  # run_prep_data(in_folder = file.path(pth_base, "raw"),
  #               out_folder = file.path(pth_base, "processed"),
  #               reproject = TRUE)
  #
  # expect_length(list.files(file.path(pth_base, "processed")), 9)

  # using filenames, ovrewrite and clim_poly file
  run_prep_data(mat_norm = file.path(pth_base, "raw/MAT.tif"),
                mat_fut = file.path(pth_base, "raw/MAT_2050.tif"),
                cmd_norm = file.path(pth_base, "raw/CMD.tif"),
                cmd_fut = file.path(pth_base, "raw/CMD_2050.tif"),
                map = file.path(pth_base, "raw/MAP.tif"),
                mcmt = file.path(pth_base, "raw/MCMT.tif"),
                mwmt = file.path(pth_base, "raw/MWMT.tif"),
                ccei = file.path(pth_base, "raw/CCEI.img"),
                clim_poly = file.path(system.file("extdata", package = "ccviR"),
                                      "assess_poly.shp"),
                out_folder = file.path(pth_base, "processed"),
                overwrite = TRUE,
                reproject = FALSE)

  expect_length(list.files(file.path(pth_base, "processed")), 10)

})

test_that("error for missing files, works without optional files",{

  # required missing:
  # copy MAT
  dir.create(file.path(pth_base, "temp"))
  file.copy(file.path(pth_base, "raw/MAT.tif"),
            file.path(pth_base, "temp/MAT.tif"))
  # remove MAT
  file.remove(file.path(pth_base, "raw/MAT.tif"))

  expect_error({
    run_prep_data(in_folder = file.path(pth_base, "raw"),
                out_folder = file.path(pth_base, "processed"),
                overwrite = TRUE)
    },
    "None of the files in")
  file.copy(file.path(pth_base, "temp/MAT.tif"),
            file.path(pth_base, "raw/MAT.tif"))

  # optional missing:
  file.copy(file.path(pth_base, "raw/CCEI.img"),
            file.path(pth_base, "temp/CCEI.img"))
  # remove MAT
  file.remove(file.path(pth_base, "raw/CCEI.img"))


  expect_type({
    run_prep_data(in_folder = file.path(pth_base, "raw"),
                  out_folder = file.path(pth_base, "processed"),
                  overwrite = TRUE,
                  reproject = FALSE)
  }, "list")

  file.copy(file.path(pth_base, "temp/CCEI.img"),
            file.path(pth_base, "raw/CCEI.img"))

  # two files matching pattern
  file.copy(file.path(pth_base, "raw/MAT_2050.tif"),
            file.path(pth_base, "raw/MAT_2050_2.tif"))

  expect_error({
    run_prep_data(in_folder = file.path(pth_base, "raw"),
                out_folder = file.path(pth_base, "processed"),
                overwrite = TRUE,
                reproject = FALSE)
    },
  "more than one file")

  file.remove(file.path(pth_base, "raw/MAT_2050_2.tif"))

})

test_that("multiple scenarios works",{
  brks_out <- run_prep_data(in_folder = file.path(pth_base, "raw"),
                            out_folder = file.path(pth_base, "processed/multi_scenario"),
                            overwrite = TRUE, scenario_name = "RCP4.5")

  run_prep_data(mat_norm = file.path(pth_base, "raw/MAT.tif"),
                mat_fut = file.path(pth_base, "raw/scenario2/MAT_2050_scn2.tif"),
                cmd_norm = file.path(pth_base, "raw/CMD.tif"),
                cmd_fut = file.path(pth_base, "raw/scenario2/CMD_2050_scn2.tif"),
                ccei = file.path(pth_base, "raw/scenario2/CCEI_scn2.tif"),
                out_folder = file.path(pth_base, "processed/multi_scenario"),
                overwrite = TRUE, scenario_name = "RCP8.5",
                brks_mat = brks_out$brks_mat, brks_cmd = brks_out$brks_cmd,
                brks_ccei = brks_out$brks_ccei)

  run_prep_data(mat_norm = file.path(pth_base, "raw/MAT.tif"),
                mat_fut = file.path(pth_base, "raw/scenario2/MAT_2050_scn3.tif"),
                cmd_norm = file.path(pth_base, "raw/CMD.tif"),
                cmd_fut = file.path(pth_base, "raw/scenario2/CMD_2050_scn3.tif"),
                ccei = file.path(pth_base, "raw/scenario2/CCEI_scn3.tif"),
                out_folder = file.path(pth_base, "processed/multi_scenario"),
                overwrite = TRUE, scenario_name = "RCP2.6",
                brks_mat = brks_out$brks_mat, brks_cmd = brks_out$brks_cmd,
                brks_ccei = brks_out$brks_ccei)

  # check that the scn2 data is higher
  mat_26 <- raster::raster(file.path(pth_base,
                                     "processed/multi_scenario/MAT_reclassRCP2.6.tif"))

  mat_45 <- raster::raster(file.path(pth_base,
                                     "processed/multi_scenario/MAT_reclassRCP4.5.tif"))

  mat_85 <- raster::raster(file.path(pth_base,
                                     "processed/multi_scenario/MAT_reclassRCP8.5.tif"))

  expect_gt(raster::freq(mat_85)[1,2], raster::freq(mat_26)[1,2])
  expect_gt(raster::freq(mat_85)[1,2], raster::freq(mat_45)[1,2])
})

# remove the temp directory
unlink(file.path(pth_base, "temp"), recursive = TRUE)
