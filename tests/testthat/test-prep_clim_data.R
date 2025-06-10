# Test the prep_clim_xxx functions

expect_silent(f <- test_files_prep())


test_that("prep_from_delta()", {

  skip_if_not(fs::file_exists(f["mat_norm_pth"]))

  out <- fs::path(tempdir(), "MAT_reclass_test.tif")
  r1 <- terra::rast(f["mat_norm_pth"]) # Would normally use diff between rasters

  expect_silent({
    p <- prep_from_delta(rast_delta = r1,
                         sd_div = 2, shift = 0, type = "IQR",
                         file_nm = out, overwrite = TRUE, brs = NULL)
  })

  expect_true(is.matrix(p))
  expect_true(fs::file_exists(out))
  r2 <- terra::rast(out)

  v1 <- terra::values(r1)
  v2 <- terra::values(r2)

  expect_false(all(v1 == v2))
  expect_false(is.integer(min(v1)))
  expect_true(is.integer(min(v2)))

  expect_snapshot_value(p, style = "json2")

  unlink(out)

})

test_that("prep_exp()", {
  skip_if_not(fs::file_exists(f["mat_norm_pth"]) &
                fs::file_exists(f["mat_fut_pth1"]))

  out <- fs::path(tempdir(), "MAT_reclass_test.tif")

  expect_silent({
    p1 <- prep_exp(rast_norm = terra::rast(f["mat_norm_pth"]),
                  rast_fut = terra::rast(f["mat_fut_pth1"]),
                  file_nm = out, reproject = FALSE, overwrite = TRUE,
                  type = "halfIQR", brs = NULL)
  })

  expect_silent({
    p2 <- prep_exp(rast_norm = terra::rast(f["mat_norm_pth"]),
                  rast_fut = terra::rast(f["mat_fut_pth2"]),
                  file_nm = out, reproject = FALSE, overwrite = TRUE,
                  type = "sd", brs = NULL)
  })

  expect_snapshot_value(p1, style = "json2")
  expect_snapshot_value(p2, style = "json2")
  unlink(out)
})

test_that("ccei_reclassify()", {

  skip_if_not(fs::file_exists(f["ccei_pth1"]))

  out <- fs::path(tempdir(), "processed")
  fs::dir_create(out)

  expect_silent({
    p <- ccei_reclassify(
      ccei = terra::rast(f["ccei_pth1"]), brks = NULL,
      out_folder = out, scenario_name = "RCP_4.5", overwrite = TRUE)
  })

  expect_true(is.matrix(p))
  expect_snapshot_value(p, style = "json2")

  unlink(out, recursive = TRUE)
})

test_that("prep_clim_data() - no ccei", {

  out <- fs::path(tempdir(), "processed")
  fs::dir_create(out)

  expect_silent({
    clim1 <- prep_clim_data(
      mat_norm = f["mat_norm_pth"],
      mat_fut = f["mat_fut_pth1"],
      cmd_norm = f["cmd_norm_pth"],
      cmd_fut = f["cmd_fut_pth1"],
      map = f["map_norm_pth"],
      mwmt = f["mwmt_norm_pth"],
      mcmt = f["mcmt_norm_pth"],
      ccei = NULL,
      out_folder = out,
      clim_poly = test_files()$assess_poly_pth,
      overwrite = TRUE,
      scenario_name = "RCP 4.5",
      quiet = TRUE)
  })

  expect_type(clim1, "list")
  expect_length(clim1, 3)

  expect_silent({
    clim2 <- prep_clim_data(
      mat_norm = f["mat_norm_pth"],
      mat_fut = f["mat_fut_pth2"],
      cmd_norm = f["cmd_norm_pth"],
      cmd_fut = f["cmd_fut_pth2"],
      map = f["map_norm_pth"],
      mwmt = f["mwmt_norm_pth"],
      mcmt = f["mcmt_norm_pth"],
      ccei = NULL,
      out_folder = out,
      clim_poly = test_files()$assess_poly_pth,
      overwrite = TRUE,
      scenario_name = "RCP 8.5",
      brks = clim1,
      quiet = TRUE)
  })

  expect_type(clim2, "list")
  expect_length(clim2, 3)

  expect_length(fs::dir_ls(out), 10) # no readme here

  # Scenario 2 has higher values than Scenario 1
  r <- fs::path(out, c("MAT_reclassRCP_4.5.tif", "MAT_reclassRCP_8.5.tif"))
  expect_true(all(fs::file_exists(r)))

  expect_lt(terra::global(terra::rast(r[1]), "mean", na.rm = TRUE)[1,1],
            terra::global(terra::rast(r[2]), "mean", na.rm = TRUE)[1,1])

  expect_snapshot_value(clim1, style = "json2")
  expect_snapshot_value(clim2, style = "json2")

  unlink(out, recursive = TRUE)

  # Multi gives same results
  fs::dir_create(out)

  expect_silent({
    clim12 <- prep_clim_data_multi(
      mat_norm = f["mat_norm_pth"],
      mat_fut = c(f["mat_fut_pth1"], f["mat_fut_pth2"]),
      cmd_norm = f["cmd_norm_pth"],
      cmd_fut = c(f["cmd_fut_pth1"], f["cmd_fut_pth2"]),
      map = f["map_norm_pth"],
      mwmt = f["mwmt_norm_pth"],
      mcmt = f["mcmt_norm_pth"],
      ccei = NULL,
      out_folder = out,
      clim_poly = test_files()$assess_poly_pth,
      overwrite = TRUE,
      scenario_name = c("RCP 4.5", "RCP 8.5"),
      quiet = TRUE)
  })

  expect_type(clim12, "list")
  expect_length(clim12, 3)
  expect_length(fs::dir_ls(out), 10) # no readme here

  # Multi is the same as a two staged prep
  expect_equal(clim2, clim12)

  expect_snapshot_value(clim1, style = "json2")
  expect_snapshot_value(clim2, style = "json2")

  unlink(out, recursive = TRUE)
})


test_that("prep_clim_data() - ccei", {

  skip_if_not(all(fs::file_exists(c(f["ccei_pth1"], f["ccei_pth2"]))))

  out <- fs::path(tempdir(), "processed")
  fs::dir_create(out)

  expect_silent({
    clim1 <- prep_clim_data(
      mat_norm = f["mat_norm_pth"],
      mat_fut = f["mat_fut_pth1"],
      cmd_norm = f["cmd_norm_pth"],
      cmd_fut = f["cmd_fut_pth1"],
      map = f["map_norm_pth"],
      mwmt = f["mwmt_norm_pth"],
      mcmt = f["mcmt_norm_pth"],
      ccei = f["ccei_pth1"],
      out_folder = out,
      clim_poly = test_files()$assess_poly_pth,
      overwrite = TRUE,
      scenario_name = "RCP 4.5",
      quiet = TRUE)
  })

  expect_type(clim1, "list")
  expect_length(clim1, 3)

  expect_silent({
    clim2 <- prep_clim_data(
      mat_norm = f["mat_norm_pth"],
      mat_fut = f["mat_fut_pth2"],
      cmd_norm = f["cmd_norm_pth"],
      cmd_fut = f["cmd_fut_pth2"],
      map = f["map_norm_pth"],
      mwmt = f["mwmt_norm_pth"],
      mcmt = f["mcmt_norm_pth"],
      ccei = f["ccei_pth2"],
      out_folder = out,
      clim_poly = test_files()$assess_poly_pth,
      overwrite = TRUE,
      scenario_name = "RCP 8.5",
      brks = clim1,
      quiet = TRUE)
  })

  expect_type(clim2, "list")
  expect_length(clim2, 3)

  expect_length(fs::dir_ls(out), 12) # no readme here

  expect_snapshot_value(clim1, style = "json2")
  expect_snapshot_value(clim2, style = "json2")

  unlink(out, recursive = TRUE)

  # Multi gives same results
  fs::dir_create(out)

  expect_silent({
    clim12 <- prep_clim_data_multi(
      mat_norm = f["mat_norm_pth"],
      mat_fut = c(f["mat_fut_pth1"], f["mat_fut_pth2"]),
      cmd_norm = f["cmd_norm_pth"],
      cmd_fut = c(f["cmd_fut_pth1"], f["cmd_fut_pth2"]),
      map = f["map_norm_pth"],
      mwmt = f["mwmt_norm_pth"],
      mcmt = f["mcmt_norm_pth"],
      ccei = c(f["ccei_pth1"], f["ccei_pth2"]),
      out_folder = out,
      clim_poly = test_files()$assess_poly_pth,
      overwrite = TRUE,
      scenario_name = c("RCP 4.5", "RCP 8.5"),
      quiet = TRUE)
  })

  expect_type(clim12, "list")
  expect_length(clim12, 3)
  expect_length(fs::dir_ls(out), 12) # no readme here

  # Multi is the same as a two staged prep
  expect_equal(clim2, clim12)

  expect_snapshot_value(clim1, style = "json2")
  expect_snapshot_value(clim2, style = "json2")

  unlink(out, recursive = TRUE)

  # Multi can use the same CCEI
  fs::dir_create(out)

  expect_warning({
    prep_clim_data_multi(
      mat_norm = f["mat_norm_pth"],
      mat_fut = c(f["mat_fut_pth1"], f["mat_fut_pth2"]),
      cmd_norm = f["cmd_norm_pth"],
      cmd_fut = c(f["cmd_fut_pth1"], f["cmd_fut_pth2"]),
      map = f["map_norm_pth"],
      mwmt = f["mwmt_norm_pth"],
      mcmt = f["mcmt_norm_pth"],
      ccei = f["ccei_pth1"],
      out_folder = out,
      clim_poly = test_files()$assess_poly_pth,
      overwrite = TRUE,
      scenario_name = c("RCP 4.5", "RCP 8.5"),
      quiet = TRUE)
  }, "Mismatch between scenarios and CCEI, using the same CCEI for all scenarios")

  unlink(out, recursive = TRUE)
})


