# test loading climate variables

expect_silent({
  f <- test_files()
  clim_vars <- get_clim_vars(f$clim_dir, f$scn_nms)

  # nonbreed <- st_read(file.path(file_dir, "nonbreed_poly.shp"), agr = "constant",
  #                     quiet = TRUE)
  # assess <- st_read(file.path(file_dir, "assess_poly.shp"), agr = "constant",
  #                   quiet = TRUE)
  # rng_high <- st_read(file.path(file_dir, "rng_poly.shp"), agr = "constant",
  #                     quiet = TRUE)
  # hs <- raster::raster(file.path(file_dir, "rng_chg_45.tif"))
})

test_that("basic version works", {
  expect_type(clim_vars, "list")
})

test_that("error if no crs",{

  t <- fs::dir_copy(f$clim_dir, fs::path_temp("clim_files"))

  # Create MAT reclass without CRS
  clim_vars_ncrs <- clim_vars
  terra::crs(clim_vars_ncrs[[1]]) <- ""
  terra::writeRaster(
    clim_vars_ncrs[[1]],
    fs::path(t, paste0("MAT_reclass", names(clim_vars_ncrs[[1]]), ".tif")),
    overwrite = TRUE)

  expect_message(expect_error(get_clim_vars(t, f$scn_nms), "does not have a CRS"))

  # Clean up
  fs::dir_delete(t)
})

test_that("trimming happens", {

  t <- fs::dir_copy(f$clim_dir, fs::path_temp("clim_files"))

  # Replace MAT
  raster::extend(clim_vars[[1]], 20) %>%
    terra::writeRaster(fs::dir_ls(t, regexp = "MAT_reclass"), overwrite = TRUE)

  # Expect trimming
  expect_message(clim_vars2 <- get_clim_vars(t, f$scn_nms), "doing trim")

  # But no effect on results
  expect_equal(terra::values(clim_vars[[1]][[1]]),
               terra::values(clim_vars2[[1]][[1]]))

  # Clean up
  fs::dir_delete(t)
})

test_that("error when two files or missing files",{
  t <- fs::dir_copy(f$clim_dir, fs::path_temp("clim_files"))

  # Two files (when there should be one)
  mat_files <- fs::dir_ls(t, regexp = "MAT")
  mat_files2 <- stringr::str_replace(mat_files, "reclass", "reclass2")
  fs::file_copy(mat_files, mat_files2)
  expect_error(get_clim_vars(t), "number of files matching")
  fs::file_delete(mat_files2)

  # Missing required files (MAT)
  fs::file_delete(mat_files)
  expect_error(get_clim_vars(t), "There is no file in")

  # other files are allowed to be missing
  fs::dir_delete(t)
  t <- fs::dir_copy(f$clim_dir, fs::path_temp("clim_files"))
  fs::file_delete(fs::dir_ls(t, regexp = "MWMT"))
  expect_null(get_clim_vars(t, f$scn_nms)$htn)

  # Clean up
  fs::dir_delete(t)
})

test_that("multi_scenario works as expected", {
  expect_error(
    get_clim_vars(f$clim_dir, scenario_names = c("RCP2.6", "RCP4.5", "RCP85")),
    "does not match")
})

# remove the temp directory (just in case)
fs::dir_delete(fs::path_temp())
