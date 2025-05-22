
# NOTE: These require test data created by /extdata/data_ccei_mini.R

expect_silent({
  path_ccei <- fs::path_package("extdata", "ccei_test", package = "ccviR")
})

test_that("ccei_clip()", {
  expect_silent(c <- ccei_clip())
  expect_s4_class(c, "SpatExtent")
})

test_that("combine_xxx()", {

  expect_silent(r <- combine_historical(path_ccei))
  expect_s3_class(r, "data.frame")
  expect_true(all(stringr::str_detect(r$file, "historical")))
  expect_true(all(stringr::str_detect(r$file, "prec|tmin|tmax")))
  expect_named(r, c("file", "year", "month", "var", "group"))

  expect_message(r <- combine_future(path_ccei),
                 "Using models \\(n = 11\\) for ssp585")
  expect_s3_class(r, "data.frame")
  expect_true(all(stringr::str_detect(r$file, "future")))
  expect_true(all(stringr::str_detect(r$file, "prec|tmin|tmax")))
  expect_named(r, c("file", "model", "ssp", "var", "group"))
})


test_that("ccei_vars() - historical", {

  r <- combine_historical(path_ccei) %>%
    dplyr::filter(group == 1960)

  v1 <- ccei_by_hand(r, 1, 1)  # cell (1,1)
  v2 <- ccei_by_hand(r, 2, 2)  # cell (2,2)
  v3 <- ccei_by_hand(r, 3, 3)  # cell (3,3)

  # Function calculations for all cells
  expect_message(
    v <- ccei_vars(prec_files = r$file[r$var == "prec"],
                   tmax_files = r$file[r$var == "tmax"],
                   tmin_files = r$file[r$var == "tmin"],
                   ccei_clip()),
    "Month: 1") %>% suppressMessages()

  expect_s4_class(v, "SpatRaster")
  expect_equal(terra::crs(v), terra::crs("epsg:4326"))
  expect_named(v, c("cmd", "tmean"))

  # Compare
  expect_equal(unlist(v[1,1]), v1)
  expect_equal(unlist(v[2,2]), v2)
  expect_equal(unlist(v[3,3]), v3)

  expect_snapshot(as.data.frame(v))
})

test_that("ccei_vars() - future", {

  r <- combine_future(path_ccei, quiet = TRUE) %>%
    dplyr::filter(group == "ACCESS-ESM1-5-ssp585")

  v1 <- ccei_by_hand(r, 1, 1)  # cell (1,1)
  v2 <- ccei_by_hand(r, 2, 2)  # cell (2,2)
  v3 <- ccei_by_hand(r, 3, 3)  # cell (3,3)

  # Function calculations for all cells
  expect_message(
    v <- ccei_vars(prec_files = r$file[r$var == "prec"],
                   tmax_files = r$file[r$var == "tmax"],
                   tmin_files = r$file[r$var == "tmin"],
                   ccei_clip()),
    "Month: 1") %>% suppressMessages()

  expect_s4_class(v, "SpatRaster")
  expect_equal(terra::crs(v), terra::crs("epsg:4326"))
  expect_named(v, c("cmd", "tmean"))

  # Compare
  expect_equal(unlist(v[1,1]), v1)
  expect_equal(unlist(v[2,2]), v2)
  expect_equal(unlist(v[3,3]), v3)

  expect_snapshot(as.data.frame(v))
})

test_that("ccei_values()", {

  # Test historical (~15s to run)
  r <- combine_historical(path_ccei)
  expect_silent(o <- prep_out(path_ccei, "intermediate", "hist"))
  expect_message(ccei_values(filter(r, group == "1989"), o, aggregate = TRUE, overwrite = TRUE),
                 paste0("1989 - ", Sys.Date())) %>%
    expect_message("Combining and Saving rasters with annual data") %>%
    expect_message("Final calculations") %>%
    suppressMessages()
  expect_silent(ccei_values(r, o, aggregate = TRUE, overwrite = TRUE, quiet = TRUE))

  # Test future
  r <- combine_future(path_ccei, quiet = TRUE)
  expect_silent(o <- prep_out(path_ccei, "intermediate", "future"))
  expect_message(ccei_values(r, o, overwrite = TRUE),
                 paste0("MRI-ESM2-0-ssp585 - ", Sys.Date())) %>%
    expect_message("Combining and Saving rasters with annual data") %>%
    expect_no_message(message = "Final calculations") %>%
    suppressMessages()

  # Check output
  d <- fs::path(path_ccei, "intermediate")

  # Expect intermediate outputs
  expect_length(h <- fs::dir_ls(d, regexp = "hist_\\d{4}.tif"), 30)
  expect_length(f <- fs::dir_ls(d, regexp = "future_[0-9A-Za-z-]+\\-ssp585.tif"), 11)

  expect_named(h <- terra::rast(h[1]), c("cmd", "tmean"))
  expect_named(f <- terra::rast(f[1]), c("cmd", "tmean"))

  # Expect grouped outputs
  expect_true(all(fs::file_exists(fs::path(
    d, c("hist_groups_cmd.tif", "hist_groups_tmean.tif")))))
  expect_true(all(fs::file_exists(fs::path(
    d, c("future_groups_cmd.tif", "future_groups_tmean.tif")))))

  # Expect combined outputs
  expect_true(fs::file_exists(h <- fs::path(d, "hist_all_vars.tif")))
  expect_true(fs::file_exists(f <- fs::path(d, "future_all_vars.tif")))

  expect_named(h <- terra::rast(h), c("cmd_mean", "cmd_sd", "tmean_mean", "tmean_sd"))
  expect_named(f <- terra::rast(f))

  # Snapshots
  expect_snapshot(as.data.frame(h))
  expect_snapshot(as.data.frame(f))
})

test_that("calc_ccei()", {

  h <- terra::rast(fs::path(path_ccei, "intermediate", "hist_all_vars.tif"))
  f <- terra::rast(fs::path(path_ccei, "intermediate", "future_all_vars.tif"))

  expect_message(calc_ccei(path_ccei, scenario = "ssp585", overwrite = TRUE),
                 "Using models \\(n = 11\\)")

  f <- fs::path(path_ccei, "ccei_ssp585.tif")
  expect_true(fs::file_exists(f))
  expect_named(r <- terra::rast(f), "ccei_ssp585")

  expect_snapshot(terra::values(r))
})

test_that("prep_ccei()", {
  skip("Can't run final, because no ssp245 files (to save space).")
})

test_that("calc_sed()", {

  hist <- fs::path(path_ccei, "intermediate", "hist_all_vars.tif") %>%
    terra::rast() %>%
    terra::values()
  future <- fs::path(path_ccei, "intermediate", "future_UKESM1-0-LL-ssp585.tif") %>%
    terra::rast() %>%
    terra::values()

  expect_silent(
    sed <- calc_sed(b = list(future[, "cmd"], future[, "tmean"]),
                    a = list(hist[, "cmd_mean"], hist[, "tmean_mean"]),
                    s = list(hist[, "cmd_sd"], hist[,"tmean_sd"]))
  )
  expect_type(sed, "double")
  expect_length(sed, length(future[, "cmd"]))

  expect_snapshot(sed)
})
