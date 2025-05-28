# Local Spatial tests, included data from misc folder

expect_silent({
  volumes <- server_setup()

  test_inputs <- list(
    "lost_from" = -1,
    "lost_to" = -1,
    "maint_from" = 0,
    "maint_to" = 0,
    "gain_from" = 1,
    "gain_to" = 1,
    "ns_from" = 99,
    "ns_to" = 99,
    "gain_mod" = 1,
    "gain_mod_comm" = "")
  input_files <- test_files()
})

# NOTE: "Error in `trace_back(top = getOption("testthat_topenv"), bottom = trace_env)`: Can't find `bottom` on the call tree."
#
# This usually means that the snapshot has changed, use:
#
# testthat::snapshot_review()

test_that("spatial data created", {

  testServer(mod_spatial_server, args = list(
    volumes,
    df_loaded = reactive(NULL),
    cave = reactive(FALSE),
    # Use Testing files
    input_files = input_files), {

      # Set the inputs
      session$setInputs(!!!test_inputs)
      session$setInputs(rng_chg_used = "multiple")

      session$flushReact()

      # Expect spatial files to load right away
      expect_s3_class(rng_poly(), "sf")
      expect_s3_class(assess_poly(), "sf")
      expect_s3_class(ptn_poly(), "sf")
      expect_s3_class(nonbreed_poly(), "sf")
      expect_s4_class(rng_chg_rast(), "SpatRaster")
      expect_s3_class(protected_poly(), "sf")

      expect_equal(terra::nlyr(rng_chg_rast()), 2)

      expect_type(clim_vars(), "list")
      expect_s4_class(clim_vars()$mat, "SpatRaster")
      expect_s4_class(clim_vars()$ccei, "SpatRaster")
      expect_s3_class(clim_vars()$clim_poly, "sf")
      expect_s3_class(clim_readme(), "data.frame")

      expect_true(is.matrix(rng_chg_mat()))

      # No spatial without button
      expect_error(spat_res())   # Reactive
      expect_null(spat_thresh()) # ReactiveVal

      # Run spatial
      doSpatial(1)
      session$flushReact() %>%
        suppressWarnings() %>% # TODO: Catch this warning in the app, don't suppress here
        suppressMessages()

      # Have spatial
      expect_type(spat_res(), "list")
      expect_s3_class(spat_thresh(), "data.frame")
      expect_s3_class(range_poly_clip(), "sf")
      expect_s3_class(range_poly_clim(), "sf")
      expect_s3_class(spat_run(), "data.frame")

      # Get Spat run and clean up paths
      r <- session$getReturned()$spat_run() %>%
        dplyr::mutate(dplyr::across(dplyr::contains("pth"), fs::path_file))
      expect_snapshot_value(r, style = "json2")
    })
})

# Can have 1 range change even if clim data has multiple scenarios
test_that("1 range with mult scenarios", {

  testServer(mod_spatial_server, args = list(
    volumes,
    df_loaded = reactive(NULL),
    cave = reactive(FALSE),
    # Use Testing files
    input_files = input_files), {

      # Set the inputs
      session$setInputs(!!!test_inputs)
      session$setInputs(rng_chg_used = "one")

      session$flushReact()

      # Expect spatial files to load right away
      expect_s4_class(rng_chg_rast(), "SpatRaster")
      expect_equal(terra::nlyr(rng_chg_rast()), 1)   # Only 1
      expect_type(clim_vars(), "list")
      expect_s4_class(clim_vars()$mat, "SpatRaster")
      expect_equal(terra::nlyr(clim_vars()$mat), 2)  # Still 2
      expect_s4_class(clim_vars()$ccei, "SpatRaster")
      expect_s3_class(clim_vars()$clim_poly, "sf")
      expect_s3_class(clim_readme(), "data.frame")

      expect_true(is.matrix(rng_chg_mat()))

      # No spatial without button
      expect_error(spat_res())   # Reactive
      expect_null(spat_thresh()) # ReactiveVal

      # Run spatial
      doSpatial(1)
      session$flushReact() %>%
        suppressWarnings() %>% # TODO: Catch this warning in the app, don't suppress here
        suppressMessages()

      # Have spatial
      expect_type(spat_res(), "list")
      expect_s3_class(spat_thresh(), "data.frame")
      expect_s3_class(range_poly_clip(), "sf")
      expect_s3_class(range_poly_clim(), "sf")
      expect_s3_class(spat_run(), "data.frame")

      # Expect two scenarios but equal range changes
      expect_equal(nrow(spat_res()$spat_table), 2)
      expect_length(unique(spat_res()$spat_table$range_change), 1)

      # Get Spat run and clean up paths
      r <- session$getReturned()$spat_run() %>%
        dplyr::mutate(dplyr::across(dplyr::contains("pth"), fs::path_file))
      expect_snapshot_value(r, style = "json2")
    })
})
