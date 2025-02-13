test_that("spatial data created", {

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
    "gain_mod" = 1)

  testServer(mod_spatial_server, args = list(
    volumes,
    df_loaded = reactive(NULL),
    cave = reactive(FALSE),
    # Use Testing files
    input_files = test_files()), {

      # Set the inputs
      session$setInputs(!!!test_inputs)
      session$setInputs(rng_chg_used = "multiple")

      session$flushReact()

      # Expect spatial files to load right away
      expect_s3_class(rng_poly(), "sf")
      expect_s3_class(assess_poly(), "sf")
      expect_s3_class(ptn_poly(), "sf")
      expect_null(nonbreed_poly())
      expect_s4_class(rng_chg_rast(), "SpatRaster")
      expect_s4_class(protected_rast(), "SpatRaster")

      expect_type(clim_vars(), "list")
      expect_s4_class(clim_vars()$mat, "SpatRaster")
      expect_null(clim_vars()$ccei)
      expect_s3_class(clim_vars()$clim_poly, "sf")
      expect_s3_class(clim_readme(), "data.frame")

      expect_true(is.matrix(rng_chg_mat()))

      # No spatial without button
      expect_error(spat_res())   # Reactive
      expect_null(spat_thresh()) # ReactiveVal

      # Run spatial
      doSpatial(1)
      session$flushReact() %>%
        suppressWarnings() # TODO: Catch this warning in the app, don't suppress here

      # Have spatial
      expect_type(spat_res(), "list")
      expect_s3_class(spat_thresh(), "data.frame")
      expect_s3_class(range_poly_clip(), "sf")
      expect_s3_class(range_poly_clim(), "sf")
      expect_s3_class(spatial_data(), "data.frame")

      r <- session$getReturned()
      expect_snapshot_value(r$spatial_data(), style = "json2")
    })
})
