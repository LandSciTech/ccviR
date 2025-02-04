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
    session$flushReact()

    # No spatial without button
    expect_false(spat_res())
    expect_null(range_poly_in())
    expect_null(assess_poly())
    expect_null(ptn_poly())

    # Run spatial
    doSpatial(1)
    session$flushReact()

    # Have spatial
    expect_s3_class(spat_res2(), "data.frame")
    expect_s4_class(clim_vars()$mat, "SpatRaster")
    expect_null(clim_vars()$ccei)
    expect_s3_class(clim_vars()$clim_poly, "sf")
    expect_s3_class(clim_readme(), "data.frame")
    expect_s3_class(range_poly(), "sf")
    expect_s3_class(range_poly_clim(), "sf")
    expect_s3_class(ptn_poly(), "sf")
    expect_null(nonbreed_poly())
    expect_s3_class(assess_poly(), "sf")
    expect_s4_class(hs_rast(), "SpatRaster")
    expect_true(is.matrix(hs_rcl_mat()))
    expect_s3_class(spatial_data(), "data.frame")

    r <- session$getReturned()
    expect_snapshot_value(r$spatial_data(), style = "json2")
    expect_snapshot_value(r$index_res(), style = "json2")
    expect_snapshot_value(r$spatial_details$spat_res(), style = "json2")
  })
})
