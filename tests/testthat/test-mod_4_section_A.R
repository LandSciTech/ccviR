test_that("A overall", {
  testServer(mod_A_server, args = list(
    spatial_details = suppressMessages(test_spatial())), {

      # Maps
      expect_message(output$texp_map, "projecting raster") %>%
        expect_message("projecting raster") # Message triggered by output (?)
      expect_s3_class(output$texp_map, "json")
      expect_s3_class(output$cmd_map, "json")
      expect_error(output$ccei_map)

      # Tables
      expect_type(output$texp_tbl, "list")
      expect_type(output$cmd_tbl, "list")
      expect_error(output$ccei_tbl)
    })
})

test_that("A min spatial", {

  sp <- test_files(protected_rast_pth = NA,
                   ptn_poly_pth = NA,
                   rng_chg_pth_1 = NA,
                   rng_chg_pth_2 = NA) %>%
    test_data() %>%
    test_spatial() %>%
    suppressMessages()

  testServer(mod_A_server, args = list(
    spatial_details = suppressMessages(test_spatial())), {
      expect_message(output$texp_map, "projecting raster") %>%
        expect_message("projecting raster") # Message triggered by output (?)
      expect_s3_class(output$texp_map, "json")
      expect_s3_class(output$cmd_map, "json")
      expect_error(output$ccei_map)

      expect_type(output$texp_tbl, "list")
      expect_type(output$cmd_tbl, "list")
      expect_error(output$ccei_tbl)
    })
})
