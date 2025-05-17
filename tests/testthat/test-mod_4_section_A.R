test_that("A overall", {
  testServer(mod_A_server, args = list(
    spatial = suppressMessages(test_spatial())), {

      # Maps
      expect_message(output$map_texp, "projecting raster") %>%
        expect_message("projecting raster") # Message triggered by output (?)
      expect_s3_class(output$map_texp, "json")
      expect_s3_class(output$map_cmd, "json")
      expect_error(output$map_ccei)

      # Tables
      expect_type(output$tbl_texp, "list")
      expect_type(output$tbl_cmd, "list")
      expect_error(output$tbl_ccei)
    })
})

test_that("A min spatial", {

  sp <- test_files(protected_poly_pth = NA,
                   ptn_poly_pth = NA,
                   rng_chg_pth_1 = NA,
                   rng_chg_pth_2 = NA) %>%
    test_data() %>%
    test_spatial() %>%
    suppressMessages()

  testServer(mod_A_server, args = list(
    spatial = suppressMessages(test_spatial())), {
      expect_message(output$map_texp, "projecting raster") %>%
        expect_message("projecting raster") # Message triggered by output (?)
      expect_s3_class(output$map_texp, "json")
      expect_s3_class(output$map_cmd, "json")
      expect_error(output$map_ccei)

      expect_type(output$tbl_texp, "list")
      expect_type(output$tbl_cmd, "list")
      expect_error(output$tbl_ccei)
    })
})
