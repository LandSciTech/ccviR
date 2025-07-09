test_that("A overall", {
  testServer(mod_A_server, args = list(
    spatial = suppressMessages(test_spatial()),
    species = reactive(test_species())), {

      # Maps
      #expect_message(output$map_texp, "projecting raster") %>%
      #  expect_message("projecting raster") # Message triggered by output (?)
      expect_s3_class(output$map_texp, "json") %>%
        expect_message("Projecting raster") %>%
        expect_message("Projecting raster") %>%
        expect_message("Projecting raster")
      expect_s3_class(output$map_cmd, "json")
      expect_s3_class(output$map_ccei, "json")

      # Tables
      expect_type(output$tbl_texp, "list")
      expect_type(output$tbl_cmd, "list")
      expect_type(output$tbl_ccei, "list")
    })
})

test_that("A min spatial", {  # NO CCEI

  sp <- test_files(min_req = TRUE) %>%
    test_data() %>%
    test_spatial(quiet = TRUE)

  testServer(mod_A_server, args = list(
    spatial = sp, species = reactive(test_species())), {

      expect_s3_class(output$map_texp, "json") %>%
        expect_message("Projecting raster") %>%
        expect_message("Projecting raster")
      expect_s3_class(output$map_texp, "json")
      expect_s3_class(output$map_cmd, "json")
      expect_error(output$map_ccei)

      expect_type(output$tbl_texp, "list")
      expect_type(output$tbl_cmd, "list")

      expect_true(stringr::str_detect(
        output$ui_ccei$html, "Optional Spatial Data not provided"))
    })
})

test_that("A non-migratory", { # NO CCEI

  sp <- test_files(min_req = TRUE) %>%
    test_data() %>%
    test_spatial(quiet = TRUE)

  testServer(mod_A_server, args = list(
    spatial = sp, species = reactive(test_species() %>% mutate(mig = FALSE))), {

      expect_s3_class(output$map_texp, "json") %>%
        expect_message("Projecting raster") %>%
        expect_message("Projecting raster")
      expect_s3_class(output$map_texp, "json")
      expect_s3_class(output$map_cmd, "json")
      expect_error(output$map_ccei)
      expect_error(output$ui_ccei,
                   "No migratory exposure for non-migratory species")

      expect_type(output$tbl_texp, "list")
      expect_type(output$tbl_cmd, "list")

    })
})
