library(shinytest2)

# Remember to re-build/install package before running tests! Ctrl-Shift-B

# devtools::test(filter = "shiny-mods-data-prep") # Run just the shiny apps test programatically


test_that("Data Prep app - inputs", {

  shiny_app <- mod_data_prep_test()
  app <- AppDriver$new(shiny_app, variant = platform_variant(r_version = FALSE))
  app$set_window_size(width = 1619, height = 993)

  # Standard setup
  app$wait_for_idle()
  Sys.sleep(1)
  app$expect_screenshot(name = "01-ready")

  # Add scenarios
  app$set_inputs(`data-scn_n` = 2)
  app$expect_screenshot(name = "01_2-ready")

  # Half of MWMT/MCMT
  app$click("data-mwmt_norm_pth_clear")
  app$expect_screenshot(name = "02-mwmt-mcmt-mismatch")

  # Missing inputs
  app$click("data-mat_norm_pth_clear")
  app$expect_screenshot(name = "03-missing-inputs")

})

test_that("Data Prep app - run", {

  # Use options by hand
  # - shinytest2 doesn't work with runApp()
  # - withr::with_options doesn't work with shinyApp()

  # Ensure prepared data are findable by these tests
  # Note: This has to go HERE, not in the options list below. Because of lazy
  #    evaluation, if you put it as a function argument it'll have the wrong location.
  path <- fs::path_abs(test_path("TESTING DATA UI OUTPUTS"))

  # Before we launch, there is no dir
  expect_false(fs::dir_exists(test_path("TESTING DATA UI OUTPUTS")))

  shiny_app <- mod_data_prep_test()
  app <- AppDriver$new(
    shiny_app, variant = platform_variant(r_version = FALSE),
    options = list("ccviR.test_data_prep" = path))
  app$set_window_size(width = 1619, height = 993)

  # Now we have the dir - but no files
  expect_true(fs::dir_exists(test_path("TESTING DATA UI OUTPUTS")))
  expect_length(fs::dir_ls(test_path("TESTING DATA UI OUTPUTS")), 0)

  # Run data prep
  app$click("data-submit")
  app$wait_for_idle()
  Sys.sleep(1)

  # Now we have the dir - and the files
  expect_true(fs::dir_exists(test_path("TESTING DATA UI OUTPUTS")))
  f0 <- fs::dir_ls(test_path("TESTING DATA UI OUTPUTS"))
  f <- fs::path_file(f0) # Remove paths
  expect_gt(length(f), 0)
  expect_true(all(c("climate_data_readme.csv",
                    "CMD_reclassRCP_4.5.tif", "MAT_reclassRCP_4.5.tif") %in% f))
  expect_false(any(c("CMD_reclassRCP_8.5.tif", "MAT_reclassRCP_8.5.tif") %in% f)) # Only one scenario

  #app$expect_screenshot(name = "04-run") # This changes every time because of the time stamp...

  # Clean up
  fs::file_delete(f0)

  # Add scenarios
  app$set_inputs(`data-scn_n` = 2)

  # Run data prep
  app$click("data-submit")
  app$wait_for_idle()
  Sys.sleep(1)

  # Expect second scenario files
  f0 <- fs::dir_ls(test_path("TESTING DATA UI OUTPUTS"))
  f <- fs::path_file(f0)
  expect_gt(length(f), 0)
  expect_true(all(c("climate_data_readme.csv",
                    "CMD_reclassRCP_4.5.tif", "CMD_reclassRCP_8.5.tif",
                    "MAT_reclassRCP_4.5.tif", "MAT_reclassRCP_8.5.tif") %in% f))

  # Clean up
  unlink(test_path("TESTING DATA UI OUTPUTS"), recursive = TRUE)
})
