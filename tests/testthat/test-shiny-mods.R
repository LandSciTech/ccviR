library(shinytest2)

# Remember to re-build/install package before running tests! Ctrl-Shift-B

# devtools::test(filter = "shiny-mods$") # Run just the shiny apps test programatically

test_that("Full app", {
  skip_on_ci()
  skip_on_covr()

  shiny_app <- ccvi_app(input_files = test_files())
  app <- AppDriver$new(shiny_app, variant = platform_variant(r_version = FALSE))
  app$set_window_size(width = 1619, height = 993, wait = FALSE)
  Sys.sleep(1)

  # Home
  app$expect_values(name = "01-home-values", transform = clean_paths) # Expect no spatial outputs
  app$click("home-continue")

  # Species
  app$set_inputs(`species-assessor_name` = "Dr. Test")
  app$set_inputs(`species-geo_location` = "Canada")
  app$set_inputs(`species-species_name` = "Testous testa")
  app$set_inputs(`species-common_name` = "Test sp.")
  app$set_inputs(`species-mig` = TRUE)

  expect_screenshot_local(app, name = "02-species-ready")
  app$click("species-continue")

  # Spatial
  # Uses built in tests files (can't test ShinyFile inputs)
  app$set_inputs(`spatial-rng_chg_used` = "multiple")
  app$wait_for_idle()
  Sys.sleep(1)
  expect_screenshot_local(app, name = "03-spatial-ready")

  app$click("spatial-startSpatial")

  app$wait_for_idle()
  Sys.sleep(1)
  expect_screenshot_local(app, name = "04-spatial-run")
  app$click("spatial-continue")
  Sys.sleep(1)

  # Section A
  app$wait_for_idle()
  Sys.sleep(1)
  expect_screenshot_local(app, name = "05-sectionA-ready")
  app$click("section_a-continue")
  Sys.sleep(1)

  # Section B
  app$wait_for_idle()
  app$click("section_b-help_B1")
  app$wait_for_idle()
  Sys.sleep(1)
  expect_screenshot_local(app, name = "06-sectionB-help")
  app$click(selector = "button[data-dismiss='modal']")
  Sys.sleep(1)
  expect_screenshot_local(app, name = "07-sectionB-ready")
  app$set_inputs(`section_b-B1` = "0")
  app$set_inputs(`section_b-B2a` = "0")
  app$set_inputs(`section_b-B2b` = "0")
  app$set_inputs(`section_b-B3` = "0")
  app$expect_values(input = TRUE, export = TRUE, name = "08-sectionB-values", transform = clean_paths)
  app$click("section_b-continue")

  # Section C
  app$wait_for_idle()
  expect_screenshot_local(app, name = "09-sectionC-ready")

  app$set_inputs(`section_c-C1` = "0")
  app$set_inputs(`section_c-C2bii` = "0")
  app$set_inputs(`section_c-C3` = "0")
  app$set_inputs(`section_c-C4a` = "0")
  app$set_inputs(`section_c-C4d` = "0")
  app$click("section_c-help_C4e")
  app$wait_for_idle()
  Sys.sleep(1)
  expect_screenshot_local(app, name = "10-sectionC-help")

  app$click(selector = "button[data-dismiss='modal']")
  app$set_inputs(`section_c-C5a` = "0")
  app$set_inputs(`section_c-C6` = "0")
  app$wait_for_idle()
  Sys.sleep(1)
  app$expect_values(input = TRUE, export = TRUE, name = "11-sectionC-values", transform = clean_paths)
  app$click("section_c-continue")

  # Section D
  app$wait_for_idle()
  Sys.sleep(1)
  expect_screenshot_local(app, name = "12-sectionD-ready")

  app$set_inputs(`section_d-D1` = "2")
  app$expect_values(input = TRUE, export = TRUE, name = "13-sectionD-set", transform = clean_paths)
  app$click("section_d-continue")
  Sys.sleep(2)

  # Results
  app$wait_for_idle()
  Sys.sleep(1)
  expect_screenshot_local(app, name = "14-results-ready")

  app$click("results-calcIndex")
  app$wait_for_idle()
  Sys.sleep(1)
  expect_screenshot_local(app, name = "15-results-run")

  # Modify and rerun
  app$set_inputs(`section_d-D1` = "3")
  expect_screenshot_local(app, name = "16-results-async")

  app$set_inputs(`section_b-B1` = "3")
  app$set_inputs(`section_b-B2a` = "3")
  app$set_inputs(`section_b-B2b` = "3")
  app$set_inputs(`section_b-B3` = "2")
  app$set_inputs(`section_c-C1` = "3")
  app$set_inputs(`section_c-C2bii` = "3")
  app$set_inputs(`section_c-C2c` = "2")
  app$set_inputs(`section_c-C2d` = "3")
  app$click("results-calcIndex")
  app$wait_for_idle()
  app$expect_values(export = TRUE, name = "17-results-change", transform = clean_paths)

#
#   # Save Data
#   app$click("save-downloadData")
#   app$set_inputs(`save-downloadData` = c("shiny_test", "", "wd"), allow_no_input_binding_ = TRUE)


})

