library(shinytest2)

# Remember to re-install package before running tests!
test_that("Full app", {

  shiny_app <- ccvi_app2(input_files = test_files())
  app <- AppDriver$new(shiny_app, variant = "ubuntu")

  app$set_window_size(width = 1619, height = 993)

  # Home
  app$expect_values()
  app$click("home-continue")

  # Species
  app$set_inputs(`species-assessor_name` = "Dr. Test")
  app$set_inputs(`species-geo_location` = "Canada")
  app$set_inputs(`species-species_name` = "Testous testa")
  app$set_inputs(`species-common_name` = "Test sp.")

  app$expect_screenshot()
  app$click("species-continue")

  # Spatial
  # Uses built in tests files (can't test ShinyFile inputs)
  app$wait_for_idle()
  Sys.sleep(1)
  app$expect_screenshot()
  app$click("spatial-startSpatial")
  #app$click("spatial-shinyalert")
  app$wait_for_idle()
  app$expect_screenshot()
  app$click("spatial-continue")

  # Section A
  app$wait_for_idle()
  app$expect_screenshot()
  app$click("section_a-continue")

  # Section B
  app$wait_for_idle()
  app$click("section_b-help_B1")
  app$wait_for_idle()
  Sys.sleep(1)
  app$expect_screenshot()
  app$click(selector = "button[data-dismiss='modal']")
  app$expect_screenshot()
  app$set_inputs(`section_b-B1` = "2")
  app$set_inputs(`section_b-B2a` = "3")
  app$set_inputs(`section_b-B2b` = "0")
  app$set_inputs(`section_b-B3` = "1")
  app$expect_values(input = TRUE, export = TRUE)
  app$click("section_b-continue")

  # Section C
  app$wait_for_idle()
  app$expect_screenshot()
  app$set_inputs(`section_c-C1` = "2")
  app$set_inputs(`section_c-C2bii` = "0")
  app$set_inputs(`section_c-C3` = "2")
  app$set_inputs(`section_c-C4a` = "0")
  app$set_inputs(`section_c-C4d` = "1")
  app$click("section_c-help_C4e")
  app$wait_for_idle()
  Sys.sleep(1)
  app$expect_screenshot()
  app$click(selector = "button[data-dismiss='modal']")
  app$set_inputs(`section_c-C5a` = "2")
  app$set_inputs(`section_c-C6` = "1")
  app$expect_values(input = TRUE, export = TRUE)
  app$click("section_c-continue")

  # Section D
  app$wait_for_idle()
  app$expect_screenshot()
  app$set_inputs(`section_d-D1` = "2")
  app$set_inputs(`section_d-D4` = "1")
  app$expect_values(input = TRUE, export = TRUE)
  app$click("section_d-continue")

  # Results
  app$wait_for_idle()
  app$expect_screenshot()
  app$click("results-calcIndex")
  app$wait_for_idle()
  app$expect_screenshot()
  app$expect_values(export = TRUE)

#
#   # Save Data
#   app$click("save-downloadData")
#   app$set_inputs(`save-downloadData` = c("shiny_test", "", "wd"), allow_no_input_binding_ = TRUE)


})
