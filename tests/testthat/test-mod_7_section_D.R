library(shinytest2)

# Remember to re-build/install package before running tests! Ctrl-Shift-B

test_that("Section D", {
  shiny_app <- mod_D_test()
  app <- AppDriver$new(shiny_app, variant = platform_variant(r_version = FALSE))
  app$set_window_size(width = 1619, height = 993)
  a <- app$wait_for_value(output = "test-map_D4", timeout = 10000)
  Sys.sleep(1)

  app$expect_screenshot(name = "01-full-spatial")
  app$stop()
})

# Optional spatial
test_that("Section D - min required", {
  shiny_app <- mod_D_test(input_files = test_files(min_req = TRUE))
  app <- AppDriver$new(shiny_app, variant = platform_variant(r_version = FALSE))
  app$set_window_size(width = 1619, height = 993)
  a <- app$wait_for_value(output = "test-ui_D4", timeout = 10000)
  Sys.sleep(1)

  app$expect_screenshot(name = "02-min-spatial")
  app$stop()
})
