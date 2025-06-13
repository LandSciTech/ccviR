library(shinytest2)

# Remember to re-build/install package before running tests! Ctrl-Shift-B

#' mod_C_test()
#' mod_C_test(tax_grp = "Reptile") # Test different questions
#' mod_C_test(df_loaded = test_df_loaded())

test_that("Section C", {
  skip_on_ci()
  skip_on_covr()

  shiny_app <- mod_C_test()
  app <- AppDriver$new(shiny_app, variant = platform_variant(r_version = FALSE))
  app$set_window_size(width = 1619, height = 993)
  a <- app$wait_for_value(output = "test-map_C2bi", timeout = 10000)
  Sys.sleep(1)

  app$expect_screenshot(name = "01-full-spatial")
  app$stop()
})

# Optional spatial
test_that("Section C - min required", {
  skip_on_ci()
  skip_on_covr()

  shiny_app <- mod_C_test(input_files = test_files(min_req = TRUE))
  app <- AppDriver$new(shiny_app, variant = platform_variant(r_version = FALSE))
  app$set_window_size(width = 1619, height = 993)
  a <- app$wait_for_value(output = "test-map_C2ai", timeout = 10000)
  Sys.sleep(1)

  app$expect_screenshot(name = "02-min-spatial")
  app$stop()
})
