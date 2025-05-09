library(shinytest2)

# Remember to re-build/install package before running tests! Ctrl-Shift-B

#' mod_C_test()
#' mod_C_test(tax_grp = "Reptile") # Test different questions
#' mod_C_test(df_loaded = test_df_loaded())

test_that("Section C", {
  shiny_app <- mod_C_test()
  app <- AppDriver$new(shiny_app, variant = platform_variant(r_version = FALSE))
  app$set_window_size(width = 1619, height = 993)
  app$wait_for_idle()
  Sys.sleep(5)

  app$expect_screenshot(name = "01-full-spatial")
  app$stop()
})

# Optional spatial
test_that("Section C - min required", {
  shiny_app <- mod_C_test(input_files = test_files(min_req = TRUE))
  app <- AppDriver$new(shiny_app, variant = platform_variant(r_version = FALSE))
  app$set_window_size(width = 1619, height = 993)
  app$wait_for_idle()
  Sys.sleep(1)

  app$expect_screenshot(name = "02-min-spatial")
  app$stop()
})
