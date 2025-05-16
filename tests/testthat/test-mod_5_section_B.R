library(shinytest2)

# Remember to re-build/install package before running tests! Ctrl-Shift-B

test_that("Section B", {

  skip_on_ci()

  shiny_app <- mod_B_test()
  app <- AppDriver$new(shiny_app, variant = platform_variant(r_version = FALSE))
  app$set_window_size(width = 1619, height = 993)

  #app$set_window_size(width = 1304, height = 718)
  app$expect_values(screenshot_args = FALSE)
  app$stop()
})
