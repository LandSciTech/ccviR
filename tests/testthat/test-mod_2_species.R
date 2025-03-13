library(shinytest2)

# Remember to re-build/install package before running tests! Ctrl-Shift-B

test_that("Species Fills in previous data", {

  # TODO: After Chrome update, shinytests2 fail, use chromium instead and DON'T
  #       update (temp?)
  Sys.setenv(CHROMOTE_CHROME = "chromium")

  shiny_app <- mod_species_test()
  app <- AppDriver$new(shiny_app, variant = platform_variant(r_version = FALSE))
  app$set_window_size(width = 1619, height = 993)

  #app$set_window_size(width = 1304, height = 718)
  app$expect_values(screenshot_args = FALSE)
  vals <- app$get_values()
  app$stop()

  expect_equal(compare_io(vals$input, vals$export$`test-species_data`),
               vals$export$`test-species_data`)
})
