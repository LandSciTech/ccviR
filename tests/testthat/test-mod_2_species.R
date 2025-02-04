library(shinytest2)

test_that("Species Fills in previous data", {

  shiny_app <- mod_species_test()
  app <- AppDriver$new(shiny_app)

  #app$set_window_size(width = 1304, height = 718)
  app$expect_values(screenshot_args = FALSE)
  vals <- app$get_values()
  app$stop()

  expect_equal(compare_io(vals$input, vals$export$`test-species_data`),
               vals$export$`test-species_data`)
})
