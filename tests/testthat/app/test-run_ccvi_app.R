library(shinytest2)

test_that("{shinytest2} recording: ccviR", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  app <- AppDriver$new(".", name = "ccviR", seed = 7)
  # app <- AppDriver$new("tests/testthat/app", name = "ccviR", seed = 7)

  # Update output value
  app$click("start")
  app$set_inputs(assessor_name = "Sarah",
                 geo_location = "Canada",
                 species_name = "Bubo scandiacus",
                 tax_grp = "Bird")
  app$expect_values()

  # app$click("next1")
  # app$click("clim_var_dir")
  # app$click("range_poly_pth")
  # app$click("assess_poly_pth")
  # app$click("startSpatial")
  # app$click("shinyalert")
  # app$click("shinyalert")
  # # Update output value
  # app$click("ptn_poly_pth")
  # # Update output value
  # # Update unbound `input` value
  # # Update output value
  # # Update unbound `input` value
  # # Update output value
  # app$click("nonbreed_poly_pth")
  # # Update output value
  # app$click("next2")
  # app$set_inputs(tabset = "Spatial Data Analysis")
  # app$click("startSpatial")
  # app$click("shinyalert")
  # app$click("shinyalert")
  # app$click("startSpatial")
  # app$click("shinyalert")
  # app$click("shinyalert")
  # app$click("next2")
  # app$set_inputs(tabset = "Spatial Data Analysis")

})
