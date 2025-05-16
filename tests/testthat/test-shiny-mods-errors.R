library(shinytest2)
# Remember to re-build/install package before running tests! Ctrl-Shift-B
#
# Consider running devtools::test(filter = "shiny-mods-errors") to test just this
# file in a 'clean' session. (Helps find out why getting warnings which are
# once per session, for example, and cannot be replicated by clicking Run Tests).

test_that("Full app - No errors", {

  shiny_app <- ccvi_app2(input_files = test_files())
  app <- AppDriver$new(shiny_app, variant = platform_variant(r_version = FALSE))
  app$set_window_size(width = 1619, height = 993)

  # Home
  app$click("home-continue")
  app$click("species-continue")
  app$set_inputs(`spatial-rng_chg_used` = "multiple") # Will print warning to console for easier debugging
  app$click("spatial-continue")

  # Section A
  expect_no_log_warnings(app) # app$get_logs()
  cat(stringr::str_subset(as.character(app$get_logs()), "Warning"))
  app$click("section_a-continue")

  # Section B
  expect_no_log_warnings(app)
  app$click("section_b-continue")

  # Section C
  expect_no_log_warnings(app)
  app$click("section_c-continue")

  # Section D
  expect_no_log_warnings(app)
  app$click("section_d-continue")

  # Results
  expect_no_log_warnings(app)

  #readr::write_csv(app$get_logs(), paste0("~/Desktop/shiny_logs1_", Sys.Date(), ".csv"))

  app$stop()
})

# Remember to re-build/install package before running tests! Ctrl-Shift-B
test_that("Full app - No errors with missing spatial", {
  skip_on_ci()

  f <- test_files(protected_poly_pth = NA,
                  ptn_poly_pth = NA,
                  rng_chg_pth_1 = NA,
                  rng_chg_pth_2 = NA)

  shiny_app <- ccvi_app2(input_files = f)
  app <- shinytest2::AppDriver$new(shiny_app, variant = shinytest2::platform_variant(r_version = FALSE))
  app$set_window_size(width = 1619, height = 993)

  app$click("home-continue")
  app$click("species-continue")
  app$click("spatial-continue")

  # Section A
  expect_no_log_warnings(app)  # app$get_logs()
  app$click("section_a-continue")

  # Section B
  expect_no_log_warnings(app)
  app$click("section_b-continue")

  # Section C
  expect_no_log_warnings(app)
  app$click("section_c-continue")

  # Section D
  expect_no_log_warnings(app)
  app$click("section_d-continue")

  # Results
  expect_no_log_warnings(app)

  #readr::write_csv(app$get_logs(), paste0("~/Desktop/shiny_logs2_", Sys.Date(), ".csv"))

  app$stop()
})
