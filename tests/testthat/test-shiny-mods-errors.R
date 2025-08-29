library(shinytest2)
# Remember to re-build/install package before running tests! Ctrl-Shift-B
#
# Consider running devtools::test(filter = "shiny-mods-errors") to test just this
# file in a 'clean' session. (Helps find out why getting warnings which are
# once per session, for example, and cannot be replicated by clicking Run Tests).

test_that("Full app - No errors", {
  skip_on_ci()
  skip_on_covr()
  # skip("skipping for now, not working but works interactively")

  shiny_app <- ccvi_app(input_files = test_files())
  app <- AppDriver$new(shiny_app, variant = platform_variant(r_version = FALSE))
  Sys.sleep(1)
  app$set_window_size(width = 1619, height = 993, wait = FALSE)
  Sys.sleep(1)

  # Home
  app$click("home-continue")
  Sys.sleep(1)
  app$click("species-continue")
  Sys.sleep(1)
  app$set_inputs(`spatial-rng_chg_used` = "multiple") # Will print warning to console for easier debugging
  Sys.sleep(1)
  app$click("spatial-continue")
  Sys.sleep(1)

  # Section A
  # Need to find a way to ignore warnings about Volume on VPN and package built under
  # expect_no_log_warnings(app) # app$get_logs()
  # cat(stringr::str_subset(as.character(app$get_logs()), "Warning"))
  app$click("section_a-continue")
  Sys.sleep(1)

  # Section B
  # expect_no_log_warnings(app)
  app$click("section_b-continue")
  Sys.sleep(1)

  # Section C
  # expect_no_log_warnings(app)
  app$click("section_c-continue")
  Sys.sleep(1)

  # Section D
  # expect_no_log_warnings(app)
  app$click("section_d-continue")
  Sys.sleep(1)

  # Results
  # expect_no_log_warnings(app)

  #readr::write_csv(app$get_logs(), paste0("~/Desktop/shiny_logs1_", Sys.Date(), ".csv"))

  app$stop()
  # made it this far with no error
  expect_true(TRUE)
})

# Remember to re-build/install package before running tests! Ctrl-Shift-B
test_that("Full app - No errors with missing spatial", {
  skip_on_ci()
  skip_on_covr()
  # skip("skipping for now, not working but works interactively")

  f <- test_files(min_req = TRUE)

  shiny_app <- ccvi_app(input_files = f)
  app <- shinytest2::AppDriver$new(shiny_app, variant = shinytest2::platform_variant(r_version = FALSE))
  app$set_window_size(width = 1619, height = 993, wait = FALSE)
  Sys.sleep(1)

  app$click("home-continue")
  Sys.sleep(1)
  app$click("species-continue")
  Sys.sleep(1)
  app$click("spatial-continue")
  Sys.sleep(1)

  # Section A
  # expect_no_log_warnings(app)  # app$get_logs()
  app$click("section_a-continue")
  Sys.sleep(1)

  # Section B
  # expect_no_log_warnings(app)
  app$click("section_b-continue")
  Sys.sleep(1)

  # Section C
  # expect_no_log_warnings(app)
  app$click("section_c-continue")

  # Section D
  # expect_no_log_warnings(app)
  app$click("section_d-continue")
  Sys.sleep(1)

  # Results
  # expect_no_log_warnings(app)

  #readr::write_csv(app$get_logs(), paste0("~/Desktop/shiny_logs2_", Sys.Date(), ".csv"))

  app$stop()
  # made it this far with no error
  expect_true(TRUE)
})
