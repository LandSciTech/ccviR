library(shinytest2)

# Remember to re-build/install package before running tests! Ctrl-Shift-B

# devtools::test(filter = "mod_x_report") # Run just this test programatically

test_that("Report downloads", {

  # TODO: After Chrome update, shinytests2 fail, use chromium instead (temp?)
  Sys.setenv(CHROMOTE_CHROME = "chromium")

  shiny_app <- mod_report_test()
  app <- AppDriver$new(shiny_app, variant = platform_variant(r_version = FALSE))
  app$set_window_size(width = 1619, height = 993)

  app$wait_for_idle()
  Sys.sleep(1)
  app$expect_screenshot(name = "01-report-ready")

  # PDF will always be different, because of dates, but can test if created.
  r <- app$get_download("test-report")
  app$wait_for_idle(timeout = 60 * 1000) # Wait at least 1 min
  expect_equal(fs::path_file(r), paste0("common_test_", Sys.Date(), ".pdf"))
  expect_true(fs::file_exists(r))
  fs::file_delete(r)
  app$stop()
})

test_that("Chrome check", {

  # TODO: After Chrome update, shinytests2 fail, use chromium instead (temp?)
  Sys.setenv(CHROMOTE_CHROME = "chromium")

  shiny_app <- mod_report_test()

  # Use options by hand
  # - shinytest2 doesn't work with runApp()
  # - withr::with_options doesn't work with shinyApp()

  app <- AppDriver$new(shiny_app, variant = "ubuntu",
                       options = list("ccviR.test_no_chrome_platform" = TRUE))
  app$set_window_size(width = 1619, height = 993)
  app$expect_screenshot(name = "02-report-chrome-platform")
  app$stop()

  app <- AppDriver$new(shiny_app, variant = "ubuntu",
                       options = list("ccviR.test_no_chrome" = TRUE))
  app$set_window_size(width = 1619, height = 993)
  app$expect_screenshot(name = "03-report-chrome")
  app$stop()
})

