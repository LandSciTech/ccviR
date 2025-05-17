test_that("have_chrome()", {

  withr::with_options(
    list("ccviR.test_no_chrome" = TRUE),
    expect_equal(have_chrome(), "Cannot find Chromium or Google Chrome")
  )

  withr::with_options(
    list("ccviR.test_no_chrome_platform" = TRUE),
    expect_equal(have_chrome(), "Your platform is not supported")
  )

  expect_true(have_chrome())

})

test_that("check_chrome()", {

  withr::with_options(
    list("ccviR.test_no_chrome" = TRUE), {
      expect_error(check_chrome(), "required to build reports")
    }
  )

  withr::with_options(
    list("ccviR.test_no_chrome_platform" = TRUE), {
      expect_error(check_chrome(), "Unfortunately your platform is not supported")
    }
  )

  expect_null(check_chrome())

})

test_that("build_report()", {

  skip_on_ci()

  withr::with_file(
    r <- test_path("test_report.pdf"), {

      expect_false(fs::file_exists(r))

      build_report(test_df_loaded(), file_loc = r) %>%
        expect_message("Preparing report template") %>%
        expect_message("Rendering HTML report") %>%
        expect_message("Converting to PDF") %>%
        expect_message("Done!") %>%
        capture.output()

      expect_true(fs::file_exists(r))

    })
})

