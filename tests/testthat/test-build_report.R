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
      expect_silent(m <- check_chrome())
      expect_s3_class(m, "shiny.tag")
      expect_true(stringr::str_detect(as.character(m), "Problem locating"))
      expect_true(stringr::str_detect(as.character(m), "modal-body"))
    }
  )

  withr::with_options(
    list("ccviR.test_no_chrome_platform" = TRUE), {
      expect_silent(m <- check_chrome())
      expect_s3_class(m, "shiny.tag")
      expect_true(stringr::str_detect(as.character(m), "Unfortunately your platform"))
      expect_true(stringr::str_detect(as.character(m), "modal-body"))
    }
  )

  expect_true(check_chrome())

})

test_that("build_report()", {

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

