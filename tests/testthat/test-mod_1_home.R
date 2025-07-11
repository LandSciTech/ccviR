
test_that("server_setup works on CI",{
  skip_on_os("windows")
  expect_silent(volumes <- server_setup())
  expect_silent(parse_path(volumes, test_files(mock = TRUE)$saved$full_run))
})

test_that("server_setup works on CI",{
  skip_on_ci()
  expect_warning(volumes <- server_setup(), "VolumeName")
  expect_silent(parse_path(volumes, test_files(mock = TRUE)$saved$full_run))
})

test_that("load_previous()", {
  expect_silent(load_previous(test_files()$saved$full_run))
  expect_error(load_previous(""), "File doesn't exist")
  expect_error(load_previous(test_files()$saved$empty),
               "CSV file is empty, cannot restore from file.")
})

test_that("loads data", {

  volumes <- suppressWarnings(server_setup())
  testServer(mod_home_server, args = list(volumes = volumes), {
    r <- session$getReturned()

    session$setInputs(loadcsv = test_files(mock = TRUE)$saved$questions_only)

    expect_true(fs::file_exists(path()))

    # No index
    expect_s3_class(df_loaded(), "data.frame")
    expect_equal(df_loaded(), r$df_loaded())
    expect_true(all(is.na(df_loaded()$CCVI_index)))

    # Index
    session$setInputs(loadcsv = test_files(mock = TRUE)$saved$full_run)
    expect_s3_class(df_loaded(), "data.frame")
    expect_equal(df_loaded(), r$df_loaded())
    expect_false(all(is.na(df_loaded()$CCVI_index)))
  })
})
