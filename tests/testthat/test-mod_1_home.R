test_that("load_previous()", {

  # Parse path
  f <- expect_silent(parse_path(server_setup(), test_files("test_files/test_final.csv")))
  expect_true(fs::file_exists(f))

  # Load data
  expect_silent(load_previous(f))

  f <- expect_silent(parse_path(server_setup(), test_files("")))
  expect_error(load_previous(f), "File doesn't exist")

  f <- expect_silent(parse_path(server_setup(), test_files("test_files/test_empty.csv")))
  expect_error(load_previous(f), "CSV file is empty, cannot restore from file.")
})

test_that("loads data", {

  volumes <- server_setup()
  testServer(mod_home_server, args = list(volumes = volumes), {
    r <- session$getReturned()

    session$setInputs(loadcsv = test_files("test_files/test_final.csv"))

    expect_true(fs::file_exists(path()))

    expect_s3_class(df_loaded(), "data.frame")
    expect_equal(df_loaded(), r$df_loaded())
    expect_true(!all(is.na(df_loaded()$CCVI_index)))

    session$setInputs(loadcsv = test_files("test_files/test_spatial.csv"))
    expect_s3_class(df_loaded(), "data.frame")
    expect_equal(df_loaded(), r$df_loaded())
    expect_false(!all(is.na(df_loaded()$CCVI_index)))
  })
})
