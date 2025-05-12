test_that("check_polys()", {

  # Standard usage
  p0 <- sf::st_read(test_files()$rng_poly_pth, quiet = TRUE)
  expect_silent(check_polys(NULL))
  expect_silent(check_polys(p0))

  # Geometries
  p1 <- sf::st_read(fs::path_package("ccviR", "extdata", "error_files", "rng_point.shp"), quiet = TRUE)
  expect_error(check_polys(p1), "has geometry type POINT but only")
  p1 <- rbind(select(p0, geometry), select(p1, geometry))
  expect_message(check_polys(p1), "POINT or LINE geometries in polygon were dropped")

  # Check for Z/M removal
  f <- test_path("..", "misc", "external_test_files", "ccvi_shp", "ecozone_max_boundary.shp")
  skip_if_not(fs::file_exists(f))
  p2 <- sf::st_read(f, quiet = TRUE)

  expect_message(p3 <- check_polys(p2), "Removing Z and/or M dimensions")
  expect_silent(check_polys(p3))
})
