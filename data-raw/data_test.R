# Make a test data set for examples etc.

# Testing deliberate errors -------------------------------------------------
# Make Error test data for spatial analyses
pkg <- fs::path(fs::path_package("extdata", package = "ccviR"))
pkg_error <- fs::dir_create(pkg, "error_files")

d <- test_data()

# Make point data when should be polygons
d$range <- sf::st_make_grid(d$range, what = "centers")
sf::st_write(d$range, fs::path(pkg_error, "rng_point.shp"), append = FALSE)

# Make empty file with correct extension
write("", fs::path(pkg_error, "empty.shp"))

# Problematic climate data
fs::dir_copy(fs::path(pkg, "clim_files", "processed"),
             fs::path(pkg_error, "clim"))
fs::file_delete(fs::path(pkg_error, "clim", "clim_poly.shp"))
