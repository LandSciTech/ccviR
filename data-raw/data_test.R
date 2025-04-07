# Make a test data set for examples etc.


# vuln <- make_vuln_df("Testious sp.", val1 = 1, mig = 1)
# vuln$Value2[c(5, 7, 9)] <- 3
# vuln$evidence[c(5, 7, 9)] <- c("Literature", "Expert Opinion", "Other")
#
# usethis::use_data(vuln, overwrite = TRUE)

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
