test_files <- function(which) {
  list(
    files = list(`0` = list("", fs::path(which))),
    root = "wd")
}

test_dir <- function(which) {
  list(
    path = list("", fs::path(which)),
    root = "wd")
}


# Overrides testthat::is_testing()
is_testing <- function() {
  testthat::is_testing() | isTRUE(getOption("shiny.testmode"))
}

is_shiny_testing <- function() {
  isTRUE(getOption("shiny.testmode"))
}

compare_io <- function(i, o) {
  i <- stats::setNames(i, stringr::str_remove_all(names(i), "^[^-]+-"))
  as.data.frame(i[names(o)])
}


# Load test data

test_data <- function(file_dir = system.file("extdata", package = "ccviR"),
                      scn_nms = c("RCP 4.5", "RCP 8.5"),
                      clim_dir = "clim_files/processed",
                      assess_file = "assess_poly.shp",
                      range_file = "rng_poly.shp",
                      ptn_file = "PTN_poly.shp",
                      range_change_45_file = "rng_chg_45.tif",
                      range_change_85_file = "rng_chg_85.tif") {

  clim_vars <- get_clim_vars(file.path(file_dir, clim_dir), scn_nms)
  rng_chg_mat <- matrix(c(-1:1,NA, 1:3,0), ncol = 2)

  # make the crs's match to avoid warning it has to be verbatim the same
  # nonbreed <- st_read(file.path(file_dir, "nonbreed_poly.shp"), agr = "constant",
  #                     quiet = TRUE)
  assess <- sf::st_read(file.path(file_dir, assess_file), agr = "constant",
                        quiet = TRUE)
  range <- sf::st_read(file.path(file_dir, range_file), agr = "constant",
                          quiet = TRUE)
  ptn <- sf::st_read(file.path(file_dir, ptn_file), agr = "constant",
                     quiet = TRUE)

  # HS
  hs <- raster::raster(file.path(file_dir, range_change_45_file))
  hs_terra <- terra::rast(file.path(file_dir, range_change_45_file))

  # hs2 less CC in same area
  #hs1 <- raster::raster(file.path(file_dir, range_change_45_file))
  hs2 <- raster::raster(file.path(file_dir, range_change_85_file))

  range_points <- range %>% sf::st_make_grid(what = "centers")

  range_clim <- st_intersection(range, clim_vars$clim_poly) %>%
    st_set_agr("constant")
  range_clim <- valid_or_error(range_clim, "range clim intersection")

  list(clim_vars = clim_vars,
       rng_chg_mat = rng_chg_mat,
       assess = assess,
       range = range,
       ptn = ptn,
       hs = hs,
       hs_terra = hs_terra,
       hs2 = hs2,
       range_points = range_points,
       range_clim = range_clim,
       scn_nms = scn_nms)
}
