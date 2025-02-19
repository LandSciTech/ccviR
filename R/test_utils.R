
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


#' Load test data
#'
#' Helper function for keeping track of local test data. Paths for files stored
#' in `misc` are returned with reference to the test/testthat directory so that
#' it can be used for unit testing as well as interactive testing.
#'
#' @param dir Character. Directory containing test data
#' @param scn_nms Character. Scenario names
#' @param clim_dir Character. Directory containing climate data
#' @param assess_poly_pth Character. Assessment area polygon shape file.
#' @param rng_poly_pth Character. Range polygon shape file.
#' @param ptn_poly_pth Character. PTN polygon shape file.
#' @param rng_chg_pths Character. Range change polygon shape files (one for each
#'   scenario).
#' @param saved_final Character. Previously saved Shiny App output.
#' @param saved_empty Character. Previously saved Shiny App output.
#' @param mock Logical. Whether or not paths should be mocked to resemble that
#'   returned by shinyFiles (i.e. relative and in a list format). See
#'   `mock_files()`.
#'
#' @noRd
#' @examples
#' test_files()
#' test_files(mock = TRUE)$saved$final

test_files <- function(dir = fs::path_package("extdata", package = "ccviR"),
                       scn_nms = c("RCP 4.5", "RCP 8.5"),
                       clim_dir = "clim_files/processed",
                       assess_poly_pth = "assess_poly.shp",
                       rng_poly_pth = "rng_poly.shp",
                       ptn_poly_pth = "PTN_poly.shp",
                       rng_chg_pth_1 = "rng_chg_45.tif",
                       rng_chg_pth_2 = "rng_chg_85.tif",
                       protected_rast_pth = "pa_north_america.tif",
                       saved = "test_files",
                       mock = FALSE) {

  # Fetch all previously saved test files
  saved <- fs::path(dir, saved) %>%
    fs::dir_ls()
  if(mock) saved <- fs::path_rel(saved, dir)
  saved <- saved %>%
    stats::setNames(nm = stringr::str_extract(., "(?<=test_)[^/]+(?=\\.csv)")) %>%
    as.list()

  # Fetch spatial data files
  f <- list(clim_dir = clim_dir,
            assess_poly_pth = assess_poly_pth,
            rng_poly_pth = rng_poly_pth,
            ptn_poly_pth = ptn_poly_pth,
            rng_chg_pth_1 = rng_chg_pth_1,
            rng_chg_pth_2 = rng_chg_pth_2)

  if(!mock) {
    # Demo files
    f <- purrr::map(f, ~fs::path(dir,  .x))

    # Big files stored in misc
    f <- c(f, list(protected_rast_pth = testthat::test_path(
      "../../misc/protected_areas", "pa_north_america.tif")))
  }

  # Only mock the reloadable files
  if(mock) {
    saved <- purrr::map(saved, mock_files)
  }

  # Add all together
  c(f, list("scn_nms" = scn_nms), list("saved" = saved))
}


test_data <- function(f = test_files()) {

  clim_vars <- get_clim_vars(f$clim_dir, f$scn_nms)
  rng_chg_mat <- matrix(c(-1:1, NA, 1:3,0), ncol = 2)

  # make the crs's match to avoid warning it has to be verbatim the same
  # nonbreed <- st_read(file.path(file_dir, "nonbreed_poly.shp"), agr = "constant",
  #                     quiet = TRUE)
  assess <- sf::st_read(f$assess_poly_pth, agr = "constant", quiet = TRUE)
  range <- sf::st_read(f$rng_poly_pth, agr = "constant", quiet = TRUE)
  ptn <- sf::st_read(f$ptn_poly_pth, agr = "constant", quiet = TRUE)

  # HS
  #hs <- raster::raster(f$rng_chg_pth_1)
  hs <- terra::rast(f$rng_chg_pth_1)

  # hs2 less CC in same area
  #hs1 <- raster::raster(f$rng_chg_pths[1])
  hs2 <- terra::rast(f$rng_chg_pth_2)

  # Protected Areas
  if(fs::file_exists(f$protected_rast_pth)) {
    protected_rast <- terra::rast(f$protected_rast_pth)
  } else protected_rast <- NULL

  range_points <- range %>% sf::st_make_grid(what = "centers")

  range_clim <- st_intersection(range, clim_vars$clim_poly) %>%
    st_set_agr("constant")
  range_clim <- valid_or_error(range_clim, "range clim intersection")

  list(clim_vars = clim_vars,
       rng_chg_mat = rng_chg_mat,
       assess_poly = assess,
       rng_poly = range,
       ptn_poly = ptn,
       rng_chg_rast_1 = hs,
       rng_chg_rast_2 = hs2,
       rng_chg_rast = c(hs, hs2),
       protected_rast = protected_rast,
       rng_pnts = range_points,  # For error checking
       rng_clim = range_clim,
       scn_nms = f$scn_nms)
}

mock_files <- function(file) {
  list(
    files = list(`0` = list("", fs::path(file))),
    root = "wd")
}

