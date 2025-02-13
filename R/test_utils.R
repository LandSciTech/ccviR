
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
#' Helper function for keeping track of local test data.
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

  if(!mock) f <- purrr::map(f, ~fs::path(dir,  .x))

  if(mock) {
    f <- purrr::map(f, mock_files)
    saved <- purrr::map(saved, mock_files)
  }

  # Add all together
  c(f, list("scn_nms" = scn_nms), list("saved" = saved))
}


test_data <- function(d = test_files()) {

  clim_vars <- get_clim_vars(d$clim_dir, d$scn_nms)
  rng_chg_mat <- matrix(c(-1:1, NA, 1:3,0), ncol = 2)

  # make the crs's match to avoid warning it has to be verbatim the same
  # nonbreed <- st_read(file.path(file_dir, "nonbreed_poly.shp"), agr = "constant",
  #                     quiet = TRUE)
  assess <- sf::st_read(d$assess_poly_pth, agr = "constant", quiet = TRUE)
  range <- sf::st_read(d$rng_poly_pth, agr = "constant", quiet = TRUE)
  ptn <- sf::st_read(d$ptn_poly_pth, agr = "constant", quiet = TRUE)

  # HS
  hs <- raster::raster(d$rng_chg_pth_1)
  hs_terra <- terra::rast(d$rng_chg_pth_2)

  # hs2 less CC in same area
  #hs1 <- raster::raster(d$rng_chg_pths[1])
  hs2 <- raster::raster(d$rng_chg_pth_2)

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
       scn_nms = d$scn_nms)
}

mock_files <- function(file) {
  list(
    files = list(`0` = list("", fs::path(file))),
    root = "wd")
}

