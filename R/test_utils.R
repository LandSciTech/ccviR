
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
    f <- c(f, list(protected_rast_pth = fs::path(dir,
                                                 "../../misc/protected_areas",
                                                 protected_rast_pth)))
  }

  # Only mock the reloadable files
  if(mock) {
    saved <- purrr::map(saved, mock_files)
  }

  # Add all together
  c(f, list("scn_nms" = scn_nms), list("saved" = saved))
}


test_data <- function(f = test_files()) {

  clim_readme <- read.csv(fs::path(f$clim_dir, "climate_data_readme.csv"))
  clim_vars <- get_clim_vars(f$clim_dir, f$scn_nms)
  rng_chg_mat <- matrix(c(-1:1, NA, 1:3,0), ncol = 2)

  # make the crs's match to avoid warning it has to be verbatim the same
  # nonbreed <- st_read(file.path(file_dir, "nonbreed_poly.shp"), agr = "constant",
  #                     quiet = TRUE)
  assess <- sf::st_read(f$assess_poly_pth, agr = "constant", quiet = TRUE)
  range <- sf::st_read(f$rng_poly_pth, agr = "constant", quiet = TRUE)

  ptn <- if(fs::file_exists(f$ptn_poly_pth)) {
    sf::st_read(f$ptn_poly_pth, agr = "constant", quiet = TRUE)
  } else NULL

  # HS
  hs <- if(fs::file_exists(f$rng_chg_pth_1)) terra::rast(f$rng_chg_pth_1) else NULL
  hs2 <- if(fs::file_exists(f$rng_chg_pth_2)) terra::rast(f$rng_chg_pth_2) else NULL

  # Protected Areas
  protected_rast <- if(fs::file_exists(f$protected_rast_pth)) {
    terra::rast(f$protected_rast_pth)
  } else NULL

  range_points <- range %>% sf::st_make_grid(what = "centers")

  range_clim <- st_intersection(range, clim_vars$clim_poly) %>%
    st_set_agr("constant")
  range_clim <- valid_or_error(range_clim, "range clim intersection")

  list(clim_vars = clim_vars,
       clim_readme = clim_readme,
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

#' Create Shiny-ready test spatial data
#'
#' Specifically for testing modules
#'
#' @param d List of test data output from `test_data()`.
#'
#' @returns List of Shiny reactives.
#' @noRd
#'
#' @examples
#' sp <- test_spatial()
#' isolate(sp$spat_res())

test_spatial <- function(d = test_data()) {
  # To run interactively
  if(shiny::isRunning()) with_p <- withProgress else with_p <- function(x) x
  with_p({
    spat_res <- analyze_spatial(range_poly = d$rng_poly,
                                non_breed_poly = NULL,
                                scale_poly = d$assess_poly,
                                hs_rast = d$rng_chg_rast,
                                ptn_poly = d$ptn_poly,
                                clim_vars_lst = d$clim_vars,
                                hs_rcl = d$rng_chg_mat,
                                protected_rast = d$protected_rast,
                                scenario_names = d$clim_readme$Scenario_Name)
  })

  spat_tbl <- apply_spat_tholds(spat_res$spat_table, cave = FALSE)

  list(
    "spat_res" = reactive(spat_tbl),
    "clim_vars" = reactive(d$clim_vars),
    "clim_readme" = reactive(d$clim_readme),
    "range_poly" = reactive(spat_res$range_poly_assess),
    "range_poly_clim" = reactive(spat_res$range_poly_clim),
    "ptn_poly" = reactive(d$ptn_poly),
    "nonbreed_poly" = reactive(NULL),
    "assess_poly" = reactive(d$assess_poly),
    "protected_rast_assess" = reactive(spat_res$protected_rast_assess),
    "hs_rast" = reactive(d$rng_chg_rast),
    "hs_rcl_mat" = reactive(d$rng_chg_mat)
  )
}



mock_files <- function(file) {
  list(
    files = list(`0` = list("", fs::path(file))),
    root = "wd")
}

expect_no_log_warnings <- function(app) {
  l <- as.character(app$get_logs())
  expect_false(any(stringr::str_detect(l, "Warning\\: ")))
}
