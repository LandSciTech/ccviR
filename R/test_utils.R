
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




mock_files <- function(file) {
  list(
    files = list(`0` = list("", fs::path(file))),
    root = "wd")
}

expect_no_log_warnings <- function(app) {
  l <- as.character(app$get_logs())
  testthat::expect_false(any(stringr::str_detect(l, "Warning\\: ")))
}


expect_screenshot_local <- function(app, name = NULL) {
  testthat::skip_on_ci()
  app$expect_screenshot(name = name)
}

#' Load test data file paths
#'
#' Helper function for loading the file paths local test data.
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
#' @param min_req Logical. Use only minimum required for spatial analysis.
#'
#' @noRd
#' @examples
#' test_files()
#' test_files(mock = TRUE)$saved$final
#' test_files(min_req = TRUE)

test_files <- function(dir = fs::path_package("extdata", package = "ccviR"),
                       scn_nms = c("RCP 4.5", "RCP 8.5"),
                       clim_dir = "clim_files/processed",
                       assess_poly_pth = "assess_poly.shp",
                       rng_poly_pth = "rng_poly.shp",
                       ptn_poly_pth = "PTN_poly.shp",
                       rng_chg_pth_1 = "rng_chg_45.tif",
                       rng_chg_pth_2 = "rng_chg_85.tif",
                       protected_poly_pth = "pa_north_america.gpkg",
                       saved = "test_files",
                       mock = FALSE, paths_only = FALSE, min_req = FALSE) {

  # Fetch all previously saved test files
  saved <- fs::path(dir, saved) %>%
    fs::dir_ls()
  if(mock) saved <- fs::path_rel(saved, dir)
  saved <- saved %>%
    stats::setNames(nm = stringr::str_remove_all(fs::path_file(.), "(test_)|(\\.csv)")) %>%
    as.list()

  # Fetch spatial data files
  f <- list(clim_dir = clim_dir,
            assess_poly_pth = assess_poly_pth,
            rng_poly_pth = rng_poly_pth,
            ptn_poly_pth = if(!min_req) ptn_poly_pth,
            rng_chg_pth_1 = if(!min_req) rng_chg_pth_1,
            rng_chg_pth_2 = if(!min_req) rng_chg_pth_2
  )

  if(!mock) {
    # Demo files
    f <- purrr::map(f, ~if(!is.null(.x)) fs::path(dir,  .x))

    # Big files stored in misc
    f <- c(f, list(protected_poly_pth = if(!min_req) fs::path(dir,
                                                              "../../misc/protected_areas",
                                                              protected_poly_pth)))
  }

  # Only mock the reloadable files
  if(mock) {
    saved <- purrr::map(saved, mock_files)
  }

  # Add all together
  if(paths_only) {
    return(f)
  } else {
    return(c(f, list("scn_nms" = scn_nms), list("saved" = saved)))
  }
}

#' Load test data
#'
#' Helper function for loading and prepare local test data. Loads a list of
#' files, returned by `test_files()` by default. Names are expected to match
#' standard file input names (this function isn't very flexible from that
#' respect).
#'
#' @param f Named character vector. Files to load (including one directory,
#'   `clim_dir`).
#' @param min_req Logical. Use only minimum required for spatial analysis.
#'
#' @noRd
#' @examples
#' test_data()

test_data <- function(f = test_files(), min_req = FALSE) {

  clim_readme <- utils::read.csv(fs::path(f$clim_dir, "climate_data_readme.csv"))
  clim_vars <- get_clim_vars(f$clim_dir, f$scn_nms)
  rng_chg_mat <- if(!min_req) matrix(c(-1:1, NA, 1:3,0), ncol = 2)

  # make the crs's match to avoid warning it has to be verbatim the same
  # nonbreed <- st_read(file.path(file_dir, "nonbreed_poly.shp"), agr = "constant",
  #                     quiet = TRUE)
  assess <- sf::st_read(f$assess_poly_pth, agr = "constant", quiet = TRUE)
  range <- sf::st_read(f$rng_poly_pth, agr = "constant", quiet = TRUE)

  ptn <- if(!min_req && !is.null(f$ptn_poly_pth) && fs::file_exists(f$ptn_poly_pth)) {
    sf::st_read(f$ptn_poly_pth, agr = "constant", quiet = TRUE)
  }

  # HS
  hs <- if(!min_req && !is.null(f$rng_chg_pth_1) && fs::file_exists(f$rng_chg_pth_1)) terra::rast(f$rng_chg_pth_1)
  hs2 <- if(!min_req && !is.null(f$rng_chg_pth_2) && fs::file_exists(f$rng_chg_pth_2)) terra::rast(f$rng_chg_pth_2)

  # Protected Areas
  protected_poly <- if(!min_req && !is.null(f$protected_poly_pth) && fs::file_exists(f$protected_poly_pth)) {
    sf::st_read(f$protected_poly_pth, agr = "constant", quiet = TRUE)
  }

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
       protected_poly = protected_poly,
       rng_pnts = range_points,  # For error checking
       rng_clim = range_clim,
       scn_nms = f$scn_nms)
}


#' Load saved data for testing in Shiny modules
#'
#' Fixes the absolute paths stored in the saved data to use the current
#' working directly.
#'
#' - These paths need to be absolute for the reports (because executing in temp)
#' - But we want these paths to work on all systems
#'
#' @param file Character. Name of the saved previous results file to get the
#'   questions from. See `test_files()`.
#'
#' @returns Data frame to mimic `df_loaded`.
#' @noRd
#'
#' @examples
#' test_df_loaded()
test_df_loaded <- function(file = "final2") {
  load_previous(test_files()$saved[[file]]) %>%
    mutate(across(contains("pth"), fix_path))
}


#' Load test data file paths for climatic data prep
#'
#' Helper function for loading the file paths local climatic test data.
#'
#' @param dir Character. File path to the where the `clim_files/raw` folders can
#'   be found.
#'
#' @noRd
#' @examples
#' test_files_prep()

test_files_prep <- function(dir = fs::path_package("extdata", package = "ccviR")) {

  dir_clim <- fs::path(dir, "clim_files", "raw")

  # Inputs
  list(

    # Hist inputs
    clim_norm_period = "1961-1990",
    clim_norm_url = "https://adaptwest.databasin.org/pages/adaptwest-climatena-cmip5/",

    # Hist/standard File/dir paths
    mat_norm_pth = fs::path(dir_clim, "NB_norm_MAT.tif"),
    cmd_norm_pth = fs::path(dir_clim, "NB_norm_CMD.tif"),
    map_norm_pth = fs::path(dir_clim, "NB_norm_MAP.tif"),
    mwmt_norm_pth = fs::path(dir_clim, "NB_norm_MWMT.tif"),
    mcmt_norm_pth = fs::path(dir_clim, "NB_norm_MCMT.tif"),
    assess_pth = fs::path(dir, "assess_poly.shp"),
    ccei_pth1 =  fs::path(dir, "../../misc/ccei", "ccei_ssp245.tif"),
    ccei_pth2 =  fs::path(dir, "../../misc/ccei", "ccei_ssp585.tif"),

    # Scenario inputs
    clim_scn_nm = c("RCP 4.5", "RCP 8.5"),
    clim_scn_gcm = c("AdaptWest 15 CMIP5 AOGCM Ensemble",
                     "AdaptWest 15 CMIP5 AOGCM Ensemble"),
    clim_scn_period = c("2050s", "2050s"),
    clim_scn_em = c("RCP 4.5", "RCP 8.5"),
    clim_scn_url = c("https://adaptwest.databasin.org/pages/adaptwest-climatena-cmip5/",
                     "https://adaptwest.databasin.org/pages/adaptwest-climatena-cmip5/"),

    # Scenario paths
    mat_fut_pth = fs::path(dir_clim, c("NB_RCP.4.5_MAT.tif", "NB_RCP.8.5_MAT.tif")),
    cmd_fut_pth = fs::path(dir_clim, c("NB_RCP.4.5_CMD.tif", "NB_RCP.8.5_CMD.tif"))
  ) %>%
    unlist() # To get name1, name2, etc. for multiple scenario information
}


fix_path <- function(p) {

  # Reframe the paths as absolute paths but relative to the current wd
  # This is assumed to be the location of ccviR because we ONLY use this during
  # testing or examples within the package.

  # We need absolute paths for testing with reports as they need to be accessible
  # even when rendering reports in temp locations.
  if(all(!is.na(p))) {
    fs::path(
      fs::path_wd(),
      fs::path_rel(p, start = "ccviR/.."))
  } else NA
}


#' Create test species data for use in Results Shiny module
#'
#' Doesn't need to return a reactive as it is simple to use `reactive(x)` in
#' the test module itself.
#'
#' @param file Character. Name of the saved previous results file to get the
#'   questions from. See `test_files()`.
#'
#' @returns Data frame of values emulating the `species_data` returned by
#' `mod_species_server()`.
#'
#' @noRd
#'
#' @examples
#' test_species()
test_species <- function(file = "final2") {
  load_previous(test_files()$saved[[file]]) %>%
    select("species_name", "common_name", "assessor_name", "geo_location",
           "tax_grp", "mig", "cave") %>%
    distinct()
}

#' Create test questions for use in Results Shiny module
#'
#' Imitates the behaviour of `ccvir_app2()` which takes the reactive questions
#' returned by each module and sends a list of question reactives to the final
#' results module.
#'
#' @param file Character. Name of the saved previous results file to get the
#'   questions from. See `test_files()`.
#' @param as_reactive Logical. Return as list of reactive values to emulate the
#'   input argument `questions` for `mod_results_server()`.
#'
#' @returns List of (optionally) Shiny reactive lists with questions, comments
#'   and evidence.
#' @noRd
#'
#' @examples
#' test_questions(as_reactive = FALSE)
#' test_questions() # Reactive values
#' isolate(test_questions()$b()) # Look at reactive values

test_questions <- function(file = "final2", as_reactive = TRUE) {

  q <- load_previous(test_files()$saved[[file]]) %>%
    select(matches("^(com_|evi_)?(B|C|D)\\d+")) %>%
    distinct() %>%
    rename_with(.cols = matches("^(B|C|D)\\d+"), ~paste0("que_", .x)) %>%
    tidyr::pivot_longer(
      everything(), names_to = c("type", "Code"),
      names_pattern = "(com|evi|que)_(.+)",
      values_to = "value",
      values_transform = as.character)

  qs <- filter(q, .data$type == "que") %>%
    select("Code", "value") %>%
    tidyr::separate("value", into = c("Value1", "Value2", "Value3", "Value4"),
                    fill = "right", sep = ", ?", convert = TRUE) %>%
    split(tolower(stringr::str_extract(.$Code, "^\\w")))
  coms <- filter(q, .data$type == "com") %>%
    select("Code", "com" = "value") %>%
    split(tolower(stringr::str_extract(.$Code, "^\\w")))
  evi <- filter(q, .data$type == "evi") %>%
    select("Code", "evi" = "value") %>%
    split(f = tolower(stringr::str_extract(.$Code, "^\\w")))

  # If as_reactive == TRUE, return `reactive(x)`, else return `x`
  if(as_reactive) trans <- reactive else trans <- function(x) x

  purrr::pmap(
    list(qs, coms, evi),
    ~ trans(list("questions" = ..1, "comments" = ..2, "evidence" = ..3)))
}

#' Create Shiny-ready test spatial data
#'
#' Specifically for testing modules
#'
#' @param d List of test data output from `test_data()`.
#' @param min_req Logical. Use only minimum required for spatial analysis.
#' @param as_reactive Logical. Return as reactive values to emulate modules.
#'
#' @returns List of (optionally) Shiny reactives.
#' @noRd
#'
#' @examples
#' sp <- test_spatial()
#' isolate(sp$spat_res())
#' isolate(sp$protected_poly())
#'
#' # Only include minimum required spatial files
#' sp_min <- test_spatial(min_req = TRUE)
#' isolate(sp_min$spat_res())
#' isolate(sp_min$protected_poly())
#'
#' # Not for shiny
#' sp <- test_spatial(as_reactive = FALSE)
#' sp$spat_res

test_spatial <- function(d = test_data(), d_paths = test_files(),
                         min_req = FALSE, as_reactive = TRUE, quiet = TRUE) {
  # To run interactively
  if(shiny::isRunning() & !quiet) {
    with_prog <- withProgress
  } else with_prog <- function(x) x

  if(min_req) {
    d$rng_chg_rast <- NULL
    d$protected_poly <- NULL
    d$ptn_poly <- NULL
    d$rng_chg_mat <- NULL
  }

  with_prog({
    spat_res <- analyze_spatial(
      range_poly = d$rng_poly,
      non_breed_poly = NULL,
      scale_poly = d$assess_poly,
      hs_rast = d$rng_chg_rast,
      ptn_poly = d$ptn_poly,
      clim_vars_lst = d$clim_vars,
      hs_rcl = d$rng_chg_mat,
      protected_poly = d$protected_poly,
      scenario_names = d$clim_readme$Scenario_Name,
      quiet = quiet)
  })

  spat_tbl <- apply_spat_tholds(spat_res$spat_table, cave = FALSE)
  pths <- d_paths[stringr::str_subset(names(d_paths), "(dir|pth)")] %>%
    purrr::discard(is.null) %>%
    as.data.frame()

  spat_run <- data.frame(
    GCM_or_Ensemble_name = "Testing",
    Historical_normal_period = "1961-1990",
    Future_period = "2050s",
    Emissions_scenario = c("RCP 4.5", "RCP8.5"),
    link = "https://test.org",
    gain_mod = 1,
    gain_mod_comm = "",
    lost = "-1, -1",
    main = "0, 0",
    gain = "1, 1",
    ns = "99, 99",
    rng_chg_used = "multiple") %>%
    bind_cols(pths) %>%
    rename("clim_dir_pth" = "clim_dir")

  # If as_reactive == TRUE, return `reactive(x)`, else return `x`
  if(as_reactive) trans <- reactive else trans <- function(x) x

  list(
    "spat_res" = trans(spat_tbl),
    "spat_run" = trans(spat_run),
    "clim_vars" = trans(d$clim_vars),
    "clim_readme" = trans(d$clim_readme),
    "range_poly" = trans(spat_res$range_poly_assess),
    "range_poly_clim" = trans(spat_res$range_poly_clim),
    "ptn_poly" = trans(d$ptn_poly),
    "nonbreed_poly" = trans(NULL),
    "assess_poly" = trans(d$assess_poly),
    "protected_poly" = trans(spat_res$protected_poly),
    "hs_rast" = trans(d$rng_chg_rast),
    "hs_rcl_mat" = trans(d$rng_chg_mat)
  )
}


test_exp_tbl <- function(exp_lev) {

  if(exp_lev == 1){
    lev6 <- rev(c(5, 10, 10, 25, 25, 25))
  }
  if(exp_lev == 2){
    lev6 <- rev(c(10, 12, 30, 25, 10, 5))
  }
  if(exp_lev == 3){
    lev6 <- c(5, 10, 10, 25, 25, 25)
  }

  tribble(
    ~scenario_name, ~MAT_1, ~MAT_2, ~MAT_3, ~MAT_4, ~MAT_5, ~MAT_6, ~CMD_1, ~CMD_2, ~CMD_3, ~CMD_4, ~CMD_5, ~CMD_6, ~CCEI_1, ~CCEI_2, ~CCEI_3, ~CCEI_4, ~perc_non_breed_not_over_ccei, ~HTN_1, ~HTN_2, ~HTN_3, ~HTN_4, ~PTN, ~MAP_max, ~MAP_min, ~range_change, ~range_overlap, ~range_size, ~protected,
    "Scn1", lev6[1], lev6[2], lev6[3], lev6[4], lev6[5], lev6[6], lev6[1], lev6[2], lev6[3], lev6[4], lev6[5], lev6[6], 40.8,	31.0,	23.3, 4.9, 0,	NA,	NA,	NA,	NA,	0,	8073,	216, NA,	NA,	3.16572E+12, 4
  )
}

#' Simplified CMD and Tmean calculations
#'
#' For comparing with ccei_values()
#'
#' @param r Data.frame of rasters created with `combine_XXX()`
#'
#' @returns vector with CMD and Tmean values for a single cell
#' @noRd

ccei_by_hand <- function(r, row = 1, col = 1) {
  # By-hand calculations for first cell
  r_prec <- terra::rast(r$file[r$var == "prec"])[row,col]
  r_tmax <- terra::rast(r$file[r$var == "tmax"])[row,col]
  r_tmin <- terra::rast(r$file[r$var == "tmin"])[row,col]
  lat <- stats::setNames(terra::init(terra::rast(r$file[1]), "y")[row,col],
                         "latitude")[[1]]

  cmd <- NULL
  tmean <- NULL
  for(m in 1:12) {
    eref <- climr:::calc_Eref(m,
                              tmmin = r_tmin[[m]],
                              tmmax = r_tmax[[m]],
                              latitude = lat)
    cmd <- c(cmd, climr:::calc_CMD(eref, r_prec[[m]]))
    tmean <- c(tmean, (r_tmax[[m]] + r_tmin[[m]]) / 2)
  }

  return(c("cmd" = sum(cmd), "tmean" = mean(tmean)))
}
