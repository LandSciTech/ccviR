library(shinytest2)
library(ccviR)

# call for snapshot review
# testthat::snapshot_review('run_ccvi_app/', path = 'tests/testthat/app/')

Sys.setenv(
  CHROMOTE_CHROME = "C:/Users/EndicottS/AppData/Local/Google/Chrome/Application/chrome.exe"
)

# Note seems you need to shut Chrome for it to work

test_that("{shinytest2} app test", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  skip_on_ci()
  skip("Skipping automated app tests")

  app_obj <- run_ccvi_app("demo", test.mode = TRUE)
  app <- AppDriver$new(app_obj, name = "ccviR", seed = 7)
  # app <- AppDriver$new("tests/testthat/app", name = "ccviR", seed = 7)

  # Update output value
  app$click("start")
  app$set_inputs(assessor_name = "Sarah",
                 geo_location = "Canada",
                 species_name = "Bubo scandiacus",
                 common_name = "Snowy Owl",
                 tax_grp = "Bird")
  app$click("next1")
  app$click("startSpatial")

  expect_equal(app$get_value(output = "spat_error"), "Spatial analysis complete")

  app$click("next2")

  # set all check boxes other than spatial vuln to 1
  non_spat_qs <- vulnq_code_lu_tbl %>% dplyr::filter(is_spatial == 0) %>%
    dplyr::pull(Code)

  inps_to_set <- lapply(seq_along(non_spat_qs), function(x) rep("1")) %>%
    setNames(non_spat_qs)

  # set a few to not one or multiple to make report more realistic
  inps_to_set$B1 <- c("2", "3")
  inps_to_set$B2a <- "0"
  inps_to_set$B3 <- NULL

  do.call(app$set_inputs, inps_to_set)

  # add long comments for all
  com_labels <- paste0("com", vulnq_code_lu_tbl$Code)
  com_to_set <- lapply(seq_along(com_labels), function(x) stringi::stri_rand_lipsum(1)) %>%
    setNames(com_labels)

  do.call(app$set_inputs, com_to_set)

  app$click("next4")

  spat_qs <- vulnq_code_lu_tbl %>% dplyr::filter(is_spatial == 1) %>%
    dplyr::pull(Code)

  spat_vals <- app$get_values(input = spat_qs)

  spat_vals_null <- lapply(spat_vals$input, function(x)(is.null(x)))

  expect_false(any(unlist(spat_vals_null)),
              label = "Spatial Vuln Qs are calculated")

  app$click("next5", wait_ = FALSE)
  app$click("calcIndex", wait_ = FALSE)

  out_data <- app$get_values(export = TRUE)$export$out_data

  out_data <- ccviR:::combine_outdata(out_data)

  expect_true(all(out_data$CCVI_index == "EV"))

  # delay to allow rendering
  app$wait_for_idle(duration = 500, timeout = 30*1000)

  path <- app$get_download("report")

  expect_snapshot_file(path, "rmd_report", variant = app$get_variant())

  # to see file in interactive session:
  # system(paste0('open "', path, '"'))

  # not needed for testing but useful for interactive report rendering
  if(FALSE){
    # check report works
    base_pth <- system.file("extdata", package = "ccviR")

    # scenario names
    scn_nms <- c("RCP 4.5", "RCP 8.5")

    clim_vars <- get_clim_vars(file.path(base_pth, "clim_files/processed"),
                               scenario_names = scn_nms)

    range_poly <- sf::read_sf(file.path(base_pth, "rng_poly.shp"), agr = "constant")
    scale_poly <- sf::read_sf(file.path(base_pth, "assess_poly.shp"), agr = "constant")

    rmarkdown::render(file.path(base_pth, "../rmd/results_report.rmd"),
                      params = list(out_data = out_data,
                                    clim_vars = clim_vars,
                                    scale_poly = scale_poly,
                                    range_poly = range_poly),
                      envir = new.env())
  }



})

test_that("save works especially for spatial and range change and all related inputs", {
  skip_on_cran()
  skip_on_ci()
  # stop("add test")
})
