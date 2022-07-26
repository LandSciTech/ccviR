base_pth <- system.file("extdata", package = "ccviR")

# scenario names
scn_nms <- c("RCP 4.5", "RCP 8.5")

clim_vars <- get_clim_vars(file.path(base_pth, "clim_files/processed"),
                           scenario_names = scn_nms)

range_poly <- sf::read_sf(file.path(base_pth, "rng_poly.shp"), agr = "constant")
scale_poly <- sf::read_sf(file.path(base_pth, "assess_poly.shp"), agr = "constant")

spat_res <- analyze_spatial(
  range_poly = range_poly,
  scale_poly = scale_poly,
  clim_vars_lst = clim_vars,
  hs_rast = raster::stack(raster::raster(file.path(base_pth, "rng_chg_45.tif")),
                          raster::raster(file.path(base_pth, "rng_chg_85.tif"))),
  hs_rcl = matrix(c(0:7, 0, 1, 2, 2 ,2, 2, 2, 3), ncol = 2),
  scenario_names = scn_nms
)

# vulnerability factor table with score 1 (somewhat increase vulnerability)
# for all factors
vuln <- make_vuln_df("test_species", val1 = 1, mig = 1)

index_res <- calc_vulnerability(spat_res$spat_table, vuln, "Bird")

test_that("paramerterized render works", {
  rmarkdown::render(file.path(base_pth, "../rmd/results_report.rmd"),
                    params = list(index_res = index_res,
                                  clim_vars = clim_vars,
                                  spat_res = spat_res,
                                  vuln_df = vuln,
                                  scale_poly = scale_poly,
                                  sp_name = "Test Species (Genus species"))
})
