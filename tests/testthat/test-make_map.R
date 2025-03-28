
# Test Data
expect_silent({
  d <- test_data()

  rng_chg_cls <- d$rng_chg_rast %>%
    terra::classify(rcl = d$rng_chg_mat, right = NA)
})


test_that("make_map2", {

  expect_message({
    map2 <- make_map2(d$rng_poly, d$clim_vars$mat[[1]], rast1_nm = "mat",
                   poly2 = d$assess_poly, poly2_nm = "assess_poly")
  }, "projecting raster")

  expect_message({
    map4 <- make_map2(
      d$rng_poly, rng_chg_cls, rast1_nm = "hs_rast",
      poly2 = d$assess_poly, poly2_nm = "assess_poly",
      rast1_lbl = data.frame(label = c("Not suitable", "Lost", "Maintained", "Gained"),
                            value = c(0, 1, 2, 3)))
  }, "projecting raster")

  expect_message({
    map1 <- make_map2(d$rng_poly, d$clim_vars$mat,
                     rast1_nm = "mat", poly2 = d$assess_poly,
                     poly2_nm = "assess_poly")
  }, "projecting raster")

  expect_silent({
    map3 <- make_map2(d$rng_poly, poly2 = d$assess_poly, poly2_nm = "assess_poly")
  })

  expect_message({
    map5 <- make_map2(d$rng_poly, d$clim_vars$map, rast1_nm = "map")
  }, "projecting raster")

  # Expect created
  purrr::map(list(map1, map2, map3, map4, map5),
             ~expect_s3_class(.x, "leaflet"))
})

# Visually inspect the maps
if(interactive()){

  map2
  map1
  map3
  map4
  map5

  ui <- fluidPage(
    leaflet::leafletOutput("map")
  )

  server <- function(input, output) {
    str(map2)
    output$map <- leaflet::renderLeaflet({
      map2
    })
  }

  shinyApp(ui, server)

}

if(FALSE){
  # test with real climate data
  # scenario names
  scn_nms <- c("RCP 4.5", "RCP 8.5")

  clim_vars2 <- get_clim_vars("../CCVI_analysis/data/CMIP5_ENSEMBLE_rcp45_rcp85_2050_NORM_6190/",
                             scn_nms)

  map2b <- make_map2(d$rng_poly, clim_vars2$mat[[1]], rast_nm = "mat", poly2 = d$assess_poly,
                   poly2_nm = "assess_poly")

  map3b <- make_map2(d$rng_poly, clim_vars2$mat, rast_nm = "mat", poly2 = d$assess_poly,
                    poly2_nm = "assess_poly")

}
