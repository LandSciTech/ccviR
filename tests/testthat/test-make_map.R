
library("sf", quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
# load the demo data
file_dir <- system.file("extdata", package = "ccviR")

# scenario names
scn_nms <- c("RCP 4.5", "RCP 8.5")

clim_vars <- get_clim_vars(file.path(file_dir, "clim_files/processed"),
                           scn_nms)

assess <- st_read(file.path(file_dir, "assess_poly.shp"), agr = "constant",
                  quiet = TRUE)
rng_high <- st_read(file.path(file_dir, "rng_poly.shp"), agr = "constant",
                    quiet = TRUE)

hs <- terra::rast(file.path(file_dir, "rng_chg_45.tif"))

# hs2 less CC in same area
hs_stack <- terra::rast(c(file.path(file_dir, "rng_chg_45.tif"),
                  file.path(file_dir, "rng_chg_85.tif"))) %>%
  terra::classify(rcl = mat <- matrix(c(-1, -1, 1,
                                        0, 0, 2,
                                        1, 1, 3,
                                        4, 4, 0),
                                      byrow = TRUE, ncol = 3), right = NA)


map2 <- make_map(rng_high, clim_vars$mat[[1]], rast_nm = "mat", poly2 = assess,
                 poly2_nm = "assess_poly")

map4 <- make_map(rng_high, hs_stack, rast_nm = "hs_rast", poly2 = assess,
                 poly2_nm = "assess_poly",
                 rast_lbl = data.frame(label = c("Not suitable", "Lost", "Maintained", "Gained"),
                                       value = c(0, 1, 2, 3)))

map1 <- make_map(rng_high, clim_vars$mat, rast_nm = "mat", poly2 = assess,
                 poly2_nm = "assess_poly")

map3 <- make_map(rng_high, poly2 = assess,
                 poly2_nm = "assess_poly")

map5 <- make_map(rng_high, clim_vars$map, rast_nm = "map")

# Issue sent to tmap about these warnings https://github.com/r-tmap/tmap/issues/630
# group does not work at the moment
test_that("map gets made",
          {purrr::map(list(map1, map2, map3, map4, map5),
                      \(x)expect_s3_class(x, "leaflet"))})

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

  map2b <- make_map(rng_high, clim_vars2$mat[[1]], rast_nm = "mat", poly2 = assess,
                   poly2_nm = "assess_poly")

  map3b <- make_map(rng_high, clim_vars2$mat, rast_nm = "mat", poly2 = assess,
                    poly2_nm = "assess_poly")

}
