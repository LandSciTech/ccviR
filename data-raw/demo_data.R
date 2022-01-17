## code to prepare `demo_data` dataset and save files to demo app

library(raster)
library(ccviR)
library(dplyr)
library(sf)

# Make demo raster datasets that are more vulerable/exposed at the top
rast <- matrix(0, nrow = 100, ncol = 100) %>%
  raster()

# MAT and CMD want difference to be higher at the top
MAT <- rast
values(MAT) <- seq(5, 10, length.out = 10000) %>%
  sort(decreasing = TRUE) %>% round()
MAT_2050 <- MAT
values(MAT_2050) <- seq(10, 30, length.out = 10000) %>%
  sort(decreasing = TRUE) %>% round()

CMD <- MAT - 5
CMD_2050 <- MAT_2050 - 5

# CCEI 0-25
CCEI <- rast
values(CCEI) <- seq(0, 25, length.out = 10000) %>% sort(decreasing = TRUE) %>%
  round()
CCEI <- shift(CCEI, dy = -1)

# MWMT MCMT more difference between the two is less exposure
MCMT <- rast
values(MCMT) <- seq(5, 10, length.out = 10000) %>%
  sort() %>% round()
MWMT <- MCMT
values(MWMT) <- seq(10, 20, length.out = 10000) %>%
  sort() %>% round()

# MAP is scored based on the range where lower variation in species range is
# more vulnerable
MAP <- rast
values(MAP) <- c(rep(seq(1, 100), 50),
                      rep(seq(1, 1000), 5))

# 7 is gain, 1 is lost, rest is maint, is assessed over the whole assessment
# area so should be 0 or NA outside range
HS_rast_high <- rast
values(HS_rast_high) <- c(sample(c(0:7, 1, 1, 1, 1), 3000, replace = TRUE),
                          rep(0, 4000),
                          rep(0, 3000))

HS_rast_med <- rast
values(HS_rast_med) <- c(rep(0, 3000),
                         sample(c(0:7), 4000, replace = TRUE),
                         rep(0, 3000))

HS_rast_low <- rast
values(HS_rast_low) <- c(rep(0, 3000),
                         rep(0, 4000),
                         sample(c(0:7, 6, 6, 6, 6), 3000, replace = TRUE))

# Should be a polygon of areas with special temperature regime
PTN_poly <- st_polygon(list(matrix(c(0.5, 0.5, 1,
                                1, 0, 1, 0.5, 0.5),
                              ncol = 2, byrow = TRUE))) %>%
  st_sfc() %>% st_sf()

rng_poly_high <- st_polygon(list(matrix(c(0, 0.75, 1, 0.75, 1,
                                          1, 0, 1, 0, 0.75),
                                        ncol = 2, byrow = TRUE))) %>%
  st_sfc() %>% st_sf()

rng_poly_med <- st_polygon(list(matrix(c(0, 0.25, 1, 0.25, 1,
                                          0.5, 0, 0.5, 0, 0.25),
                                        ncol = 2, byrow = TRUE))) %>%
  st_sfc() %>% st_sf()

rng_poly_low <- st_polygon(list(matrix(c(0, 0, 1, 0, 1,
                                         0.25, 0, 0.25, 0, 0),
                                       ncol = 2, byrow = TRUE))) %>%
  st_sfc() %>% st_sf()

nonbreed_poly <-  st_polygon(list(matrix(c(0, 0, 1, 0, 1,
                                           -0.25, 0, -0.25, 0, 0),
                                         ncol = 2, byrow = TRUE))) %>%
  st_sfc() %>% st_sf()

assess_poly <- st_bbox(MAT) %>% st_as_sfc() %>% st_as_sf()

# spat_res <- run_spatial(range_poly = rng_poly_high, scale_poly = assess_poly,
#                         non_breed_poly = nonbreed_poly,
#                         clim_vars_lst = list(mat = MAT_rast, cmd = CMD_rast,
#                                              ccei = CCEI_rast, htn = HTN_rast,
#                                              ptn = PTN_poly, map = MAP_rast),
#                                              hs_rast = mask(HS_rast, rng_poly_high))


#
# vuln_df <- make_vuln_df(0)
#
# vuln_df$Value1[1:15] <- c(0,0, 0,0,0,0, -1, -1, -1, -1, 0, 0, 0, 0, 0)
# vuln_df$Value1[26:29] <- c(-1, -1, -1, -1)
#
# res <- calc_vulnerability(spat_res, vuln_df)

# save the data to extdata so that it can be used with the app for a demo
clim_dat <- lst(MAT, MAT_2050, CMD, CMD_2050, CCEI, MWMT, MCMT, MAP)

sp_dat <- lst(rng_poly_high, rng_poly_med, rng_poly_low, nonbreed_poly,
              HS_rast_high, HS_rast_med, HS_rast_low,
              assess_poly, PTN_poly)

write_fun <- function(x, nm, dir, crs_use){
  if(inherits(x, "Raster")){
    crs(x) <- paste0("EPSG:", crs_use)
    if(nm == "CCEI"){
      writeRaster(x, paste0(dir, nm, ".img"), overwrite = TRUE)
    } else {
      writeRaster(x, paste0(dir, nm, ".tif"), overwrite = TRUE)
    }
  }
  if(inherits(x, "sf")){
    x <- st_set_crs(x, crs_use)
    write_sf(x, paste0(dir, nm, ".shp"))
  }
}

purrr::walk2(clim_dat, names(clim_dat), write_fun,
             dir = "inst/extdata/clim_files/raw/",
             crs_use = 3162)
purrr::walk2(sp_dat, names(sp_dat), write_fun,
             dir = "inst/extdata/",
             crs_use = 3162)

run_prep_data(in_folder = "inst/extdata/clim_files/raw",
              out_folder = "inst/extdata/clim_files/processed/",
              reproject = F, overwrite = T)

rng_poly_high <- read_sf("inst/extdata/rng_poly_high.shp", agr = "constant")
assess_poly <- read_sf("inst/extdata/assess_poly.shp", agr = "constant")
HS_rast_high <- raster("inst/extdata/HS_rast_high.tif")
PTN_poly <- read_sf("inst/extdata/PTN_poly.shp", agr = "constant")
nonbreed_poly <- read_sf("inst/extdata/nonbreed_poly.shp", agr = "constant")

spat_res <- run_spatial(range_poly = rng_poly_high, scale_poly = assess_poly,
                        ptn_poly = PTN_poly, non_breed_poly = nonbreed_poly,
                        hs_rast = HS_rast_high,
                        hs_rcl = matrix(c(c(0:7), c(0, 1, 2,2,2,2,2,3)), ncol = 2),
                        clim_vars_lst = get_clim_vars("inst/extdata/clim_files/processed/"))

vuln_df <- make_vuln_df("sp_name", 0)

vuln_df$Value1[1:15] <- c(0,0, 0,0,0,0, -1, -1, -1, -1, 0, 0, 0, 0, 0)
vuln_df$Value1[26:29] <- c(0, -1, -1, 0)

res <- calc_vulnerability(spat_res$spat_table, vuln_df, tax_grp = "Bird")

