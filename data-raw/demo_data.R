## code to prepare `demo_data` dataset and save files to demo app

library(raster)
library(ccviR)
library(dplyr)
library(sf)

# Make demo raster datasets that are more vulerable/exposed at the top
rast <- matrix(0, nrow = 100, ncol = 100) %>%
  raster()

# MAT and CMD have values 1-6 where 1 is more CC exposure
MAT_rast <- rast
values(MAT_rast) <- seq(1, 6, length.out = 10000) %>% sort() %>% round()

CMD_rast <- MAT_rast

# CCEI and HTN have values 1-4 and CCEI should be in the nonbreeding range
CCEI_rast <- rast
values(CCEI_rast) <- seq(1, 4, length.out = 10000) %>% sort() %>% round()

HTN_rast <- CCEI_rast

CCEI_rast <- shift(CCEI_rast, dy = -1)

# MAP is scored based on the range where lower variation in species range is
# more vulnerable
MAP_rast <- rast
values(MAP_rast) <- c(rep(seq(1, 100), 50),
                      rep(seq(1, 1000), 5))

# 7 is gain, 1 is lost, rest is maint, is assessed over the whole assessment
# area so should be 0 or NA outside range
HS_rast <- rast
values(HS_rast) <- c(sample(c(1:7, 1, 1, 1, 1), 3000, replace = TRUE),
                     sample(c(1:7), 4000, replace = TRUE),
                     sample(c(1:7, 7, 7, 7, 7), 3000, replace = TRUE))

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

assess_poly <- st_bbox(MAT_rast) %>% st_as_sfc() %>% st_as_sf()

spat_res <- run_spatial(range_poly = rng_poly_high, scale_poly = assess_poly,
                        non_breed_poly = nonbreed_poly,
                        clim_vars_lst = list(mat = MAT_rast, cmd = CMD_rast,
                                             ccei = CCEI_rast, htn = HTN_rast,
                                             ptn = PTN_poly, map = MAP_rast),
                                             hs_rast = mask(HS_rast, rng_poly_high))

make_vuln_df <- function(val1, val2 = NA, cave = 0 , mig = 0){
  vuln_qs <- tribble(
    ~Species, ~Code, ~Value1, ~Value2,
    "test_sp", "Z2", cave, NA,
    "test_sp", "Z3", mig, NA,
    "test_sp", "B1", val1, val2,
    "test_sp", "B2a", val1, val2,
    "test_sp", "B2b", val1, val2,
    "test_sp", "B3", val1, val2,
    "test_sp", "C1", val1, val2,
    "test_sp", "C2ai", val1, val2,
    "test_sp", "C2aii", val1, val2,
    "test_sp", "C2bi", val1, val2,
    "test_sp", "C2bii", val1, val2,
    "test_sp", "C2c", val1, val2,
    "test_sp", "C2d", val1, val2,
    "test_sp", "C3", val1, val2,
    "test_sp", "C4a", val1, val2,
    "test_sp", "C4b", val1, val2,
    "test_sp", "C4c", -1, val2,
    "test_sp", "C4d", val1, val2,
    "test_sp", "C4e", val1, ifelse(is.na(val2), val2, -1),
    "test_sp", "C4f", val1, val2,
    "test_sp", "C4g", val1, val2,
    "test_sp", "C5a", val1, val2,
    "test_sp", "C5b", val1, val2,
    "test_sp", "C5c", val1, val2,
    "test_sp", "C6", val1, val2,
    "test_sp", "D1", val1, val2,
    "test_sp", "D2", val1, val2,
    "test_sp", "D3", val1, val2,
    "test_sp", "D4", val1, val2,
  )
  vuln_qs %>% mutate(Value3 = NA_real_, Value4 = NA_real_)
}

vuln_df <- make_vuln_df(0)

vuln_df$Value1[1:15] <- c(0,0, 0,0,0,0, -1, -1, -1, -1, 0, 0, 0, 0, 0)
vuln_df$Value1[26:29] <- c(-1, -1, -1, -1)

res <- calc_vulnerability(spat_res, vuln_df)

# save the data to extdata so that it can be used with the app for a demo
clim_dat <- lst(MAT_rast, CMD_rast, CCEI_rast, HTN_rast, PTN_poly, MAP_rast)

sp_dat <- lst(rng_poly_high, rng_poly_med, rng_poly_low, nonbreed_poly, HS_rast,
              assess_poly)

write_fun <- function(x, nm, dir){
  if(is(x, "Raster")){
    writeRaster(x, paste0(dir, nm, ".tif"))
  }
  if(is(x, "sf")){
    st_write(x, paste0(dir, nm, ".shp"))
  }
}

purrr::walk2(clim_dat, names(clim_dat), write_fun,
             dir = "inst/extdata/clim_files/")
purrr::walk2(sp_dat, names(sp_dat), write_fun,
             dir = "inst/extdata/")

# make raw versions to use for testing data_prep

MAT <- HTN_rast
MAT_2050 <- MAT_rast

CMD <- HTN_rast
CMD_2050 <- CMD_rast

MAP <- MAP_rast

CCEI <- MAP_rast

MWMT <- MAT_rast

MCMT <- HTN_rast

clim_dat2 <- lst(MAT, MAT_2050, CMD, CMD_2050, MAP, MWMT, MCMT, CCEI)

clim_dat2 <- purrr:::map(clim_dat2, ~`crs<-`(.x, value = "+proj=longlat +datum=WGS84"))

purrr::walk2(clim_dat2[1:7], names(clim_dat2[1:7]), write_fun,
             dir = "inst/extdata/clim_files/raw/")

writeRaster(clim_dat2[[8]], paste0("inst/extdata/clim_files/raw/", "CCEI", ".img"))

run_prep_data(in_folder = "inst/extdata/clim_files/raw",
              out_folder = "inst/extdata/clim_files/processed/",
              reproject = F, overwrite = T)

prep_exp(MAT, MAT_2050, file_nm = "inst/extdata/clim_files/processed/MAT_reclass.tif",
         reproject = F)
MAT - MAT_2050

