# Some things to consider with CCEI
path_ccei <- "misc/ccei"
hist <- terra::rast(fs::path(path_ccei, "intermediate", "hist_all_vars.tif"))
future <- terra::rast(fs::path(path_ccei, "intermediate", "future_all_vars.tif"))

# Some CMD st devs are zero or very small as they occur in cold areas, where the
# CMD is 0 (where all monthly mean temps are < 0, or where precipitation is greater than Eref).

terra::plot(hist[["cmd_sd"]] < 1)
terra::plot(hist[["tmean_mean"]] < 0)

# This results in Infinite values for the Standardized Euclidean distance (dividing by zero)


terra::plot(r)

r0 <- terra::rast("misc/ccei/ccei.img")

terra::plot(r0)
terra::plot(terra::crop(r, r0))


#' calc_ccei(scenario = "ssp245")
#' calc_ccei(scenario = "ssp245",
#'           models = c("ACCESS-ESM1-5", "CanESM5"),
#'           out_append = "test")
r0 <- terra::rast("misc/ccei/ccei.img")
r1 <- terra::rast("misc/ccei/ccei_ssp245.tif")
r2 <- terra::rast("misc/ccei/ccei_ssp585.tif")
#t <- terra::rast("misc/ccei/ccei_ssp245_test.tif")

terra::plot(r0)
terra::plot(r1)
terra::plot(r2)
#terra::plot(t)

ext <- terra::ext(-67.68503693181682, -45.94485241375469,
                  -22.813051124621953, 0.49423033772188435)
ext2 <- terra::ext(-75.68503693181682, -45.94485241375469,
                   -22.813051124621953, 0.49423033772188435)


p <- terra::rast(fs::dir_ls("misc/ccei/historical/", glob = "*prec*.tif"))
raster::plot(p, ext = ext2)

c <- terra::rast("misc/ccei/intermediate/hist_groups_cmd.tif")
t <- terra::rast("misc/ccei/intermediate/hist_groups_tmean.tif")
terra::plot(c)
terra::plot(c, ext = ext2)
terra::plot(t)
terra::plot(t[[1]])

vars <- terra::rast("misc/ccei/intermediate/hist_all_vars.tif")
terra::plot(vars, ext = ext2)

terra::plot(vars[["cmd_sd"]], ext = ext2)
sd <- vars[["cmd_sd"]]
sd[sd[["cmd_sd"]] > 1] <- NA
terra::plot(sd, add = TRUE, col = "red")  # Really low standard deviations


ccei <- terra::rast("misc/ccei/ccei_ssp245.tif")
terra::plot(ccei)
terra::plot(ccei, ext = ext2)

ccei2 <- ccei
ccei2[ccei == Inf] <- NA
terra::plot(ccei2, ext = ext2)

brks_ccei <- c(0, 4, 5, 7, Inf)
rcl_tbl_ccei <- matrix(c(brks_ccei[1:4], brks_ccei[2:5], 1:4), ncol = 3)
ccei3 <- terra::classify(ccei, rcl_tbl_ccei, right = NA)
terra::plot(ccei3)
terra::plot(ccei3, ext = ext2)

# Orig NatureServe
brks_ccei <- c(0, 4, 5, 7, 25)
rcl_tbl_ccei <- matrix(c(brks_ccei[1:4], brks_ccei[2:5], 1:4), ncol = 3)
ccei0 <- terra::rast("misc/ccei/ccei.img") %>%
  terra::classify(rcl_tbl_ccei, right = NA)
terra::plot(ccei0)
terra::plot(ccei3, ext = ext2)
