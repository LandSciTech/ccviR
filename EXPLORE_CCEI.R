library(terra)
library(dplyr)
library(fs)

# Local paths
d <- path("misc", "ccei")

ccei_ns <- rast(path(d, "ccei.img")) %>% stats::setNames("NatureServe")
ccei_245 <- rast(path(d, "ccei_ssp245.tif")) %>% stats::setNames("SSP 245")
ccei_858 <- rast(path(d, "ccei_ssp585.tif")) %>% stats::setNames("SSP 858")

# South American points of interest where CMD extra high
box1 <- sf::st_bbox(c(xmin = -75, xmax = -50, ymin = -15, ymax = 0), crs = 4326) %>% ext()
box2 <- sf::st_bbox(c(xmin = -72.4, xmax = -71, ymin = -2.8, ymax = -1.5), crs = 4326) %>% ext()
box3 <- sf::st_bbox(c(xmin = -55, xmax = -50, ymin = -5, ymax = 0), crs = 4326) %>% ext()

# Compare the plots ---------------------------------
c(crop(ccei_245, box1), crop(ccei_858, box1), crop(ccei_ns, box1)) %>%
  plot()

c(crop(ccei_245, box2), crop(ccei_858, box2), crop(ccei_ns, box2)) %>%
  plot()

c(crop(ccei_245, box3), crop(ccei_858, box3), crop(ccei_ns, box3)) %>%
  plot()

# So the NatureServe CCEI also has missing ares
# NS already normalized? Standardized to 0-25? Or truncated...?

max(ccei_ns) # Max is is just about 25...



# Explore how calculations result in high CCEI --------------------------------

# SED - calc_sed()
# (b - a)^2 / s^2
# b = Future climate
# a = Historical climate
# s = SD historical

# High values are driven by either b ~= a or by very small s
# Inf values are driven by s == 0

# Climate values are CMD or Tmean

# CMD is zero if Eref <= Precipitation (so if Precipitation is especially high or Eref low)
# CMD is small if Eref is very close to PPT

# Look at pre-cut out mini section (i.e. box2 from above)
path_ccei <- fs::path_package("extdata", "ccei_test", package = "ccviR")

# Inf and high CCEI Values:
ccei_85 <- terra::rast(fs::path(path_ccei, "ccei_ssp585.tif")) %>%
  stats::setNames("SSP 585")
ccei_45 <- terra::rast(fs::path(path_ccei, "ccei_ssp245.tif")) %>%
  stats::setNames("SSP 245")

# White Squares are NAs
# (Current version of calc_ccei() replaces 0 sd values with low values to prevent
#  dividing by zero which caused the missing values)
terra::plot(c(ccei_45, ccei_85))
terra::plot(c(is.na(ccei_45), is.na(ccei_85)))

# Values are higher in 585, but the same problems of exceptionally
# high areas happen in both, suggesting it's related to *historical*
# values.


# Some typical values for Eref in the area we're looking at (upper right corner)
# NOTE: Not exact, because using min/max expected values for tmin, tmax
eref <- purrr::map_dbl(1:12, ~climr:::calc_Eref(.x, 20, 30, -1.6))

# Historical precipitation - 1960 & 1989 for example
prec <- fs::dir_ls(fs::path(path_ccei, "historical"), glob = "*prec*") %>%
  terra::rast()

n1 <- 1:12    # 1960
n2 <- 349:360 # 1989
terra::plot(prec[[n1]])
terra::plot(prec[[n2]])

# Rough CMD - HIST
cmd <- eref - prec[[n2]]

# Where are we negative (i.e. would be CMD 0)
terra::plot(cmd > 0)

# Which areas never have a CMD > 0?
terra::plot(sum(cmd > 0) > 0)

# Therefore, the 0 values in CMD result from unexpectedly high precipitation
# In general, and the fact that preciptation is *never* less than ERef in some
# locations (i.e. the upper right corner).

# Definitely a CMD problem, not a tmean problem ----------------------------

hist <- fs::path(path_ccei, "intermediate", "hist_all_vars.tif") %>%
  terra::rast()
future <- fs::path(path_ccei, "intermediate",
                   "future_CanESM5-ssp585.tif") %>% # High CMD values
  terra::rast()
h <- terra::values(hist)
f <- terra::values(future)

# Replace zeros
for(v in c("cmd_sd", "tmean_sd")) {
  zeros <- h[, v] == 0
  h[zeros, v] <- min(0.00001, min(h[!zeros, v], na.rm = TRUE))
}

# Calculate SED for different subsets
sed_all <- calc_sed(b = list(f[, "cmd"], f[, "tmean"]),
                    a = list(h[, "cmd_mean"], h[, "tmean_mean"]),
                    s = list(h[, "cmd_sd"], h[,"tmean_sd"]))
sed_cmd <- calc_sed(b = list(f[, "cmd"]),
                    a = list(h[, "cmd_mean"]),
                    s = list(h[, "cmd_sd"]))
sed_tmean <- calc_sed(b = list(f[, "tmean"]),
                      a = list(h[, "tmean_mean"]),
                      s = list(h[,"tmean_sd"]))

future$sed_all <- sed_all
future$sed_cmd <- sed_cmd
future$sed_tmean <- sed_tmean

terra::plot(future)

# Two problems:
# - SD of 0 - Fixed by replacing with very small value.
#   This results in a very large CCEI but it gets reclassified anyway.
# - Low SD values - Fixed by reclassifying.
