# Create a mini test data set for CCEI

library(terra)
library(dplyr)
library(fs)

# Local paths
dir_ccei <- path("misc", "ccei")
dir_hist <- path(dir_ccei, "historical")
dir_future <- path(dir_ccei, "future")

# Subsample out paths
dir_test <- path(path_package("extdata", package = "ccviR"), "ccei_test")
dir_create(dir_test)
dir_out_future <- dir_create(path(dir_test, "future"))
dir_out_hist <- dir_create(path(dir_test, "historical"))

ccei_ns <- rast(path(dir_ccei, "ccei.img"))
ccei_245 <- rast(path(dir_ccei, "ccei_ssp245.tif"))
ccei <- rast(path(dir_ccei, "ccei_ssp585.tif"))

# Find really high values
summary(ccei, size = 1000000)

# Not very many, but huge!
# In South America
plot(ccei, type = "interval", breaks = c(0, 20, Inf))

brks <- c(0, 1, 3, 5, 10, 15, 20, 50, 100, 500, 1000, Inf)

(box1 <- sf::st_bbox(c(xmin = -75, xmax = -50, ymin = -15, ymax = 0), crs = 4326)) %>%
  ext() %>%
  crop(x = ccei, y = .) %>%
  plot(type = "interval", breaks = brks)

# Even closer
(box2 <- sf::st_bbox(c(xmin = -72.4, xmax = -71, ymin = -2.8, ymax = -1.5), crs = 4326)) %>%
  ext() %>%
  crop(x = ccei, y = .) %>%
  plot(type = "interval", breaks = brks)

sample <- crop(ccei, box2)

# Grab future data and crop - 3.1 MB in inst/ext_data ... okay?

f_future <- dir_ls(path(dir_future), glob = "*ssp585*")

for(f in f_future) {
  nm <- stringr::str_replace(path_file(f), "\\.tif", "_subsample.tif")
  rast(f) %>%
    crop(box2) %>%
    writeRaster(path(dir_out_future, nm), overwrite = TRUE)
}


f_hist <- dir_ls(path(dir_hist), glob = "*.tif")
for(f in f_hist) {
  nm <- stringr::str_replace(path_file(f), "\\.tif", "_subsample.tif")
  rast(f) %>%
    crop(box2) %>%
    writeRaster(path(dir_out_hist, nm), overwrite = TRUE)
}

# Prep
prep_ccei_historical(dir_test)
prep_ccei_future(dir_test)
calc_ccei(dir_test, scenario = "ssp585", overwrite = TRUE)

# Check
r <- terra::rast(path(dir_test, "ccei_ssp585.tif"))
terra::plot(r)
terra::values(r)[1:30,]

# Add for comparison
crop(ccei_245, box2) %>%
  writeRaster(path(dir_test, "ccei_ssp245.tif"), overwrite = TRUE)
