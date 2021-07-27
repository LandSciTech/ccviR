context("test loading climate variables")
library("sf", quietly = TRUE)
library("raster", quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)

# load the demo data
file_dir <- system.file("extdata", package = "ccviR")
clim_vars <- get_clim_vars(file.path(file_dir, "clim_files"))

# make the crs's match to avoid warning it has to be verbatim the same
nonbreed <- st_read(file.path(file_dir, "nonbreed_poly.shp"), agr = "constant",
                    quiet = TRUE) %>% st_set_crs(4326)
assess <- st_read(file.path(file_dir, "assess_poly.shp"), agr = "constant",
                  quiet = TRUE) %>% st_set_crs(4326)
rng_high <- st_read(file.path(file_dir, "rng_poly_high.shp"), agr = "constant",
                    quiet = TRUE) %>% st_set_crs(4326)
hs <- raster(file.path(file_dir, "HS_rast.tif")) %>%
  `crs<-`(value = "+proj=longlat +datum=WGS84 +no_defs")

# clim_vars[1:5] <- purrr::map(clim_vars[1:5],
#                              ~`crs<-`(.x, value = "+proj=longlat +datum=WGS84 +no_defs"))
# clim_vars[[6]] <- st_set_crs(clim_vars[[6]], 4326)

test_that("basic version works",{
  expect_is(clim_vars, "list")
})

test_that("trimming happens", {
  na_rast <- clim_vars[[1]]
  na_rast <- setValues(na_rast, NA_real_) %>% shift(dy = 1)

  na_rast <- merge(na_rast, clim_vars[[1]])

  dir.create(file.path(file_dir, "temp"))
  file.copy(file.path(file_dir, "clim_files/MAT_rast.tif"),
            file.path(file_dir, "temp/MAT_rast.tif"))
  file.remove(file.path(file_dir, "clim_files/MAT_rast.tif"))

  writeRaster(na_rast, file.path(file_dir, "clim_files/MAT_rast_na.tif"))

  expect_message({
    clim_vars2 <- get_clim_vars(file.path(file_dir, "clim_files"))
  }, "doing trim" )

  file.copy(file.path(file_dir, "temp/MAT_rast.tif"),
            file.path(file_dir, "clim_files/MAT_rast.tif"))

  expect_equal(values(clim_vars[[1]]),
               values(clim_vars2[[1]]))

})

test_that("error when two files or missing files",{
  expect_error(get_clim_vars(file.path(file_dir, "clim_files")),
               "More than one file")

  #remove na_rast from previous version
  file.remove(file.path(file_dir, "clim_files/MAT_rast_na.tif"))

  # missing MAT
  file.remove(file.path(file_dir, "clim_files/MAT_rast.tif"))
  expect_error(get_clim_vars(file.path(file_dir, "clim_files")),
               "There is no file in")

  file.copy(file.path(file_dir, "temp/MAT_rast.tif"),
            file.path(file_dir, "clim_files/MAT_rast.tif"))

  # other files are allowed to be missing
  file.copy(file.path(file_dir, "clim_files/HTN_rast.tif"),
            file.path(file_dir, "temp/HTN_rast.tif"))
  file.remove(file.path(file_dir, "clim_files/HTN_rast.tif"))

  expect_null(get_clim_vars(file.path(file_dir, "clim_files"))$htn)

  file.copy(file.path(file_dir, "temp/HTN_rast.tif"),
            file.path(file_dir, "clim_files/HTN_rast.tif"))

  })

# remove the temp directory
unlink(file.path(file_dir, "temp"), recursive = TRUE)
