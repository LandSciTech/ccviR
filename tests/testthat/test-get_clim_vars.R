context("test loading climate variables")
library("sf", quietly = TRUE)
library("raster", quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)

# load the demo data
file_dir <- system.file("extdata", package = "ccviR")
clim_vars <- get_clim_vars(file.path(file_dir, "clim_files/processed"))

# make the crs's match to avoid warning it has to be verbatim the same
nonbreed <- st_read(file.path(file_dir, "nonbreed_poly.shp"), agr = "constant",
                    quiet = TRUE)
assess <- st_read(file.path(file_dir, "assess_poly.shp"), agr = "constant",
                  quiet = TRUE)
rng_high <- st_read(file.path(file_dir, "rng_poly_high.shp"), agr = "constant",
                    quiet = TRUE)
hs <- raster(file.path(file_dir, "HS_rast_high.tif"))

# clim_vars[1:5] <- purrr::map(clim_vars[1:5],
#                              ~`crs<-`(.x, value = "+proj=longlat +datum=WGS84 +no_defs"))
# clim_vars[[6]] <- st_set_crs(clim_vars[[6]], 4326)

test_that("basic version works",{
  expect_is(clim_vars, "list")
})

test_that("trimming happens", {
  na_rast <- clim_vars[[1]]
  na_rast <- setValues(na_rast, NA_real_) %>% shift(dy = 1)

  na_rast <- raster::merge(na_rast, clim_vars[[1]])

  # copy MAT
  dir.create(file.path(file_dir, "temp"))
  file.copy(file.path(file_dir, "clim_files/processed/MAT_reclass.tif"),
            file.path(file_dir, "temp/MAT_reclass.tif"))
  # remove MAT
  file.remove(file.path(file_dir, "clim_files/processed/MAT_reclass.tif"))

  # replace MAT
  writeRaster(na_rast, file.path(file_dir, "clim_files/processed/MAT_rast_na.tif"))

  expect_message({
    clim_vars2 <- get_clim_vars(file.path(file_dir, "clim_files/processed"))
  }, "doing trim" )

  # return MAT
  expect_true(file.copy(file.path(file_dir, "temp/MAT_reclass.tif"),
                        file.path(file_dir, "clim_files/processed/MAT_reclass.tif")))

  expect_equal(values(clim_vars[[1]]),
               values(clim_vars2[[1]]))

})

test_that("error when two files or missing files",{
  expect_error(get_clim_vars(file.path(file_dir, "clim_files/processed")),
               "More than one file")

  #remove MAT na_rast from previous test
  file.remove(file.path(file_dir, "clim_files/processed/MAT_rast_na.tif"))

  # missing MAT
  file.remove(file.path(file_dir, "clim_files/processed/MAT_reclass.tif"))
  expect_error(get_clim_vars(file.path(file_dir, "clim_files/processed")),
               "There is no file in")

  # return MAT
  expect_true(file.copy(file.path(file_dir, "temp/MAT_reclass.tif"),
                        file.path(file_dir, "clim_files/processed/MAT_reclass.tif")))

  # other files are allowed to be missing
  file.copy(file.path(file_dir, "clim_files/processed/MWMT_MCMT_reclass.tif"),
            file.path(file_dir, "temp/MWMT_MCMT_reclass.tif"))
  file.remove(file.path(file_dir, "clim_files/processed/MWMT_MCMT_reclass.tif"))

  expect_null(get_clim_vars(file.path(file_dir, "clim_files/processed"))$htn)

  expect_true(file.copy(file.path(file_dir, "temp/MWMT_MCMT_reclass.tif"),
            file.path(file_dir, "clim_files/processed/MWMT_MCMT_reclass.tif")))

  })

# remove the temp directory
#unlink(file.path(file_dir, "temp"), recursive = TRUE)
