context("test loading climate variables")
library("sf", quietly = TRUE)
library("raster", quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)

# load the demo data
file_dir <- system.file("extdata", package = "ccviR")
# scenario names
scn_nms <- c("RCP 4.5", "RCP 8.5")
clim_vars <- get_clim_vars(file.path(file_dir, "clim_files/processed"),
                           scn_nms)

# nonbreed <- st_read(file.path(file_dir, "nonbreed_poly.shp"), agr = "constant",
#                     quiet = TRUE)
assess <- st_read(file.path(file_dir, "assess_poly.shp"), agr = "constant",
                  quiet = TRUE)
rng_high <- st_read(file.path(file_dir, "rng_poly.shp"), agr = "constant",
                    quiet = TRUE)
hs <- raster(file.path(file_dir, "rng_chg_45.tif"))


test_that("basic version works",{
  expect_is(clim_vars, "list")
})

test_that("trimming happens", {
  na_rast <- raster::extend(clim_vars[[1]], 20)

  # copy MAT
  dir.create(file.path(file_dir, "temp"))
  file.copy(file.path(file_dir, "clim_files/processed/MAT_reclassRCP_4.5.tif"),
            file.path(file_dir, "temp/MAT_reclassRCP_4.5.tif"))
  file.copy(file.path(file_dir, "clim_files/processed/MAT_reclassRCP_8.5.tif"),
            file.path(file_dir, "temp/MAT_reclassRCP_8.5.tif"))

  # remove MAT
  file.remove(file.path(file_dir, "clim_files/processed/MAT_reclassRCP_4.5.tif"))
  file.remove(file.path(file_dir, "clim_files/processed/MAT_reclassRCP_8.5.tif"))

  # replace MAT
  purrr::map2(unstack(na_rast), stringr::str_replace_all(scn_nms, "\\s", "_"),
       ~writeRaster(.x,
                    file.path(file_dir,
                              paste0("clim_files/processed/MAT_rast_na", .y, ".tif"))))

  expect_message({
    clim_vars2 <- get_clim_vars(file.path(file_dir, "clim_files/processed"),
                                scn_nms)
  }, "doing trim" )

  # return MAT
  expect_true(file.copy(file.path(file_dir, "temp/MAT_reclassRCP_4.5.tif"),
                        file.path(file_dir, "clim_files/processed/MAT_reclassRCP_4.5.tif")))
  expect_true(file.copy(file.path(file_dir, "temp/MAT_reclassRCP_8.5.tif"),
                        file.path(file_dir, "clim_files/processed/MAT_reclassRCP_8.5.tif")))

  expect_equal(raster::values(clim_vars[[1]][[1]]),
               raster::values(clim_vars2[[1]][[1]]))

})

test_that("error when two files or missing files",{
  expect_error(get_clim_vars(file.path(file_dir, "clim_files/processed")),
               "number of files matching")

  #remove MAT na_rast from previous test
  file.remove(file.path(file_dir, "clim_files/processed/MAT_rast_naRCP_4.5.tif"))
  file.remove(file.path(file_dir, "clim_files/processed/MAT_rast_naRCP_8.5.tif"))

  # missing MAT
  file.remove(file.path(file_dir, "clim_files/processed/MAT_reclassRCP_4.5.tif"))
  file.remove(file.path(file_dir, "clim_files/processed/MAT_reclassRCP_8.5.tif"))
  expect_error(get_clim_vars(file.path(file_dir, "clim_files/processed")),
               "There is no file in")

  # return MAT
  expect_true(file.copy(file.path(file_dir, "temp/MAT_reclassRCP_4.5.tif"),
                        file.path(file_dir, "clim_files/processed/MAT_reclassRCP_4.5.tif")))
  expect_true(file.copy(file.path(file_dir, "temp/MAT_reclassRCP_8.5.tif"),
                        file.path(file_dir, "clim_files/processed/MAT_reclassRCP_8.5.tif")))

  # other files are allowed to be missing
  file.copy(file.path(file_dir, "clim_files/processed/MWMT_MCMT_reclass.tif"),
            file.path(file_dir, "temp/MWMT_MCMT_reclass.tif"))
  file.remove(file.path(file_dir, "clim_files/processed/MWMT_MCMT_reclass.tif"))

  expect_null(get_clim_vars(file.path(file_dir, "clim_files/processed"), scn_nms)$htn)

  expect_true(file.copy(file.path(file_dir, "temp/MWMT_MCMT_reclass.tif"),
            file.path(file_dir, "clim_files/processed/MWMT_MCMT_reclass.tif")))

  })

test_that("multi_scenario works as expected", {
  expect_error(get_clim_vars(file.path(file_dir,
                                       "clim_files/processed"),
                             scenario_names = c("RCP2.6", "RCP4.5", "RCP85")),
               "does not match")


})

# remove the temp directory
unlink(file.path(file_dir, "temp"), recursive = TRUE)
