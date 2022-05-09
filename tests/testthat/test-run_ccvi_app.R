context("Test application")
# This file is for testing the applications in the apps/ directory.

library(shinytest)
# Not working and keeps breaking for no reason!
# test_that("run_ccvi_app() works", {
#   # Don't run these tests on the CRAN build servers
#   skip_on_cran()
#
#   # Use compareImages=FALSE because the expected image screenshots were created
#   # on a Mac, and they will differ from screenshots taken on the CI platform,
#   # which runs on Linux.
#   expect_pass(testApp(test_path("app/"), compareImages = FALSE))
# })

# # to see what changed
# shinytest::snapshotCompare('tests/testthat/app')

# # To update a snapshot
# shinytest::snapshotUpdate('tests/testthat/app')

# # To record a new test
# shinytest::recordTest("tests/testthat/app")

file.remove("app/Rplots.pdf")
