
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# -----> Build and Install package - Ctrl-Shift-B <------
#
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Load all - Ctrl-Shift-L

# Making shinytest2 tests ----------------------
# Make sure to COPY THE CODE!
record_test(mod_home_test())
record_test(mod_species_test())

shinytest2::record_test(ccvi_app())

# Reviewing any test --------------------------
# NOTE: Often "cannot find bottom of stack" (or similar) for testServer()
#  simply implies that the snapshot has changed, so review.

snapshot_review()

testthat::snapshot_review()
