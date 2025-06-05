# Most if not all of these 'tests' are also presented as 'examples' in their
# respective documentations.

# Full app tests -----------------------------

# Use extdata for interactive testing
shinyOptions("file_dir" = fs::path_package("extdata/", package = "ccviR"))

ccvi_app2()  # Basic, no files
ccvi_app2(input_files = test_files()) # With test paths pre-filled
ccvi_app2(input_files = test_files(min_req = TRUE)) # Min-required only

# Module tests ------------------------------
mod_home_test()
mod_spatial_test()
mod_A_test()
mod_B_test()
mod_C_test()
mod_D_test()
mod_results_test()


# Minimum spatial data --------------------------
mod_spatial_test(input_files = test_files(min_req = TRUE))
mod_A_test(spatial = test_spatial(min_req = TRUE))
mod_C_test(input_files = test_files(min_req = TRUE))
mod_D_test(input_files = test_files(min_req = TRUE))
mod_results_test(spatial = test_spatial(min_req = TRUE),
                 questions = test_questions())


# Re-loading data --------------------------------------
mod_C_test(df_loaded = test_df_loaded())
mod_C_test(input_files = test_files(min_req = TRUE)) # Min-required only

# ✔ Check saved data -----------------------------
# Launch App - Reload File - Save file - Compare

ccvi_app2()

waldo::compare(
  read.csv(fs::path_package("ccviR", "extdata", "test_files", "test_full_run.csv")),
  read.csv(fs::path_package("ccviR", "extdata", "test_comp.csv"))
)


waldo::compare(
  read.csv(fs::path_package("ccviR", "extdata", "test_files", "test_full_run.csv")),
  read.csv(fs::path_package("ccviR", "extdata", "test_files", "test_full_run2.csv"))
)

# ✔ Check saved data - With manually changing spatial scores -------------------
# test_sp_changes.csv -> Launch App - Reload test_full_run.csv  - Make change to spatial questions - Save
# Test -> Launch App - Reload test_sp_changes.csv - Check results - Save - Compare

ccvi_app2()

waldo::compare(
  read.csv(fs::path_package("ccviR", "extdata", "test_files", "test_sp_changes.csv")),
  read.csv(fs::path_package("ccviR", "extdata", "test_comp.csv"))
)

# ✔ Check that when re-run spatial, don't loose comments -----------------

# ✔ Check that when only comments/evidence updated gets saved and included in reports -----------------

# Check that can reload a different file  -----------------

# Varieties of conditions ----------------------------------------------

# Obliate caves
# Migratory
# Gain modifier

# Different questions for different taxa
mod_C_test(tax_grp = "Nonvascular Plant")
mod_C_test(tax_grp = "Lichen")
mod_C_test(tax_grp = "Invert-Insect")
mod_C_test(tax_grp = "Invert-Mollusk")
mod_C_test(tax_grp = "Invert-Other")
mod_C_test(tax_grp = "Fish")
mod_C_test(tax_grp = "Amphibian")
mod_C_test(tax_grp = "Reptile")
mod_C_test(tax_grp = "Mammal")
mod_C_test(tax_grp = "Bird")


# Test when run spatial but then change spatial question answers -------

# Test when reload previous run but then change questions ---------

# Only one range change but multiple scenarios
# Empty range change matrix values (should not be NA in matrix)

# Troubleshooting -------------------------------------------------------
# Testing a particular set of files
shinyOptions("file_dir" = "misc/external_test_files/tundra_example/")
ccvi_app2()
