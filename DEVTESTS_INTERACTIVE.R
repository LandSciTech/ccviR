# Most if not all of these 'tests' are also presented as 'examples' in their
# respective documentations.

# Full app tests -----------------------------

# Use extdata for interactive testing
shinyOptions("file_dir" = fs::path_package("extdata/", package = "ccviR"))

ccvi_app()  # Basic, no files
ccvi_app(input_files = test_files()) # With test paths pre-filled
ccvi_app(input_files = test_files(min_req = TRUE)) # Min-required only

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

# Big test ----------------------------------
# Use big local data and full sized CCEI
shinyOptions("file_dir" = "misc")
ccvi_app()

# Specific checks --------------------------------------
# ✔ Check saved data - Full Run-----------------------------
# Launch App - Reload File - Save file - Generate Report - Re-calculate index - Save file - Compare

ccvi_app()

waldo::compare(
  read.csv(fs::path_package("ccviR", "extdata", "test_files", "test_full_run.csv")),
  read.csv(fs::path_package("ccviR", "extdata", "test_comp1.csv"))
)

waldo::compare(
  read.csv(fs::path_package("ccviR", "extdata", "test_files", "test_full_run.csv")),
  read.csv(fs::path_package("ccviR", "extdata", "test_comp2.csv"))
)


# Launch App - Reload File - Re-calculate index - Save file - Compare
waldo::compare(
  read.csv("misc/test_save_protected_areas2.csv"),
  read.csv("misc/test_save_protected_areas2_comp.csv"), max_diffs = Inf
)

# ✔ Check saved data - Min Spatial Run-----------------------------
# Launch App - Reload File - Save file - Re-calculate index - Save file - Compare

ccvi_app()

waldo::compare(
  read.csv(fs::path_package("ccviR", "extdata", "test_files", "test_min_spatial.csv")),
  read.csv(fs::path_package("ccviR", "extdata", "test_comp1.csv")),
  list_as_map = TRUE
)

waldo::compare(
  read.csv(fs::path_package("ccviR", "extdata", "test_files", "test_min_spatial.csv")),
  read.csv(fs::path_package("ccviR", "extdata", "test_comp2.csv")),
  list_as_map = TRUE
)

# ✔ Check saved data - With manually changing spatial scores -------------------
# Test1 -> Reload test_full_run.csv  - Make change to spatial questions - Compare (expect no index now)
# Test2 -> Reload - Check results - Save - Compare

ccvi_app()

# Expect differences in the questions
waldo::compare(
  read.csv(fs::path_package("ccviR", "extdata", "test_files", "test_full_run.csv")) %>%
    select(matches("[A-D]{1}\\d")),
  read.csv(fs::path_package("ccviR", "extdata", "test_comp1.csv"))  %>%
    select(matches("[A-D]{1}\\d"))
)

# Expect NO differences
waldo::compare(
  read.csv(fs::path_package("ccviR", "extdata", "test_comp1.csv")) %>%
    select(matches("[A-D]{1}\\d")),
  read.csv(fs::path_package("ccviR", "extdata", "test_comp2.csv")) %>%
    select(matches("[A-D]{1}\\d"))
)

# ✔ Check saved data - Do not lose manually answered spatial questions when no spatial data to answer them -------------------
# Test1 -> Reload test_min_spatial.csv  - Make change to spatial questions (D, make several) - Save as test_comp1.csv - Compare (expect no index now)
# Test2 -> Reload test_comp1.csv - Check results - Save as test_comp2.csv - Compare

ccvi_app()

# Expect differences in the questions
waldo::compare(
  read.csv(fs::path_package("ccviR", "extdata", "test_files", "test_min_spatial.csv")) %>%
    select(matches("[A-D]{1}\\d")),
  read.csv(fs::path_package("ccviR", "extdata", "test_comp1.csv"))  %>%
    select(matches("[A-D]{1}\\d"))
)

# Expect NO differences
waldo::compare(
  read.csv(fs::path_package("ccviR", "extdata", "test_comp1.csv")),
  read.csv(fs::path_package("ccviR", "extdata", "test_comp2.csv"))
)

# ✔ Check that when re-run spatial, don't loose comments -----------------

# ✔ Check that when only comments/evidence updated gets saved and included in reports -----------------
# (even if the index is NOT rerun)

# Check that can reload a different file in the same session  -----------------
# - No errors
# - All questions are replaced
#
# Launch - Fill in only a few file paths - Save file as test_mini_run.csv
# Launch - Load test_full_run.csv - Load test_mini_run.csv and check that file paths have been removed
ccvi_app()

# TRICKY, consider just asking users not to do this?

# ✔ Check that can save/reload a questions from optional spatial data -----------------
# This data has no spatial
mod_D_test(df_loaded = test_df_loaded("questions_only"),
           input_files = test_files(min_req = TRUE))

ccvi_app()

# ✔ Check that can run with min spatial requirements  -----------------
ccvi_app()

# ✔ Check that get sensible error when missing CCEI (etc.) breaks  ------------
# Use misc/clim full/processed_no_ccei_brks for the Clim files
shinyOptions("file_dir" = ".")
ccvi_app()

# Varieties of conditions ----------------------------------------------

## ✔ Non-migratory --------------------

# Choose anything BUT Mammal, Bird, or Invert-Insect
# Expect no Migratory Exposure index - In results, in report, in saved files
ccvi_app()
ccvi_app(input_files = test_files())


mod_results_test(species_data = test_species("full_run_non_migratory"),
                 spatial = test_spatial(),
                 questions = test_questions("full_run_non_migratory"))


## Obligate caves --------------------------


## Gain modifier -------------------------



## ✔ Expect different questions for different taxa -----------------------------

# ✔ Animal only - C4b
mod_C_test(tax_grp = "Invert-Insect")
mod_C_test(tax_grp = "Invert-Mollusk")
mod_C_test(tax_grp = "Invert-Other")
mod_C_test(tax_grp = "Fish")
mod_C_test(tax_grp = "Amphibian")
mod_C_test(tax_grp = "Reptile")
mod_C_test(tax_grp = "Mammal")
mod_C_test(tax_grp = "Bird")

# ✔ Plant only - C4c
mod_C_test(tax_grp = "Vascular Plant")
mod_C_test(tax_grp = "Nonvascular Plant")

# ✔ Not plant nor animal (no C4b nor C4c)
mod_C_test(tax_grp = "Lichen")


## ✔ Only one range change but multiple scenarios ---------------------------
# Launch, migratory bird, all spatial except only ONE range change scenario
# - Expect that cannot edit D because multiple scenarios... correct?
shinyOptions("file_dir" = "inst/extdata")
ccvi_app()

# Expect NO differences
waldo::compare(
  read.csv(fs::path_package("ccviR", "extdata", "test_comp.csv")),
  read.csv(fs::path_package("ccviR", "extdata", "test_comp1.csv"))
)

## ✔ Only one scenario, section D works as expected ---------------------------
# Use misc/clim full/processed_single_scenario with only 4.5 (and README has only one row for 4.5)
shinyOptions("file_dir" = ".")
ccvi_app()

# Expect NO differences
waldo::compare(
  read.csv(fs::path_package("ccviR", "extdata", "test_comp.csv")),
  read.csv(fs::path_package("ccviR", "extdata", "test_comp1.csv"))
)

## Empty range change matrix values (should not be NA in matrix) ----------

# Troubleshooting -------------------------------------------------------
# Testing a particular set of files
shinyOptions("file_dir" = "misc/")
ccvi_app()

shinyOptions("file_dir" = ".")
ccvi_app()


# Clean up ----------------------------------------------------------------

file.remove(fs::path_package("ccviR", "extdata", "test_comp1.csv"))
file.remove(fs::path_package("ccviR", "extdata", "test_comp2.csv"))
