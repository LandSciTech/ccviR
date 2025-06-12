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

# Big test ----------------------------------
# Use big local data and full sized CCEI
shinyOptions("file_dir" = ".")
ccvi_app2()

# Specific checks --------------------------------------
# ✔ Check saved data -----------------------------
# Launch App - Reload File - Save file - Compare

ccvi_app2()

waldo::compare(
  read.csv(fs::path_package("ccviR", "extdata", "test_files", "test_full_run.csv")),
  read.csv(fs::path_package("ccviR", "extdata", "test_comp1.csv"))
)

# Launch App - Reload File - Re-calculate index - Save file - Compare
waldo::compare(
  read.csv(fs::path_package("ccviR", "extdata", "test_files", "test_full_run.csv")),
  read.csv(fs::path_package("ccviR", "extdata", "test_comp2.csv"))
)

# ✔ Check saved data - With manually changing spatial scores -------------------
# Test1 -> Reload test_full_run.csv  - Make change to spatial questions Save as test_sp_changes.csv - Compare (expect no index now)
# Test2 -> Reload test_sp_changes.csv - Check results - Save as test_comp.csv - Compare

ccvi_app2()

# Expect differences in the questions
waldo::compare(
  read.csv(fs::path_package("ccviR", "extdata", "test_files", "test_full_run.csv")) %>%
    select(matches("[A-D]{1}\\d")),
  read.csv(fs::path_package("ccviR", "extdata", "test_files", "test_sp_changes.csv"))  %>%
    select(matches("[A-D]{1}\\d"))
)

# Expect NO differences
waldo::compare(
  read.csv(fs::path_package("ccviR", "extdata", "test_files", "test_sp_changes.csv")),
  read.csv(fs::path_package("ccviR", "extdata", "test_comp.csv"))
)

# ✔ Check that when re-run spatial, don't loose comments -----------------

# ✔ Check that when only comments/evidence updated gets saved and included in reports -----------------

# ✔ Check that can reload a different file in the same session  -----------------

# ✔ Check that can save/reload a questions from optional spatial data -----------------
# This data has no spatial
mod_D_test(df_loaded = test_df_loaded("questions_only"),
           input_files = test_files(min_req = TRUE))

ccvi_app2()

# ✔ Check that can run with min spatial requirements  -----------------
ccvi_app2()

# ✔ Check that get sensible error when missing CCEI (etc.) breaks  ------------
# Use misc/clim full/processed_no_ccei_brks for the Clim files
shinyOptions("file_dir" = ".")
ccvi_app2()

# Varieties of conditions ----------------------------------------------

## ✔ Non-migratory --------------------

# Choose anything BUT Mammal, Bird, or Invert-Insect
# Expect no Migratory Exposure index - In results, in report, in saved files
ccvi_app2()
ccvi_app2(input_files = test_files())


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


## Only one range change but multiple scenarios ---------------------------

## Only one scenario, section D works as expected ---------------------------

## Empty range change matrix values (should not be NA in matrix) ----------

# Troubleshooting -------------------------------------------------------
# Testing a particular set of files
shinyOptions("file_dir" = "misc/")
ccvi_app2()

shinyOptions("file_dir" = ".")
ccvi_app2()
