# Prepare ccviR for release!!!


# Update internal data sets --------------------
source("data-raw/maps.R") # North American Context and example non-breeding
source("data-raw/lookup_tbls.R")

# Update/Create Large Supplemental data sets as needed ---------------
# source("data-raw/ccei.R")  # Calculate CCEI
# source("data-raw/data_climate_pkg2.R") / source("data-raw/data_climate_pkg.R") # Prep climate and re-scale CCEI
# source("data-raw/protected.R") # Calculate protected areass

# Update/Create Demo or testing datasets
# source("data-raw/data_test.R")      # General test data
# source("data-raw/data_ccei_mini.R") # CCEI data for tests and exploring large values
# source("data-raw/data_demo.R")      # Sub data sets (protected areas, CCEI, ranges, etc.)


# Test coverage --------------------------------------------
# Check as if on CI to prevent testing shiny mods
# Note that terra sometimes seems to fail (but not always with the ccei functions... temp dir issues?)
withr::with_envvar(c(CI = "true"), covr::package_coverage(
  clean = FALSE))


# Documentation --------------------------------

# - Update NEWS
file.edit("NEWS.md")

# - Check spelling
dict <- hunspell::dictionary('en_CA')
devtools::spell_check()
spelling::update_wordlist()

# - Update README.Rmd
devtools::build_readme()

# Finalize package version --------------------------------------

# - Edit DESCRIPTION as needed
file.edit("DESCRIPTION")

# Final checks --------------------------------------------------

# - Checks (avoid issues when installed to a temp location)
withr::with_envvar(c(CI = "true"), devtools::check())

# - Run in console - (Only if trying to be very persnickity)
system("cd ..; R CMD build ccviR")
system("cd ..; R CMD check ccviR_0.1.0.tar.gz --as-cran") # Update version

# - Check GH Actions on GitHub

# Update documentation -------------------------------
pkgdown::build_site()
pkgdown::build_article("app_vignette")

# Release! ------------------------------------------------------

# - Merge dev into master/main
# - PULL updates to master/main
# - Actually release it! Create signed release on GitHub
usethis::use_github_release()

# Get ready for next cycle --------------------------------------

# - Create sandbox/dev branch
# - Change API in data-raw/data_creation.R to sandbox
# - Add dev components to version in DESCRIPTION and NEWS.md
