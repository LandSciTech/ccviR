# Create CCEI raster
#
# Download and process data to create CCEI raster for all of the Americas for
# using an ensemble of models.
#
# Data Source: WorldClim.org
#
# Because we need the future data with models and the historical data at the
# finest temporal resolution (monthly by year, not monthly averaged), we cannot
# use the geodata package (too bad)!
#
# Resolution: Smallest possible (2.5 min, limited by need for monthly historical)
#
# Sources:
#  - Historical monthly weather data (https://www.worldclim.org/data/monthlywth.html)
#  - Future climate data (https://www.worldclim.org/data/cmip6/cmip6climate.html)
#
# Dates:
#  - Historical 1960-1990 (or, precisely, 1960-1989)
#  - Future 2041-2060 (centred on 2050)
#
# Parameters:
#  - Precipitation
#  - Tmin
#  - Tmax

library(httr2)
library(fs)
library(glue)

# Local paths
dir_ccei <- path("misc", "ccei")
dir_hist <- path(dir_ccei, "historical")
dir_future <- path(dir_ccei, "future")
dir_create(c(dir_ccei, dir_hist, dir_future)) # Ensure the misc/ccei folders exists

# Data versions (most recent as of Jan 2025)
v_wc1 <- "2_1"      # WorldClim version 2.1
v_wc2 <- "2.1"      # Because they're not consistent
v_hist <- "4.06" # Downscaled from CRU-TS 4.06
v_future <- "cmip6" # Downscaled from CMIP
res <- "2.5m"

# Base Urls
url_hist <- glue(
  "https://geodata.ucdavis.edu/climate/worldclim/{v_wc1}/hist/cts{v_hist}/{res}/")
url_future <- glue("https://geodata.ucdavis.edu/{v_future}/{res}/")

# Details
params <- c("tmin", "tmax", "prec")

years_hist <- c("1960-1969", "1970-1979", "1980-1989") # 1960-1990
years_future <- "2041-2060"                            # 2050

# See selection discussion: https://github.com/LandSciTech/ccviR/issues/25#issuecomment-2593765878
models <- c("ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5", "CNRM-ESM2-1", "GISS-E2-1-G",
            "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6", "MPI-ESM1-2-HR", "MRI-ESM2-0",
            "UKESM1-0-LL")
scenarios <- c("ssp245", "ssp585")

# Files to download

# Example URLS
# Historical by month - https://geodata.ucdavis.edu/climate/worldclim/2_1/hist/cts4.06/2.5m/wc2.1_cruts4.06_2.5m_tmin_1960-1969.zip
# Future - https://geodata.ucdavis.edu/cmip6/2.5m/MIROC6/ssp585/wc2.1_2.5m_bioc_MIROC6_ssp585_2041-2060.tif

# Get all specific file urls
u_hist <- tidyr::expand_grid(v_wc2, v_hist, res, params, years_hist) %>%
  glue_data("wc{v_wc2}_cruts{v_hist}_{res}_{params}_{years_hist}.zip") %>%
  path(url_hist, .)

u_future <- tidyr::expand_grid(v_wc2, res, models, scenarios, params, years_future) %>%
  glue_data("{models}/{scenarios}/wc{v_wc2}_{res}_{params}_{models}_{scenarios}_{years_future}.tif") %>%
  path(url_future, .)

u <- c(u_hist, u_future)

# Get local file path destinations
f <- c(f_hist <- path(dir_hist, path_file(u_hist)),
       f_future <- path(dir_future, path_file(u_future)))

# Check if already downloaded - Don't re-download if present and complete
u <- u[!is_downloaded(f)]
f <- f[!is_downloaded(f)]


# Download data --------------------------------------------------------------
purrr::walk2(u, f, ~ {
  rlang::inform(.x)
  request(.x) |>
    req_progress() |>
    req_retry(max_tries = 3) |>
    req_perform(path = .y)
})

# Unzip historical data ------------------------------------------------------
purrr::walk(f_hist, ~unzip(.x, exdir = dir_hist, overwrite = TRUE))

# Option to remove zip files after unzipping, but may be better to keep them
# for now...(depends on space requirements). Make TRUE to remove

if(FALSE) unlink(f_hist)

# Citations -----------------------------------

## Monthly historical -----------------------------------
# Bottom of: https://www.worldclim.org/data/monthlywth.html
#
# > You could cite this dataset as follows: CRU-TS 4.06 (Harris et al., 2020)
# > downscaled with WorldClim 2.1 (Fick and Hijmans, 2017).
# >
# > Fick, S.E. and R.J. Hijmans, 2017. WorldClim 2: new 1km spatial resolution
# > climate surfaces for global land areas. International Journal of Climatology 37
# > (12): 4302-4315.
# >
# > Harris, I., Osborn, T.J., Jones, P.D., Lister, D.H. (2020). Version 4 of the
# > CRU TS monthly high-resolution gridded multivariate climate dataset. Scientific
# > Data 7: 109

# Future -----------------------------------
# See CMIP6 Terms of Use: https://pcmdi.llnl.gov/CMIP6/TermsOfUse/TermsOfUse6-1.html


