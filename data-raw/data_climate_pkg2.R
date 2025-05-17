# Download and Prepare Climate Data

# Assumes we have ccei data in misc/ folder

# Setup ----------------------------------------------------------------------
library(fs)

scenarios <- c("ssp245", "ssp585")

clim_raw <- path("misc", "climate", "raw")
clim_prep <- path("misc", "climate", "processed") %>%
  dir_create()
ccei <- dir_ls("misc/ccei") %>%
  fs::path_filter(regexp = paste0(paste0(scenarios, "\\.tif"), collapse = "|"))

# Download Climate data ------------------------------------------------------
# TODO: Add steps for climate data prep.


# Process Climate data -------------------------------------------------------

brks <- prep_clim_data_multi(
  mat_norm = path(clim_raw, "NB_norm_MAT.tif"),
  mat_fut = path(clim_raw, c("NB_RCP.4.5_MAT.tif", "NB_RCP.8.5_MAT.tif")),
  cmd_norm = path(clim_raw, "NB_norm_CMD.tif"),
  cmd_fut = path(clim_raw, c("NB_RCP.4.5_CMD.tif", "NB_RCP.8.5_CMD.tif")),
  ccei = ccei,
  map = path(clim_raw, "NB_norm_MAP.tif"),
  mwmt = path(clim_raw, "NB_norm_MWMT.tif"),
  mcmt = path(clim_raw, "NB_norm_MCMT.tif"),
  clim_poly = file.path(system.file("extdata", package = "ccviR"),
                        "assess_poly.shp"),
  out_folder = clim_prep,
  overwrite = TRUE,
  scenario_name = c("RCP 4.5", "RCP 8.5"))

prep_clim_readme(
  scenario_name = c("RCP 4.5", "RCP 8.5"),
  gcm_ensemble = "AdaptWest 15 CMIP5 AOGCM Ensemble",
  hist_period = "1961-1990",
  fut_period = "2050s",
  emissions_scenario = c("RCP 4.5", "RCP 8.5"),
  url = "https://adaptwest.databasin.org/pages/adaptwest-climatena-cmip5/",
  out_folder = clim_prep,
  brks = brks)

# Checks...
t0 <- terra::rast(ccei[1])
t1 <- terra::rast(path(clim_prep, "CCEI_reclassRCP_4.5.tif"))

terra::plot(t0)
terra::plot(t1)

# Upload Climate Data --------------------------------------------------------
# TODO: Consider piggyback?
