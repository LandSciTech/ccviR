# Prepare README for climate data

Prepare README for climate data

## Usage

``` r
prep_clim_readme(
  scenario_name,
  gcm_ensemble,
  hist_period,
  fut_period,
  emissions_scenario,
  url,
  out_folder,
  brks = NULL,
  brks_mat = NULL,
  brks_cmd = NULL,
  brks_ccei = NULL
)
```

## Arguments

- scenario_name:

  Character. A string identifying the climate change scenario(s) that
  will be used as a suffix for the output files.

- gcm_ensemble:

  Character vector. GCM ensemble names

- hist_period:

  Character vector. Historical periods used.

- fut_period:

  Character vector. Future periods used.

- emissions_scenario:

  Character vector. Emissions scenarios.

- url:

  Character vector. Source link

- out_folder:

  Character. Path where the processed files will be saved.

- brks:

  List. Optionally supply `brks_mat`, `brks_cmd` and `brks_ccei` as a
  list.

- brks_mat, brks_cmd, brks_ccei:

  a matrix containing breaks to use for classifying mat, cmd and ccei
  into 6, 6 and 4 classes, respectively. See
  [`reclassify`](https://rdrr.io/pkg/raster/man/reclassify.html) for
  details on the matrix format. If NULL, the default, the breaks will be
  determined using the median and half the interquartile range

## Value

Nothing, but writes the readme to the `out_folder`

## Examples

``` r
in_folder <- system.file("extdata/clim_files/raw", package = "ccviR")
pth_out <- fs::dir_create("processed_temp")

# Process both scenarios at once with the same set of breaks
brks <- prep_clim_data_multi(
  mat_norm = file.path(in_folder, "NB_norm_MAT.tif"),
  mat_fut = file.path(in_folder, c("NB_RCP.4.5_MAT.tif", "NB_RCP.8.5_MAT.tif")),
  cmd_norm = file.path(in_folder, "NB_norm_CMD.tif"),
  cmd_fut = file.path(in_folder, c("NB_RCP.4.5_CMD.tif", "NB_RCP.8.5_CMD.tif")),
  map = file.path(in_folder, "NB_norm_MAP.tif"),
  mwmt = file.path(in_folder, "NB_norm_MWMT.tif"),
  mcmt = file.path(in_folder, "NB_norm_MCMT.tif"),
  out_folder = pth_out,
  clim_poly = file.path(system.file("extdata", package = "ccviR"),
                        "assess_poly.shp"),
  scenario_name = c("RCP 4.5", "RCP 8.5"))
#> Preparing Scenario 1
#> Processing MAT
#> Processing CMD
#> Processing MAP
#> Processing MWMT and MCMT
#> Finished processing
#> Preparing Scenario 2
#> Processing MAT
#> Processing CMD
#> Finished processing

# Add README
prep_clim_readme(
  scenario_name = c("RCP 4.5", "RCP 8.5"),
  gcm_ensemble = "AdaptWest 15 CMIP5 AOGCM Ensemble",
  hist_period = "1961-1990",
  fut_period = "2050s",
  emissions_scenario = c("RCP 4.5", "RCP 8.5"),
  url = "https://adaptwest.databasin.org/pages/adaptwest-climatena-cmip5/",
  out_folder = pth_out,
  brks = brks)

# Clean up
fs::dir_delete(pth_out)
```
