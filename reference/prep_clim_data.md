# Prepare climate data

Prepare data from raw to form needed for calculating the index. See the
NatureServe Guidelines for details on how the data is prepared.

## Usage

``` r
prep_clim_data(
  mat_norm,
  mat_fut,
  cmd_norm,
  cmd_fut,
  ccei = NULL,
  map = NULL,
  mwmt = NULL,
  mcmt = NULL,
  clim_poly = NULL,
  in_folder = NULL,
  out_folder,
  reproject = FALSE,
  overwrite = FALSE,
  scenario_name = "",
  brks = NULL,
  brks_mat = NULL,
  brks_cmd = NULL,
  brks_ccei = NULL,
  quiet = FALSE,
  n = c(1, 1)
)
```

## Arguments

- mat_norm, mat_fut, cmd_norm, cmd_fut, ccei, map, mwmt, mcmt,
  clim_poly:

  file paths to find data if in_folder is not given

- in_folder:

  file path where files are stored. Files must be named according to the
  convention described in details

- out_folder:

  Character. Path where the processed files will be saved.

- reproject:

  should the data be re-projected to lat/long? Not recommended.

- overwrite:

  Logical. Whether to overwrite existing file(s)

- scenario_name:

  Character. A string identifying the climate change scenario(s) that
  will be used as a suffix for the output files.

- brks:

  List. Optionally supply `brks_mat`, `brks_cmd` and `brks_ccei` as a
  list.

- brks_mat, brks_cmd, brks_ccei:

  a matrix containing breaks to use for classifying mat, cmd and ccei
  into 6, 6 and 4 classes, respectively. See
  [`reclassify`](https://rdrr.io/pkg/raster/man/reclassify.html) for
  details on the matrix format. If NULL, the default, the breaks will be
  determined using the median and half the interquartile range

- quiet:

  Logical. Whether to suppress progress messages.

- n:

  Numeric vector. Only for use by
  [`prep_clim_data_multi()`](https://landscitech.github.io/ccviR/reference/prep_clim_data_multi.md)
  for progresses messages and to skip unnecessary steps. Do not change.

## Value

Returns a list of matrices with the breaks used to classify mat, cmd and
ccei. This list can be supplied to a future call to `prep_clim_data` in
order to use the same breaks for multiple climate data sets. Processed
data is saved in `out_folder`

## Details

Definition of input data sets and file names required in in_folder:

- mat_norm::

  "MAT" mean annual temperature for the historical normal period

- mat_fut::

  "MAT_2050" mean annual temperature for the future under climate change
  it can be any number eg 2050, 2100

- cmd_norm::

  "CMD" climate moisture deficit for the historical normal period

- cmd_fut::

  "CMD_2050" climate moisture deficit for the future under climate
  change it can be any number eg 2050, 2100

- ccei::

  "CCEI" Climate Change Exposure Index from NatureServe website

- map::

  "MAP" mean annual precipitation for the historical normal period

- mwmt::

  "MWMT" mean warmest month temperature for the historical normal period

- mcmt::

  "MCMT" mean coldest month temperature for the historical normal period

- clim_poly::

  An optional shapefile with a polygon of the range of the climate data.
  It will be created from the climate data if it is missing but it is
  faster to provide it.

Accepted raster file types are ".asc", ".tif", ".nc", ".grd", ".img",
".bil" and ".gpkg" (see
[`spatial_file_raster()`](https://landscitech.github.io/ccviR/reference/spatial_file_raster.md)).

## See also

[`get_clim_vars`](https://landscitech.github.io/ccviR/reference/get_clim_vars.md)
for loading the processed data.

## Examples

``` r
in_folder <- system.file("extdata/clim_files/raw", package = "ccviR")
pth_out <- fs::dir_create("processed_temp")

# use first scenario to set breaks
brks_out <- prep_clim_data(mat_norm = file.path(in_folder, "NB_norm_MAT.tif"),
                           mat_fut = file.path(in_folder, "NB_RCP.4.5_MAT.tif"),
                           cmd_norm = file.path(in_folder, "NB_norm_CMD.tif"),
                           cmd_fut = file.path(in_folder, "NB_RCP.4.5_CMD.tif"),
                           map = file.path(in_folder, "NB_norm_MAP.tif"),
                           mwmt = file.path(in_folder, "NB_norm_MWMT.tif"),
                           mcmt = file.path(in_folder, "NB_norm_MCMT.tif"),
                           out_folder = pth_out,
                           clim_poly = file.path(system.file("extdata", package = "ccviR"),
                                                 "assess_poly.shp"),
                           overwrite = TRUE,
                           scenario_name = "RCP 4.5")
#> Processing MAT
#> Processing CMD
#> Processing MAP
#> Processing MWMT and MCMT
#> Finished processing

prep_clim_data(mat_norm = file.path(in_folder, "NB_norm_MAT.tif"),
               mat_fut = file.path(in_folder, "NB_RCP.8.5_MAT.tif"),
               cmd_norm = file.path(in_folder, "NB_norm_CMD.tif"),
               cmd_fut = file.path(in_folder, "NB_RCP.8.5_CMD.tif"),
               out_folder = pth_out,
               clim_poly = file.path(system.file("extdata", package = "ccviR"),
                                     "assess_poly.shp"),
               overwrite = TRUE,
               scenario_name = "RCP 8.5",
               brks_mat = brks_out$brks_mat, brks_cmd = brks_out$brks_cmd,
               brks_ccei = brks_out$brks_ccei)
#> Processing MAT
#> Processing CMD
#> Finished processing

fs::dir_delete(pth_out)
```
