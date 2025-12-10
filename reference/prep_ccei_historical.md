# Prepare Historical Data for Climate Change Exposure Index

Calculate CMD and Tmean from monthly data, calculate annual values (see
`ccei_annual()`). Combine and calculate mean and interannual standard
deviations for each raster cell over the entire historical record.

## Usage

``` r
prep_ccei_historical(path_ccei = "misc/ccei", overwrite = TRUE, quiet = FALSE)
```

## Arguments

- path_ccei:

  Character. Path to pre-downloaded climate data to be used to calculate
  CCEI. Expects two folders `historical` and `future`.

- overwrite:

  Logical. Whether to overwrite existing file(s)

- quiet:

  Logical. Whether to suppress progress messages.

## Value

Writes intermediate and final rasters to `path_ccei` in the
"Intermediate" folder. Annual means (`hist_YYYY.tif` and
`hist_groups_VAR.tif`)as well as overall historical averages and
standard deviations `hist_all_vars.tif`.

## Examples

``` r
if (FALSE) { # \dontrun{
  prep_ccei_historical()
} # }
```
