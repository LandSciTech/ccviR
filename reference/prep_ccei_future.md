# Prepare Future Data for Climate Change Exposure Index

Calculate CMD and Tmean from monthly data averages.

## Usage

``` r
prep_ccei_future(path_ccei = "misc/ccei", overwrite = TRUE, quiet = FALSE)
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

Writes rasters to `path_ccei` in the "Intermediate" folder. Overall
averages for each model/scenario `future_MODEL-SCENARIO.tif`.

## Examples

``` r
if (FALSE) { # \dontrun{
  prep_ccei_future()
} # }
```
