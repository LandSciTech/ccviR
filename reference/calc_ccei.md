# Caculate CCEI from prepared rasters

Caculate CCEI from prepared rasters

## Usage

``` r
calc_ccei(
  path_ccei = "misc/ccei",
  scenario,
  models = NULL,
  out_append = NULL,
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- path_ccei:

  Character. Path to pre-downloaded climate data to be used to calculate
  CCEI. Expects two folders `historical` and `future`.

- scenario:

  Character. Which scenario to calculate CCEI for. Must match scenario
  used in raster file names, e.g., "ssp245" or "ssp585"

- models:

  Character Vector. Subset of models to include (otherwise uses all
  models).

- out_append:

  Character. String to apped to the output file.

- overwrite:

  Logical. Whether to overwrite existing file(s)

- quiet:

  Logical. Whether to suppress progress messages.

## Value

Writes final CCEI rasters (one for each scenario) to `path_ccei`.
`ccei_SCENARIO.tif`.

## Examples

``` r
if (FALSE) { # \dontrun{
calc_ccei(scenario = "ssp245")
calc_ccei(scenario = "ssp585")
calc_ccei(scenario = "ssp245",
          models = c("ACCESS-ESM1-5", "CanESM5"),
          out_append = "test")
} # }
```
