# Calculate Annual Climate Moisture Deficit and Mean Temperature

Calculate Annual CMD and Mean Temperature in preparation for creating
the Climate Exposure Index Raster.

## Usage

``` r
ccei_values(rasts, out, aggregate = FALSE, overwrite = TRUE, quiet = FALSE)
```

## Arguments

- rasts:

  Data.frame of rasters from
  [`prep_ccei_historical()`](https://landscitech.github.io/ccviR/reference/prep_ccei_historical.md)
  or
  [`prep_ccei_future()`](https://landscitech.github.io/ccviR/reference/prep_ccei_future.md)

- out:

  Character. Out folder/file preface

- aggregate:

  Logical. Whether tmean and cmd should be aggregated.

- overwrite:

  Logical. Whether to overwrite existing file(s)

- quiet:

  Logical. Whether to suppress progress messages.

## Value

Annual CMD raster tif and Annual Mean Temperature raster tif saved to an
'intermediate' folder in the `path_ccei`. If `aggregate == TRUE`, then a
final raster with mean and standard deviations calculated.

## Details

CMD = Sum of Monthly CMD (difference between Eref and monthly
precipitation)

Mean Temperature = Mean of monthly average temperature (Midpoint:
(Monthly maximum temp + Monthly minimum temp) / 2)

## Examples

``` r
if (FALSE) { # \dontrun{
  ccei_values()
} # }
```
