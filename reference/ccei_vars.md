# Calculate monthly Climate Moisture Deficit and Mean Temperature

Because `climr:::calc_Eref()` and and `climr:::calc_CMD()` are expected
to work on vectors, they don't work with a raster in memory.

## Usage

``` r
ccei_vars(prec_files, tmin_files, tmax_files, clip, quiet = FALSE)
```

## Arguments

- prec_files:

  Character. Files paths to precipitation rasters

- tmin_files:

  Character. Files paths to minimum temperature rasters

- tmax_files:

  Character. Files paths to maximum temperature rasters

- clip:

  SpatExtent. Area which to clip the raster to.

- quiet:

  Logical. Whether to suppress progress messages.

## Value

SpatRaster with `tmean` and `cmd`

## Details

[`terra::app()`](https://rspatial.github.io/terra/reference/app.html) is
very slow as it applies a function to every cell, where as even if we
extract the raster values to memory, being able to apply things in
parallel is much faster.

Therefore we extract raster values calculate the Eref and CMD and then
return as a raster. It's faster if we omit the NAs for these
calculations.
