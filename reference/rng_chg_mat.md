# Create a range change matrix

Creates a matrix defining the range change scores for a range change
raster. This matrix is required for the `hs_rcl` argument of
[`analyze_spatial()`](https://landscitech.github.io/ccviR/reference/analyze_spatial.md).
The matrix indicates how the range change matrix is scored; which cell
values mean the range was lost, maintained, or gained. This is then used
by
[`analyze_spatial()`](https://landscitech.github.io/ccviR/reference/analyze_spatial.md)
to re-calibrate the range change raster to values of 0-3 indicating NA,
lost, maintained, or gained.

## Usage

``` r
rng_chg_mat(lost, maintained, gained, not_suitable = NA)
```

## Arguments

- lost:

  Numeric vector. Two values representing the lowest and highest values
  indicating range lost.

- maintained:

  Numeric vector. Two values representing the lowest and highest values
  indicating range maintained.

- gained:

  Numeric vector. Two values representing the lowest and highest values
  indicating range gained.

- not_suitable:

  Numeric vector. Two values representing the lowest and highest values
  indicating range not suitable.

## Value

A matrix of scores for use in
[`analyze_spatial()`](https://landscitech.github.io/ccviR/reference/analyze_spatial.md)

## Examples

``` r
rng_chg_mat(lost = -1,
            maintained = 0,
            gained = 1,
            not_suitable = NA)
#>      [,1] [,2] [,3]
#> [1,]   -1   -1    1
#> [2,]    0    0    2
#> [3,]    1    1    3
#> [4,]   NA   NA    0

rng_chg_mat(lost = 1,
            maintained = c(2, 6),
            gained = 7,
            not_suitable = NA)
#>      [,1] [,2] [,3]
#> [1,]    1    1    1
#> [2,]    2    6    2
#> [3,]    7    7    3
#> [4,]   NA   NA    0
```
