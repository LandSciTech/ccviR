# Prepare Climate Change Exposure Index raster

Using the Standardized Euclidean Distance method from Williams et al.,
2007 and NatureServe v3.02.1 2016.

## Usage

``` r
prep_ccei(path_ccei = "misc/ccei", overwrite = TRUE, quiet = FALSE)
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

Creates rasters and saves them to `path_ccei`. The final CCEI rasters
are saved as `ccei_ssp245.tif` and `ccei_ssp585.tif`.

## References

J.W. Williams, S.T. Jackson, J.E. Kutzbach, Projected distributions of
novel and disappearing climates by 2100 AD, Proc. Natl. Acad. Sci.
U.S.A. 104 (14) 5738-5742, https://doi.org/10.1073/pnas.0606292104
(2007).

B.E. Young, E. Byers, G. Hammerson, A. Frances, L. Oliver, A. Treher,
Guidelines for Using the NatureServe Climate Change Vulnerability Index.
Release 3.02.
https://www.natureserve.org/sites/default/files/guidelines_natureserveclimatechangevulnerabilityindex_r3.02_1_jun_2016.pdf
(June 1st 2016)

## Examples

``` r
if (FALSE) { # \dontrun{
  prep_ccei()
} # }
```
