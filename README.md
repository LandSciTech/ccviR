
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ccviR

<!-- badges: start -->
<!-- badges: end -->

*This application/package is under active development*

The ccviR package implements the [NatureServe Climate Change
Vulnerability Index (CCVI) version
3.02](https://www.natureserve.org/conservation-tools/climate-change-vulnerability-index)
in an R package and Shiny App. The package allows all of the geospatial
aspects of calculating the CCVI to be done in R, removing the need for
separate GIS calculations. The app provides an interactive application
designed to offer a user-friendly and simple interface for calculating
the NatureServe CCVI. See [Young et. al
(2012)](https://www.degruyter.com/document/doi/10.7208/9780226074641-007/html)
and [Young et. al. (2015)](https://doi.org/10.1002/wsb.478) for a
detailed description of the index.

## Installation

You can install the development version of ccviR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("see24/ccviR")
```

Or clicking the “Latest Release” in the right-side menu of the GitHub
repository, downloading the .zip file containing the package and
installing it and its dependencies with:

``` r
install.packages(c('dplyr', 'raster', 'sf', 'purrr', 'tidyr', 'shiny', 'stringr',
                    'units', 'exactextractr', 'shinyFiles', 'shinyjs', 'tmap', 
                    'ggplot2', 'shinycssloaders', 'R.utils', 'fs', 'plotly',
                    'rgdal', 'scales', 'shinyvalidate'))

install.packages("path/to/zip/file", repos = NULL)
```

## Launching the app

The code below will open the app in your default browser with an example
data set available.

``` r
library(ccviR)
run_ccvi_app("demo")
```

While the following will open the app with the current working directory
as the default data location.

``` r
run_ccvi_app()
```

## Additional Help

See `vignette("app_vignette", package = "ccviR")` for a more detailed
description of how to use the app and
`vignette("package_vignette", package = "ccviR")` for a tutorial on how
to use the package to calculate the index directly in R.
