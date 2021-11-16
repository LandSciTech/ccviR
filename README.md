
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ccviR

<!-- badges: start -->
<!-- badges: end -->

*This application/package is under active development*

The ccviR app is an interactive application designed to offer a
user-friendly and simple interface for using the [NatureServe Climate
Change Vulnerability
Index](https://www.natureserve.org/conservation-tools/climate-change-vulnerability-index)
to identify species that are vulnerable to climate change. Effective use
of the NatureServe CCVI tool requires some GIS analysis followed by
entering the results of that analysis and answers to vulnerability
questions into an Excel spreadsheet which then calculates the
corresponding index. With this app we combine these two steps into one,
using R to do the spatial analysis and calculate the index based on user
inputs entered through a Shiny application.

The ccviR app uses the ccviR package to calculate the NatureServe CCVI
index based on user inputs but users familiar with R could use the ccviR
package directly to investigate different aspects of the NatureServe
CCVI algorithm.

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
install.packages(c(‘dplyr', 'raster', 'sf', 'purrr', 'tidyr', 'shiny', 'stringr',
                    'units', 'exactextractr', 'shinyFiles', 'shinyjs', 'tmap', 
                    'ggplot2', 'shinycssloaders', 'R.utils', 'fs', 'plotly',
                    'shinyFeedback', 'rgdal’))

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

See `vignette("app_vignette")` for a more detailed description of how to
use the app.