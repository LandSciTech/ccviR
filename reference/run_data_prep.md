# Launch the data preparation app

Launch the data preparation app for the ccviR package. See
[`vignette("data_prep_vignette", package = "ccviR")`](https://landscitech.github.io/ccviR/articles/data_prep_vignette.md)
for details on how to use the app.

## Usage

``` r
run_data_prep(
  file_dir = getwd(),
  launch.browser = TRUE,
  port = getOption("shiny.port")
)
```

## Arguments

- file_dir:

  The directory to locate files from or "demo" to use the demo data
  included in the package.

- launch.browser:

  logical. Run app in browser?

- port:

  If launch.browser is FALSE, specify port to run CCVI app.

## Value

A shiny app.

## Examples

``` r
if (FALSE) { # interactive()
 run_data_prep("demo")
}
```
