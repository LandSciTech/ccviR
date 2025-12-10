# Launch the CCVI app

Launch the ccviR app to calculate the NatureServe Climate Change
Vulnerability Index. See `vignette("app_vignette", package = "ccviR")`
for details on how to use the app.

## Usage

``` r
run_ccvi_app(
  file_dir = getwd(),
  launch.browser = TRUE,
  port = getOption("shiny.port"),
  test.mode = FALSE
)
```

## Arguments

- file_dir:

  The directory to locate files from or "demo" to use the demo data
  included in the package.

- launch.browser:

  logical. Run CCVI app in browser?

- port:

  If launch.browser is FALSE, specify port to run CCVI app.

- test.mode:

  Should the app be launched using shiny test.mode. Only set to TRUE for
  debugging.

## Value

A shiny app.

## Examples

``` r
if (FALSE) { # interactive()
 run_ccvi_app("demo")
 run_ccvi_app(file_dir = "../")
}
```
