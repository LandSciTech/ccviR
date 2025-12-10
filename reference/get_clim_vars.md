# Load climate variables

Load climate variables and store them in a list that can be used with
[`analyze_spatial`](https://landscitech.github.io/ccviR/reference/analyze_spatial.md).
The climate variables should first be prepared using
[`prep_clim_data`](https://landscitech.github.io/ccviR/reference/prep_clim_data.md).

## Usage

``` r
get_clim_vars(root_pth, scenario_names = "scn1", quiet = FALSE)
```

## Arguments

- root_pth:

  A folder location where all the climate data is stored. The names must
  match one of
  `c("MAT.*tif", "CMD.*tif", "clim_poly.*shp", "MAP.*tif", "ccei.*tif|CCEI.*tif","MWMT.*tif|HTN.*tif")`
  and the first three are required.

- scenario_names:

  character vector with names that identify multiple future climate
  scenarios. If this is supplied the raster file must include the
  scenario name as a suffix to the pattern mentioned above eg. if there
  are two MAT files "MAT_RCP 4.5.tif" and "MAT_RCP 8.5.tif" the scenario
  names should be "RCP 4.5" and "RCP 8.5". This will happen
  automatically if the scenario name is provided to
  [`prep_clim_data`](https://landscitech.github.io/ccviR/reference/prep_clim_data.md).

- quiet:

  Logical. Whether to suppress progress messages.

## Value

A list of climate variables with names "mat", "cmd", "map", "ccei",
"htn", "clim_poly". If multiple scenarios are used mat, cmd and ccei
will be SpatRasters with one layer per scenario.

## Examples

``` r
pth <- system.file("extdata/clim_files/processed", package = "ccviR")

# scenario names
scn_nms <- c("RCP 4.5", "RCP 8.5")

get_clim_vars(pth, scn_nms)
#> $mat
#> class       : SpatRaster 
#> size        : 44, 43, 2  (nrow, ncol, nlyr)
#> resolution  : 10000, 10000  (x, y)
#> extent      : 1922000, 2352000, 6573000, 7013000  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs 
#> sources     : MAT_reclassRCP_4.5.tif  
#>               MAT_reclassRCP_8.5.tif  
#> names       : RCP_4.5, RCP_8.5 
#> min values  :       1,       6 
#> max values  :       5,       6 
#> 
#> $cmd
#> class       : SpatRaster 
#> size        : 44, 43, 2  (nrow, ncol, nlyr)
#> resolution  : 10000, 10000  (x, y)
#> extent      : 1922000, 2352000, 6573000, 7013000  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs 
#> sources     : CMD_reclassRCP_4.5.tif  
#>               CMD_reclassRCP_8.5.tif  
#> names       : RCP_4.5, RCP_8.5 
#> min values  :       1,       1 
#> max values  :       6,       6 
#> 
#> $map
#> class       : SpatRaster 
#> size        : 44, 43, 1  (nrow, ncol, nlyr)
#> resolution  : 10000, 10000  (x, y)
#> extent      : 1922000, 2352000, 6573000, 7013000  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs 
#> source      : MAP.tif 
#> name        : NB_norm_MAP 
#> min value   :         902 
#> max value   :        1357 
#> 
#> $ccei
#> class       : SpatRaster 
#> size        : 168, 194, 2  (nrow, ncol, nlyr)
#> resolution  : 0.04166667, 0.04166667  (x, y)
#> extent      : -87.875, -79.79167, 24.29167, 31.29167  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> sources     : CCEI_reclassRCP_4.5.tif  
#>               CCEI_reclassRCP_8.5.tif  
#> names       : RCP_4.5, RCP_8.5 
#> min values  :       2,       3 
#> max values  :       3,       3 
#> 
#> $htn
#> class       : SpatRaster 
#> size        : 44, 43, 1  (nrow, ncol, nlyr)
#> resolution  : 10000, 10000  (x, y)
#> extent      : 1922000, 2352000, 6573000, 7013000  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs 
#> source      : MWMT_MCMT_reclass.tif 
#> name        : NB_norm_MWMT 
#> min value   :            1 
#> max value   :            4 
#> 
#> $clim_poly
#> Simple feature collection with 1 feature and 10 fields
#> Attribute-geometry relationships: constant (10)
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 1921779 ymin: 6575395 xmax: 2347091 ymax: 7012589
#> Projected CRS: unnamed
#>   GID_0 NAME_0   GID_1        NAME_1                VARNAME_1 NL_NAME_1
#> 1   CAN Canada CAN.4_1 New Brunswick Nouveau-Brunswick|Acadia      <NA>
#>     TYPE_1 ENGTYPE_1 CC_1 HASC_1                       geometry
#> 1 Province  Province   13  CA.NB MULTIPOLYGON (((2210266 657...
#> 
```
