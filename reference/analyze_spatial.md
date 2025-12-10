# Run the spatial analysis

Run the required spatial analysis to create the `spat_df` input for
[`calc_vulnerability`](https://landscitech.github.io/ccviR/reference/calc_vulnerability.md)
and clip the range polygon to the appropriate scales.

## Usage

``` r
analyze_spatial(
  range_poly,
  scale_poly,
  clim_vars_lst,
  non_breed_poly = NULL,
  ptn_poly = NULL,
  hs_rast = NULL,
  hs_rcl = NULL,
  protected_poly = NULL,
  gain_mod = 1,
  scenario_names = "Scenario 1",
  quiet = FALSE
)
```

## Arguments

- range_poly:

  an sf polygon object giving the species range.

- scale_poly:

  an sf polygon object giving the area of the assessment

- clim_vars_lst:

  a list of climate data, the result of
  [`get_clim_vars`](https://landscitech.github.io/ccviR/reference/get_clim_vars.md)

- non_breed_poly:

  Optional. An sf polygon object giving the species range in the
  non-breeding season.

- ptn_poly:

  Optional. An sf polygon object giving the locations that are
  considered part of the physiological thermal niche (See NatureServe
  Guidelines for definition).

- hs_rast:

  Optional. A SpatRaster object with results from a model of the change
  in the species' range caused by climate change. To supply different
  results for each scenario use a raster with multiple layers and ensure
  that the order of the layers matches the order of `scenario_names`.

- hs_rcl:

  a matrix used to classify `hs_rast` into 0: not suitable, 1: lost, 2:
  maintained, 3: gained. See
  [`rng_chg_mat()`](https://landscitech.github.io/ccviR/reference/rng_chg_mat.md)
  for a helper function. See
  [`classify`](https://rspatial.github.io/terra/reference/classify.html)
  for details on the matrix format.

- protected_poly:

  Optional. A sf polygon object with protected areas.

- gain_mod:

  a number between 0 and 1 that can be used to down-weight gains in the
  modeled range change under climate change

- scenario_names:

  character vector with names that identify multiple future climate
  scenarios.

- quiet:

  Logical. Whether to suppress progress messages.

## Value

a list with elements: `spat_table` the results of the spatial analysis,
`range_poly_assess` the range polygon clipped to the assessment area,
and `range_poly_clim` the range polygon clipped to the extent of the
climate data.

`spat_table` contains the following columns:

- scenario_name:

  Name identifying the scenario

- MAT\_#:

  The percentage of the species' range that is exposed to each class of
  change in mean annual temperature between the historical normal and
  predicted climate. Class 1 has the highest exposure and Class 6 the
  lowest

- CMD\_#:

  The percentage of the species' range that is exposed to each class of
  change in climate moisture deficit between the historical normal and
  predicted climate. Class 1 has the highest exposure and Class 6 the
  lowest

- CCEI\_#:

  The percentage of the species' non-breeding range that falls into each
  climate change exposure index class. Class 4 indicates high exposure
  while Class 1 indicates low exposure

- perc_non_breed_not_over_ccei:

  The precentage of the non-breeding range that does not overlap with
  the CCEI raster data

- HTN\_#:

  The percentage of the species' range that is exposed to each class of
  variation between the historical coldest and warmest monts. Class 1
  has the smallest variation and Class 4 is the largest

- PTN:

  The percentage of the species' range that falls into cool or cold
  environments that may be lost or reduced in the assessment area as a
  result of climate change

- MAP_max/min:

  The maximum and minimum historical mean annual precipitation in the
  species' range

- range_change:

  The projected decrease in range size as a percentage of current range
  size. Negative numbers indicate an increase in range size

- range_overlap:

  The percentage of the current range that is projected to remain in the
  future range.

- range_size:

  The area of the species' range in m2

## Details

The range polygon will be clipped to the area overlapping the
`scale_poly` and also to the area overlapping the extent of the climate
data polygon. The range within the assessment area is used to calculate
all results except the historical thermal and hydrological niches for
which the range within the extent of the climate data is used.

The projections used will be that of the Mean Annual Temperature (in
`clim_vars_lst`), the CCEI (in `clim_vars_lst`, if used), and the
predicted changes in breeding range (`hs_rasts`; if used). Therefore
these spatial data files will ideally be projected with a projection
appropriate for preserving area in calculations (Equal Area projections,
for example).

## Examples

``` r
# Setup
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
base_pth <- system.file("extdata", package = "ccviR")
scn_nms <- c("RCP 4.5", "RCP 8.5") # scenario names

clim_vars <- get_clim_vars(file.path(base_pth, "clim_files/processed"),
                           scenario_names = scn_nms)

spat_res <- analyze_spatial(
  range_poly = sf::read_sf(file.path(base_pth, "rng_poly.shp"), agr = "constant"),
  scale_poly = sf::read_sf(file.path(base_pth, "assess_poly.shp"), agr = "constant"),
  clim_vars_lst = clim_vars,
  hs_rast = terra::rast(c(file.path(base_pth, "rng_chg_45.tif"),
                          file.path(base_pth, "rng_chg_85.tif"))),
  hs_rcl = rng_chg_mat(-1, 0, 1),
  scenario_names = scn_nms
)
#> Checking files
#> Preparing polygon 'Climate Data Extext'
#> Preparing polygon 'Assessment Area'
#> Preparing polygon 'Range'
#> Clipping 'Range' to 'Climate Data Extent'
#> Clipping 'Range' to 'Assessment Area'
#> Assessing local climate exposure
#> Assessing thermal & hydrological niches
#> Assessing modelled range response to climate change
#> Finalizing outputs

# With only one range change scenario (suboptimal)

spat_res <- analyze_spatial(
  range_poly = sf::read_sf(file.path(base_pth, "rng_poly.shp"), agr = "constant"),
  scale_poly = sf::read_sf(file.path(base_pth, "assess_poly.shp"), agr = "constant"),
  clim_vars_lst = clim_vars,
  hs_rast = terra::rast(file.path(base_pth, "rng_chg_45.tif")),
  hs_rcl = rng_chg_mat(-1, 0, 1),
  scenario_names = scn_nms
)
#> Checking files
#> Preparing polygon 'Climate Data Extext'
#> Preparing polygon 'Assessment Area'
#> Preparing polygon 'Range'
#> Clipping 'Range' to 'Climate Data Extent'
#> Clipping 'Range' to 'Assessment Area'
#> Assessing local climate exposure
#> Assessing thermal & hydrological niches
#> Assessing modelled range response to climate change
#> Finalizing outputs

# With protected areas
spat_res <- analyze_spatial(
  range_poly = sf::read_sf(file.path(base_pth, "rng_poly.shp"), agr = "constant"),
  scale_poly = sf::read_sf(file.path(base_pth, "assess_poly.shp"), agr = "constant"),
  protected_poly = sf::read_sf(file.path(base_pth, "protected_areas.shp")),
  clim_vars_lst = clim_vars,
  hs_rast = terra::rast(c(file.path(base_pth, "rng_chg_45.tif"),
                          file.path(base_pth, "rng_chg_85.tif"))),
  hs_rcl = rng_chg_mat(-1, 0, 1),
  scenario_names = scn_nms
)
#> Checking files
#> Preparing polygon 'Climate Data Extext'
#> Preparing polygon 'Assessment Area'
#> Preparing polygon 'Range'
#> Clipping 'Range' to 'Climate Data Extent'
#> Clipping 'Range' to 'Assessment Area'
#> Assessing local climate exposure
#> Assessing thermal & hydrological niches
#> Assessing modelled range response to climate change
#> Preparing polygon 'Protected Areas'
#> Clipping 'Protected Areas' to 'Assessment Area'
#> Transforming polygon 'Protected Areas'
#> Finalizing outputs

# With CCEI (by supplying `non_breed.shp` spatial example)
spat_res <- analyze_spatial(
  range_poly = read_sf(file.path(base_pth, "rng_poly.shp"), agr = "constant"),
  scale_poly = read_sf(file.path(base_pth, "assess_poly.shp"), agr = "constant"),
  protected_poly = read_sf(file.path(base_pth, "protected_areas.shp")),
  non_breed_poly = read_sf(file.path(base_pth, "non_breed.shp")),
  clim_vars_lst = clim_vars,
  hs_rast = terra::rast(c(file.path(base_pth, "rng_chg_45.tif"),
                          file.path(base_pth, "rng_chg_85.tif"))),
  hs_rcl = rng_chg_mat(-1, 0, 1),
  scenario_names = scn_nms
)
#> Checking files
#> Preparing polygon 'Climate Data Extext'
#> Preparing polygon 'Assessment Area'
#> Preparing polygon 'Range'
#> Preparing polygon 'non-breeding range'
#> Transforming polygon 'non-breeding range'
#> Clipping 'Range' to 'Climate Data Extent'
#> Clipping 'Range' to 'Assessment Area'
#> Assessing local climate exposure
#> Assessing thermal & hydrological niches
#> Assessing modelled range response to climate change
#> Preparing polygon 'Protected Areas'
#> Clipping 'Protected Areas' to 'Assessment Area'
#> Transforming polygon 'Protected Areas'
#> Finalizing outputs
```
