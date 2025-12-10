# Make vulnerability table

Make an empty vulnerability factor table that can be filled in with the
appropriate value for each vulnerability factor. The Code corresponds to
the questions in the NatureServe Guidelines and Question is an
abbreviated version of the question. See the NatureServe Guidelines for
detailed instructions on how to score species. Values can be -1:
Unknown, 0: Neutral, 1: Somewhat Increase, 2: Increase, 3: Greatly
Increase.

## Usage

``` r
make_vuln_df(
  sp_nm,
  val1 = -1,
  val2 = NA,
  val3 = NA,
  val4 = NA,
  cave = 0,
  mig = 0,
  use_spatial = TRUE
)
```

## Arguments

- sp_nm:

  Species name

- val1:

  A single number to fill the first column with. The default -1 for
  Unknown should be used in most cases.

- val2, val3, val4:

  Additional values. Use default NA

- cave:

  0 or 1 For whether the species is cave or ground water dependent. See
  Guidelines.

- mig:

  0 or 1 is the species migratory?

- use_spatial:

  if TRUE then values for factors that are calculated in
  `analyze_spatial` will be set to -1 for Unknown so that they do not
  override the results of the spatial analysis

## Value

a data.frame that can be edited and used as input for `vuln_df` in
[`calc_vulnerability`](https://landscitech.github.io/ccviR/reference/calc_vulnerability.md)

## Examples

``` r
make_vuln_df("sfa", cave = 1, mig = 0)
#>    Species  Code
#> 1      sfa    Z2
#> 2      sfa    Z3
#> 3      sfa    B1
#> 4      sfa   B2a
#> 5      sfa   B2b
#> 6      sfa    B3
#> 7      sfa    C1
#> 8      sfa  C2ai
#> 9      sfa C2aii
#> 10     sfa  C2bi
#> 11     sfa C2bii
#> 12     sfa   C2c
#> 13     sfa   C2d
#> 14     sfa    C3
#> 15     sfa   C4a
#> 16     sfa   C4b
#> 17     sfa   C4c
#> 18     sfa   C4d
#> 19     sfa   C4e
#> 20     sfa   C4f
#> 21     sfa   C4g
#> 22     sfa   C5a
#> 23     sfa   C5b
#> 24     sfa   C5c
#> 25     sfa    C6
#> 26     sfa    D1
#> 27     sfa    D2
#> 28     sfa    D3
#> 29     sfa    D4
#>                                                                                  Question
#> 1                              Is the species an obligate of caves or groundwater systems
#> 2                                                                Is the species migratory
#> 3                                                              Exposure to sea level rise
#> 4                                                                        Natural barriers
#> 5                                                                  Anthropogenic barriers
#> 6   Predicted impact of land use changes resulting from human responses to climate change
#> 7                                                                 Dispersal and movements
#> 8                                                                Historical thermal niche
#> 9                                                             Physiological thermal niche
#> 10                                                          Historical hydrological niche
#> 11                                                       Physiological hydrological niche
#> 12    Dependence on a specific disturbance regime likely to be impacted by climate change
#> 13                                    Dependence on ice, ice-edge, or snow-cover habitats
#> 14                   Restriction to uncommon landscape/geological features or derivatives
#> 15                               Dependence on other species to generate required habitat
#> 16                                                     Dietary versatility (animals only)
#> 17                                                   Pollinator versatility (plants only)
#> 18                                    Dependence on other species for propagule dispersal
#> 19                                            Sensitivity to pathogens or natural enemies
#> 20                           Sensitivity to competition from native or non-native species
#> 21                         Forms part of an interspecific interaction not covered by 5a-f
#> 22                                                             Measured genetic variation
#> 23 Occurrence of bottlenecks in recent evolutionary history (use only if 5a is "unknown")
#> 24                 Reproductive system (plants only; use only if c5a and c5b are unknown)
#> 25      Phenological response to changing seasonal temperature and precipitation dynamics
#> 26                                           Documented response to recent climate change
#> 27                               Modeled future (2050) change in population or range size
#> 28                              Overlap of modeled future (2050) range with current range
#> 29                    Occurrence of protected areas in modeled future (2050) distribution
#>    Max_Value is_spatial Value1 Value2 Value3 Value4 comment evidence
#> 1       <NA>       <NA>      1     NA     NA     NA                 
#> 2       <NA>       <NA>      0     NA     NA     NA                 
#> 3          3          0     -1     NA     NA     NA                 
#> 4          3          0     -1     NA     NA     NA                 
#> 5          3          0     -1     NA     NA     NA                 
#> 6          2          0     -1     NA     NA     NA                 
#> 7          3          0     -1     NA     NA     NA                 
#> 8          3          1     -1     NA     NA     NA                 
#> 9          3          1     -1     NA     NA     NA                 
#> 10         3          1     -1     NA     NA     NA                 
#> 11         3          0     -1     NA     NA     NA                 
#> 12         2          0     -1     NA     NA     NA                 
#> 13         3          0     -1     NA     NA     NA                 
#> 14         2          0     -1     NA     NA     NA                 
#> 15         2          0     -1     NA     NA     NA                 
#> 16         2          0     -1     NA     NA     NA                 
#> 17         2          0     -1     NA     NA     NA                 
#> 18         2          0     -1     NA     NA     NA                 
#> 19         2          0     -1     NA     NA     NA                 
#> 20         2          0     -1     NA     NA     NA                 
#> 21         2          0     -1     NA     NA     NA                 
#> 22         2          0     -1     NA     NA     NA                 
#> 23         2          0     -1     NA     NA     NA                 
#> 24         2          0     -1     NA     NA     NA                 
#> 25         2          0     -1     NA     NA     NA                 
#> 26         3          0     -1     NA     NA     NA                 
#> 27         3          1     -1     NA     NA     NA                 
#> 28         3          1     -1     NA     NA     NA                 
#> 29         2          1     -1     NA     NA     NA                 
```
