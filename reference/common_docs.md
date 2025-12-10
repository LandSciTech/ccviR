# Documentation common to different functions

Documentation common to different functions

## Arguments

- path_ccei:

  Character. Path to pre-downloaded climate data to be used to calculate
  CCEI. Expects two folders `historical` and `future`.

- overwrite:

  Logical. Whether to overwrite existing file(s)

- quiet:

  Logical. Whether to suppress progress messages.

- scenario_name:

  Character. A string identifying the climate change scenario(s) that
  will be used as a suffix for the output files.

- out_folder:

  Character. Path where the processed files will be saved.

- brks:

  List. Optionally supply `brks_mat`, `brks_cmd` and `brks_ccei` as a
  list.

- brks_mat, brks_cmd, brks_ccei:

  a matrix containing breaks to use for classifying mat, cmd and ccei
  into 6, 6 and 4 classes, respectively. See
  [`reclassify`](https://rdrr.io/pkg/raster/man/reclassify.html) for
  details on the matrix format. If NULL, the default, the breaks will be
  determined using the median and half the interquartile range

## Details

Use `@inheritParams common_docs` to include the above in any function
documentation with a matching argument (will only include matching args)
