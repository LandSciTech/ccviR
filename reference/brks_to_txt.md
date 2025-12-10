# Convert breaks matrix to text that can be stored in readme

Convert breaks matrix to text that can be stored in readme

## Usage

``` r
brks_to_txt(brks)
```

## Arguments

- brks:

  a matrix of breaks with columns start, end and class

## Value

A string with breaks in the format "class: (start - end);"

## Examples

``` r
brks_to_txt(matrix(data = 1:6, nrow = 2, byrow = TRUE))
#> [1] "3: (1 - 2);6: (4 - 5)"
```
