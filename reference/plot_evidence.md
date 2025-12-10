# Plot evidence by type

Plot evidence by type

## Usage

``` r
plot_evidence(score_df, base_size = 14)
```

## Arguments

- score_df:

  Data frame of codes, evidence and scores for questions.

- base_size:

  Numeric. Font base size passed.

## Value

ggplot2 figure

## Examples

``` r
e <- data.frame(
  score = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE),
  Code = c("B1", "B2a", "B2b", "B3", "C1", "C2ai", "C2aii"),
  evidence = c("", "Other", "Expert Opinion",
               "Expert Opinion, Literature", "",
               "Spatial Analysis", "Spatial Analysis - ccviR"))
plot_evidence(e)

plot_evidence(e[-5,])


plot_evidence(data.frame(score = FALSE, Code = "B1", evidence = "Other"))
```
