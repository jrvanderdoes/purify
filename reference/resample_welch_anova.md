# Resample Welch ANOVA

Resample data for significance of groups when running an ANOVA
[`stats::oneway.test()`](https://rdrr.io/r/stats/oneway.test.html)

## Usage

``` r
resample_welch_anova(data, var.equal = FALSE, M = 1000)
```

## Arguments

- data:

  Data.frame with the first column the values and the second column the
  group names

- var.equal:

  Boolean if we can assume equal variance over the groups

- M:

  Numeric for the number of iterations in the resampling

## Value

p-value for the effect of the groups

## Examples

``` r
data <- data.frame(
  "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
  "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
)
resample_welch_anova(data, M = 100)
#> [1] 0
```
