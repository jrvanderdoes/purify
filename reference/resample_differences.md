# Resampled Mean Differences

Estimate pairwise mean differences and percentile bootstrap confidence
intervals between groups. The returned `se` column is the ordinary
standard error of a difference between two independent means, estimated
from the bootstrap group variances.

## Usage

``` r
resample_differences(data, alpha = 0.05, M = 1000)
```

## Arguments

- data:

  Data.frame with the first column the values and the second column the
  group names

- alpha:

  Significance for confidence intervals, defaults to 0.05

- M:

  Numeric. Number of resample iterations.

## Value

A data frame with one row for each pair of groups. The `diff` column
contains the mean difference (second group minus first group), `lwr` and
`upr` contain the percentile bootstrap confidence limits, and `se`
contains the estimated standard error.

## Examples

``` r
data <- data.frame(
  "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
  "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
)
# Be sure to increase M for real use cases
resample_differences(data, M = 50)
#>           diff        lwr      upr        se
#> B-A -0.3032863 -1.9857053 0.974165 0.7479113
#> C-A  2.5072506  0.9401893 3.809685 0.7595657
#> C-B  2.8105369  2.3198538 3.298522 0.3083827
```
