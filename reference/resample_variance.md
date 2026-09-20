# Resample Variance

Estimate variance for each group and bootstrap the alpha-confidence
intervals.

## Usage

``` r
resample_variance(data, alphas = 0.05, M = 1000)
```

## Arguments

- data:

  Data.frame with the first column the values and the second column the
  group names

- alphas:

  Vector of significances for confidence intervals, defaults to 0.05

- M:

  Numeric for the number of iterations in the resampling

## Value

Matrix with variance and alpha-confidence intervals for each group

## Examples

``` r
data <- data.frame(
  "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
  "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
)
resample_variance(data, M = 50)
#>              A          B         C
#> vars  3.661153 0.47866705 0.7052855
#> 2.5%  1.454074 0.06371255 0.3221473
#> 97.5% 5.744791 0.81587626 1.0639515
```
