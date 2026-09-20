# Variance Tests for Homogeneity

Compute and return information on tests for the homogeneity of data.

## Usage

``` r
variance_tests(
  data,
  tests = c("ratio", "levene", "bartlett", "fligner"),
  ratio.threshold = 4
)
```

## Arguments

- data:

  Data.frame with the first column the values and the second column the
  group names

- tests:

  Vector of strings, or a string, indicating the tests to check. Options
  include 'ratio', 'levene', 'bartlett', 'fligner', and 'hartley'

- ratio.threshold:

  Numeric threshold for comparision of the variance ratio

## Value

Lists of variances and desired variance tests

## Details

Tests require independent, quantitative data unless otherwise specified.
*Ratio*: Looks at the ratio of largest to smallest variance. Rule of
thumb is that if this ratio is less than 4, no major violations are
observed *Levene*: No other major assumptions *Bartlett*: Requires
normally distributed data and no major outliers *Fligner*: Also called
Fligner-Killeen. Non-parametric test *Hartley*: Requires normal
distribution and equal number of observations.

By default, ratio, Levene, Bartlett, and Fligner tests are run. Use
`tests` to select a subset. The tests have different assumptions, so
results should be interpreted in light of the study design rather than
by selecting the smallest p-value.

## See also

[`car::leveneTest()`](https://rdrr.io/pkg/car/man/leveneTest.html),
[`stats::bartlett.test()`](https://rdrr.io/r/stats/bartlett.test.html),
[`stats::fligner.test()`](https://rdrr.io/r/stats/fligner.test.html),
[`PMCMRplus::hartleyTest()`](https://rdrr.io/pkg/PMCMRplus/man/hartleyTest.html)

## Examples

``` r
data <- data.frame(
  "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
  "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
)
variance_tests(data)
#> $var
#>         A         B         C 
#> 5.2883160 0.1656275 1.1667309 
#> 
#> $ratio
#> [1] 31.92896
#> 
#> $ratio.threshold
#> [1] 4
#> 
#> $levene_pvalue
#> [1] 0.01265854
#> 
#> $bartlett_pvalue
#> [1] 0.000189387
#> 
#> $fligner_pvalue
#> [1] 0.03930279
#> 
```
