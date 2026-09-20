# Two Group Tests for Mean Differences

Compute and return information on tests for two group differences in the
data.

## Usage

``` r
two_group_tests(
  data,
  tests = c("t", "wilcox", "bayes", "factor"),
  alpha = 0.05
)
```

## Arguments

- data:

  Data.frame with the first column the values and the second column the
  group names

- tests:

  Vector of strings, or a string, indicating the tests to check. Options
  include 't', 'wilcox', 'bayes', and 'factor'

- alpha:

  Significance for confidence intervals, defaults to 0.05

## Value

A list of two group difference statistics and related information

## Details

Tests require independent data unless otherwise specified. *t*: t-test
and a Welch modified t-test for uneven variances *wilcox*: A Wilcoxon
test *bayes*: Bayesian t-test and a Welch modified Bayesian t-test for
uneven variances *factor*: Bayesian factor approach to a t-test

## See also

[`group_tests()`](https://jrvanderdoes.github.io/purify/reference/group_tests.md),
[`stats::t.test()`](https://rdrr.io/r/stats/t.test.html),
[`stats::wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html),
[`Bolstad::bayes.t.test()`](https://rdrr.io/pkg/Bolstad/man/bayes.t.test.html),
[`BayesFactor::ttestBF()`](https://rdrr.io/pkg/BayesFactor/man/ttestBF.html)

## Examples

``` r
data <- data.frame(
  "value" = c(rnorm(14, sd = 2), rnorm(10, mean = 2)),
  "group" = c(rep("A", 14), rep("B", 10))
)
two_group_tests(data)
#> $ttest_student
#> $ttest_student$pvalue
#> [1] 0.05028172
#> 
#> $ttest_student$means
#> mean in group A mean in group B 
#>      0.02779983      1.46492186 
#> 
#> $ttest_student$interval
#> [1] -2.876170823  0.001926754
#> attr(,"conf.level")
#> [1] 0.95
#> 
#> 
#> $ttest_welch
#> $ttest_welch$pvalue
#> [1] 0.03001478
#> 
#> $ttest_welch$means
#> mean in group A mean in group B 
#>      0.02779983      1.46492186 
#> 
#> $ttest_welch$interval
#> [1] -2.7184615 -0.1557825
#> attr(,"conf.level")
#> [1] 0.95
#> 
#> 
#> $wilcox
#> $wilcox$pvalue
#> [1] 0.01553596
#> 
#> 
#> $bayes
#> $bayes$pvalue
#> [1] 0.05028172
#> 
#> $bayes$means
#> mean in group A mean in group B 
#>      0.02779983      1.46492186 
#> 
#> $bayes$interval
#> [1] -2.876170823  0.001926754
#> attr(,"conf.level")
#> [1] 0.95
#> 
#> 
#> $bayes_welch
#> $bayes_welch$pvalue
#> [1] 0.02838053
#> 
#> $bayes_welch$means
#> mean in group A mean in group B 
#>      0.02662059      1.49945461 
#> 
#> $bayes_welch$interval
#> [1] -2.744668 -0.172878
#> attr(,"conf.level")
#> [1] 0.95
#> 
#> 
#> $bayes_factor
#> [1] 0.5017802
#> 
```
