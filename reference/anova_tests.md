# ANOVA (and similar) Tests for Mean/Median Differences

Compute and return information on tests for the homogeneity of data.

## Usage

``` r
anova_tests(data, tests = c("anova", "welch", "bayes", "kruskal", "median"))
```

## Arguments

- data:

  Data.frame with the first column the values and the second column the
  group names

- tests:

  Vector of strings, or a string, indicating the tests to check. Options
  include 'anova', 'welch', 'bayes', 'kruskal', and 'median'

## Value

A list of means, medians, and anova test results

## Details

Tests require independent data unless otherwise specified. *anova*:
Classic ANOVA. Requires normal data with homogeneous variance and
similar group sizes *welch*: ANOVA modification that allows
nonhomogeneous variances and is more robust to varying group sizes
*bayes*: Bayesian approach to group differences *kruskal*: Also called
the Krukal-wallis test. Non-parametric test *median*: Also called
Brown-Mood median test. Non-parametric test with Bonferroni correction

By default, all five tests are run. Use `tests` to request only selected
tests. Running multiple tests does not replace choosing a test based on
the study design and assumptions, and the returned p-values are not a
single multiplicity-adjusted decision across all methods.

## See also

[`stats::aov()`](https://rdrr.io/r/stats/aov.html),
[`stats::oneway.test()`](https://rdrr.io/r/stats/oneway.test.html),
[`BayesFactor::anovaBF()`](https://rdrr.io/pkg/BayesFactor/man/anovaBF.html),
[`stats::kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html),
[`PMCMRplus::medianTest()`](https://rdrr.io/pkg/PMCMRplus/man/medianTest.html)

## Examples

``` r
data <- data.frame(
  "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
  "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
)
anova_tests(data, tests = c("anova", "welch", "kruskal", "median"))
#> $means
#>          A          B          C 
#> -0.5577610 -0.3827129  2.0143038 
#> 
#> $medians
#>          A          B          C 
#> -0.4915249 -0.2873072  2.0480439 
#> 
#> $anova
#> [1] 0.000125744
#> 
#> $welch
#> [1] 0.0001171815
#> 
#> $kruskal
#> [1] 0.000219581
#> 
#> $median
#> [1] 0.0003760779
#> 
```
