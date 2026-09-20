# Normality Tests

Compute and return information on tests for the normality of data.

## Usage

``` r
normality_tests(
  x,
  tests = c("shapiro", "ks", "ad", "cvm", "lilliefors", "pearson", "sf")
)
```

## Arguments

- x:

  Vector of numerics to check normality

- tests:

  Vector of strings, or a string, indicating the tests to check. Options
  include 'shapiro', 'ks', 'ad', 'cvm', 'lilliefors', 'pearson', and
  'sf'

## Value

A list of qqplot and normality test results

## Details

Tests require independent data unless otherwise specified. *shapiro*:
Shapiro-Wilk test *ks*: Kolmogorov-Smirnov test against the standard
normal distribution \\N(0, 1)\\ *ad*: Anderson-Darling test *cvm*:
Cramer-von Mises test *lilliefors*: lilliefors test *pearson*: Pearson
chi-square test *sf*: Shapiro-Francia test

The input must be a finite numeric vector with at least three
observations. The Shapiro-Wilk test additionally supports at most 5000
observations.

## See also

[`stats::shapiro.test()`](https://rdrr.io/r/stats/shapiro.test.html),
[`stats::ks.test()`](https://rdrr.io/r/stats/ks.test.html),
[`nortest::ad.test()`](https://rdrr.io/pkg/nortest/man/ad.test.html),
[`nortest::cvm.test()`](https://rdrr.io/pkg/nortest/man/cvm.test.html),
[`nortest::lillie.test()`](https://rdrr.io/pkg/nortest/man/lillie.test.html),
[`nortest::pearson.test()`](https://rdrr.io/pkg/nortest/man/pearson.test.html),
[`nortest::sf.test()`](https://rdrr.io/pkg/nortest/man/sf.test.html)

## Examples

``` r
x <- c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2))
normality_tests(x)
#> $qqplot

#> 
#> $shapiro
#> [1] 0.2246418
#> 
#> $ks
#> [1] 5.715684e-05
#> 
#> $ad
#> [1] 0.1258442
#> 
#> $cvm
#> [1] 0.1710959
#> 
#> $lilliefors
#> [1] 0.2466502
#> 
#> $pearson
#> [1] 0.5438131
#> 
#> $sf
#> [1] 0.1231092
#> 
```
