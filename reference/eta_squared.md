# Eta Squared

Compute the Eta squared effect size statistic.

## Usage

``` r
eta_squared(data)
```

## Arguments

- data:

  Data.frame with the first column the values and the second column the
  group names

## Value

Table with eta-squared attached to classic ANOVA decomposition

## References

Navarro, D. J. (2015) Learning statistics with R: A tutorial for
psychology students and other beginners. (Version 0.6) University of New
South Wales. Sydney, Australia

## Examples

``` r
data <- data.frame(
  "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
  "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
)
eta_squared(data)
#>             eta squared Df   Sum Sq   Mean Sq  F value       Pr(>F)
#> group         0.3429468  2 37.97889 18.989443 9.656017 0.0004222887
#> Residuals     0.6570532 37 72.76389  1.966592       NA           NA
```
