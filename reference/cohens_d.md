# Effect Size Statistics

Compute the Cohen's d and Hedges' g effect size statistics.

## Usage

``` r
cohens_d(
  x,
  y = NULL,
  var.type = "unequal",
  hedges.correction = FALSE,
  hedges.approx = TRUE
)
```

## Arguments

- x:

  Vector of values for the first group (if y specified) or a data.frame
  with the first column the values and the second column the group names
  (no y). If using a data.frame, ensure only two groups are given.

- y:

  Vector of values for the second group (if x is a vector), otherwise
  NULL if x is a data.frame.

- var.type:

  String indicating the variance assumption. Options include 'unequal'
  (default), 'pooled', 'x', or 'y'.

- hedges.correction:

  Boolean indicating if Hedges correction should be applied

- hedges.approx:

  Boolean if Hedges correction should be approximated or computed using
  the gamma function

## Value

Numeric value for the effect size

## References

Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences
(2nd ed.). Routledge.

Hedges, Larry & Olkin, Ingram. (1985). Statistical Methods in
Meta-Analysis. 10.2307/1164953.

Theriault, R., (2023). rempsyc: Convenience functions for psychology.
*Journal of Open Source Software*, *8*(87), 5466.

## Examples

``` r
x <- rnorm(10, mean = 1, sd = 1)
y <- rnorm(40, mean = 3, sd = 10)
cohens_d(x, y, var.type = "unequal")
#> [1] 0.1905781
cohens_d(x, y, var.type = "pooled", hedges.correction = TRUE)
#> [1] 0.1475207
```
