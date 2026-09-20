# Reverse Box-Cox Transformation

Compute the reverse Box-Cox transformation for improvement of the
Normality and residual assumptions.

## Usage

``` r
boxcox_inverse(x, lambda, shift)
```

## Arguments

- x:

  Vector that was modified by the Box-Cox transformation

- lambda:

  Value for the lambda parameter of the Box-Cox transformation

- shift:

  Shift amount from Box-Cox transformation

## Value

Data x transformed to the original scale before the Box-Cox
transformation

## Examples

``` r
data <- data.frame(
  "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
  "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
)
tmp <- boxcox_transformation(data)

boxcox_inverse(tmp$data[, 1], tmp$lambda, tmp$shift)
#>  [1]  0.14006970 -1.27824665 -0.09992980 -0.50296689  0.88959423  5.51083515
#>  [7]  0.09306276  1.15541814  0.23638975 -3.82344098  1.72417296 -0.48647348
#> [13] -0.41217439  0.03835518  0.02956075  0.54982754 -2.27411486  2.68255718
#> [19] -0.36122126  0.21335575  3.07434588  1.33491175  3.11395242  1.75410359
#> [25]  0.82243669  1.02414938  3.06505732  2.13167063  2.48862881  0.30054943
#> [31]  0.52926369  2.28415034  3.33732041  2.23669628  3.31829338  2.52390979
#> [37]  2.60674805  1.89006433  2.17218172  1.90967271
```
