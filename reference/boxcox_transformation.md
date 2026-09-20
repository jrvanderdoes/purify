# Box-Cox Transformation

Compute the Box-Cox transformation for improvement of the Normality and
residual assumptions.

## Usage

``` r
boxcox_transformation(data, lambdas = seq(-3, 3, 1/10))
```

## Arguments

- data:

  Data.frame with the first column the values and the second column the
  group names

- lambdas:

  Numeric values for potential lamba value

## Value

List with shift data, lambda parameter, and shift amount to ensure no
negative values

## Examples

``` r
data <- data.frame(
  "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
  "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
)
boxcox_transformation(data)

#> $data
#>       value group
#> 1  53.11016     A
#> 2  40.44976     A
#> 3  30.75590     A
#> 4  27.67300     A
#> 5  13.11800     A
#> 6  36.98701     A
#> 7   0.00000     A
#> 8  17.68290     A
#> 9  25.79883     A
#> 10 14.97023     A
#> 11 25.65299     A
#> 12 36.13723     A
#> 13 35.88139     A
#> 14 19.18063     A
#> 15 23.00616     B
#> 16 32.21296     B
#> 17 17.41573     B
#> 18 26.39353     B
#> 19 29.28876     B
#> 20 17.77505     B
#> 21 31.70966     C
#> 22 28.47461     C
#> 23 26.37005     C
#> 24 28.39475     C
#> 25 21.31475     C
#> 26 29.22975     C
#> 27 30.29652     C
#> 28 30.47519     C
#> 29 37.04160     C
#> 30 44.12646     C
#> 31 33.21474     C
#> 32 23.82691     C
#> 33 37.32536     C
#> 34 31.25594     C
#> 35 40.18460     C
#> 36 56.15813     C
#> 37 38.84594     C
#> 38 21.81036     C
#> 39 37.24272     C
#> 40 45.06042     C
#> 
#> $lambda
#> [1] 2.030303
#> 
#> $shift
#> [1] -5.224669
#> 
```
