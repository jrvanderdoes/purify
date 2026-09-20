# Plot Barplot for Strata Sizes

Produce a barplot of strata sizes based on data.

## Usage

``` r
plot_strata_bar(data)
```

## Arguments

- data:

  Vector of data with values of strata.

## Value

A ggplot2 object of barplots for strata counts.

## Examples

``` r
data <- data.frame("A" = rnorm(100), "B" = rbinom(100, 2, 0.5))
plot_strata_bar(data$B)
```
