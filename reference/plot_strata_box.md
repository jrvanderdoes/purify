# Plot Boxplot for Strata Sizes

Produce a boxplot of strata sizes based on data.

## Usage

``` r
plot_strata_box(data)
```

## Arguments

- data:

  Data.frame of the data, the first column is the strata and the second
  some value for the box plots.

## Value

A ggplot2 object of boxplots for data based on strata.

## Examples

``` r
plot_strata_box(data.frame("B" = rbinom(100, 2, 0.5), "A" = rnorm(100)))
```
