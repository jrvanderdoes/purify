# Summarize Resampled Data

Summarize the results of resampled data. Such data is returned from
[`resample()`](https://jrvanderdoes.github.io/purify/reference/resample.md),
typically when `fn()` is given.

## Usage

``` r
summarize_resample(data, alpha = 0.05)
```

## Arguments

- data:

  Data.frame of resampled data from
  [`resample()`](https://jrvanderdoes.github.io/purify/reference/resample.md).

- alpha:

  Significance level for confidence intervals.

## Value

A dataframe. It has rows for each column in data. The columns are:

- estimates: Mean of the parameter resampled estimates.

- sd: Standard deviations of the resampled estimates.

- lower: Lower confidence interval from empirical quantiles of the
  resampled data.

- upper: Upper confidence interval from empirical quantiles of the
  resampled data.

## Examples

``` r
n <- 50
data <- data.frame(
  output = rnorm(n),
  predictor1 = rnorm(n),
  predictor2 = rnorm(n)
)

fn <- function(data) {
  coef(lm(output ~ ., data = data))
}
# Warning, M is below recommendation for example speed
resample_data <- resample(data = data, fn = fn, M = 10, ignore.columns = "output")
results <- summarize_resample(resample_data)
```
