# Resample Distributions

Resample Distributions

## Usage

``` r
resample_distribution(
  data,
  resampled_data = NULL,
  strata = NULL,
  ignore.columns = NULL,
  ...
)
```

## Arguments

- data:

  Data.frame (or vector) to be resampled where the rows are the
  observations and the columns are the variables. Note, all variables
  are permuted not specified in `ignore.columns`.

- resampled_data:

  Data from
  [`resample()`](https://jrvanderdoes.github.io/purify/reference/resample.md)
  or NULL to run
  [`resample()`](https://jrvanderdoes.github.io/purify/reference/resample.md)
  on data

- strata:

  String or numeric. This indicate the column to stratify the data when
  `method` is stratify. This can be the column number or the column
  name. When NULL the data is not stratified. When given, strata are
  sampled separately.

- ignore.columns:

  Name or column numbers to ignore when resampling data. These are not
  permuted. Note that if less/more samples are collected than the
  original, these are permuted separately.

- ...:

  Additional parameters for
  [`resample()`](https://jrvanderdoes.github.io/purify/reference/resample.md)

## Value

ggplot object showing original and resample distribution. Note that if a
function is used, they many not be describing the same thing

## Examples

``` r
resample_distribution(
  data.frame(
    "data" = c(rnorm(100), rnorm(50, mean = 10)),
    "strata" = c(rep("A", 100), rep("B", 50))
  ),
  strata = "strata", M = 10
)

resample_distribution(rnorm(100), fn = mean, M = 10)
```
