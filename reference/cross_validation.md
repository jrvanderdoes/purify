# Cross-Validation

Run cross-validation on data which may be dependent. Parameters offer
choices for simple application on various data sets.

## Usage

``` r
cross_validation(
  data,
  pred_fn,
  cv_group_number = NULL,
  error_fn = function(data, pred) {
     sum((data - pred)^2)
 },
  cv.dependent = FALSE,
  sliding.start = NULL,
  ...
)
```

## Arguments

- data:

  Data.frame (or vector) to apply cross-validation where the rows are
  the observations and the columns are the variables.

- pred_fn:

  Function that takes data without CV group (first), then the CV group
  (second), and arguments from ... to predict the CV group.

- cv_group_number:

  Number of CV groups to use. The default is leave-on-out.

- error_fn:

  Function that take the data and predictions and returns an error
  value. When multiple errors are returned, the mean is taken. The
  default is mean squared error.

- cv.dependent:

  Boolean that indicates if data should not be mixed due to internal
  dependence.

- sliding.start:

  Index to start cross-validation. Only used when `cv.dependent`. Useful
  when the first observations should be used to fit, then data is slowly
  added and predicted.

- ...:

  Additional parameters for `pred_fn()`

## Value

Vector with (1) mean of the errors and (2) sd of the errors.

## Examples

``` r
n <- 50
data <- data.frame(Y = NA, X = rnorm(n))
data$Y <- 2 * data$X + stats::rnorm(nrow(data))
pred_fn <- function(x_fit, x_pred) {
  as.numeric(predict(lm(Y ~ X, data = data[-c(1:10), ]), data[1:10, ]))
}
cross_validation(data, pred_fn, cv_group_number = 10, cv.dependent = FALSE)
#>       est        sd 
#> 101.94353  25.02722 
```
