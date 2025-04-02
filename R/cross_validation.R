#' Cross-Validation
#'
#' Run cross-validation on data which may be dependent. Parameters offer choices
#'  for simple application on various data sets.
#'
#' @param data Data.frame (or vector) to apply cross-validation where the rows
#'  are the observations and the columns are the variables.
#' @param pred_fn Function that takes data without CV group (first), then the CV
#'  group (second), and arguments from ... to predict the CV group.
#' @param cv_group_number Number of CV groups to use. The default is leave-on-out.
#' @param error_fn Function that take the data and predictions and returns an
#'  error value. When multiple errors are returned, the mean is taken. The
#'  default is mean squared error.
#' @param cv.dependent Boolean that indicates if data should not be mixed due to
#'  internal dependence.
#' @param sliding.start Index to start cross-validation. Only used when
#'  \code{cv.dependent}. Useful when the first observations should be used to
#'  fit, then data is slowly added and predicted.
#' @param ... Additional parameters for \code{pred_fn()}
#'
#' @returns Mean of the errors
#' @export
#'
#' @examples
#' n <- 50
#' data <- data.frame(Y = NA, X = rnorm(n))
#' data$Y <- 2 * data$X + stats::rnorm(nrow(data))
#' pred_fn <- function(x_fit, x_pred) {
#'   as.numeric(predict(lm(Y ~ X, data = data[-c(1:10), ]), data[1:10, ]))
#' }
#' cross_validation(data, pred_fn, cv_group_number = 10, cv.dependent = FALSE)
cross_validation <- function(
    data, pred_fn, cv_group_number = nrow(data),
    error_fn = function(data, pred) {
      sum((data - pred)^2)
    },
    cv.dependent = FALSE, sliding.start = NULL, ...) {
  ## Define length
  n <- nrow(data)
  data.vector <- FALSE
  if (is.null(n)) {
    n <- length(data)
    data.vector <- TRUE
  }

  ## Define groups
  cv_group_number <- min(cv_group_number, n)
  if (!cv.dependent) {
    cv_groups <- split(1:n, cut(sample(1:n), cv_group_number, labels = FALSE))
  } else {
    if (is.null(sliding.start)) {
      cv_groups <- .getChunks(1:n, n / cv_group_number)
    } else {
      pred_choices <- (sliding.start + 1):n
      cv_group_number <- min(length(pred_choices), cv_group_number)
      cv_groups <- .getChunks(pred_choices, cv_group_number)
    }
  }

  ## Forecast
  errors <- rep(NA, cv_group_number)

  for (i in 1:cv_group_number) {
    if (is.null(sliding.start)) {
      if (data.vector) {
        dat_fit <- data[-cv_groups[[i]]]
        dat_cv <- data[cv_groups[[i]]]
      } else {
        dat_fit <- data[-cv_groups[[i]], ]
        dat_cv <- data[cv_groups[[i]], ]
      }
    } else {
      if (data.vector) {
        dat_fit <- data[1:(min(cv_groups[[i]]) - 1)]
        dat_cv <- data[cv_groups[[i]]]
      } else {
        dat_fit <- data[1:(min(cv_groups[[i]]) - 1), ]
        dat_cv <- data[cv_groups[[i]], ]
      }
    }

    preds <- pred_fn(dat_fit, dat_cv, ...)
    err <- error_fn(dat_cv, preds)
    if (length(err) == 1) {
      errors[i] <- err
    } else {
      errors[i] <- mean(err)
    }
  }

  mean(errors)
}
