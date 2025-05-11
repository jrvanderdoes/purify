#' Box-Cox Transformation
#'
#' Compute the Box-Cox transformation for improvement
#'  of the Normality and residual assumptions.
#'
#' @param data Data.frame with the first column the values and the second column
#'  the group names
#' @param lambdas Numeric values for potential lamba value
#'
#' @returns List with shift data, lambda parameter, and shift amount to ensure
#'  no negative values
#' @export
#'
#' @examples
#' data <- data.frame(
#'   "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
#'   "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
#' )
#' boxcox_transformation(data)
boxcox_transformation <- function(data, lambdas = seq(-3, 3, 1 / 10)) {
  # Make all positive
  x_shift <- data[, 1] - min(data[, 1]) + 1

  # Apply the Box-Cox transformation
  boxcox_result <- MASS::boxcox(x_shift ~ data[, 2], lambda = lambdas)

  # Find the optimal lambda
  lambda <- boxcox_result$x[which.max(boxcox_result$y)]

  # Transform and return
  if (lambda != 0) {
    dat <- data.frame((x_shift^lambda - 1) / lambda, data[, 2])
  } else {
    dat <- data.frame(log(x_shift), data[, 2])
  }
  colnames(dat) <- colnames(data)

  list("data" = dat, "lambda" = lambda, "shift" = min(data[, 1]))
}


#' Reverse Box-Cox Transformation
#'
#' Compute the reverse Box-Cox transformation for improvement
#'  of the Normality and residual assumptions.
#'
#' @param x Vector that was modified by the Box-Cox transformation
#' @param lambda Value for the lambda parameter of the Box-Cox transformation
#' @param shift Shift amount from Box-Cox transformation
#'
#' @returns Data x transformed to the original scale before the Box-Cox
#'  transformation
#' @export
#'
#' @examples
#' data <- data.frame(
#'   "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
#'   "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
#' )
#' tmp <- boxcox_transformation(data)
#' boxcox_inverse(tmp$data[, 1], tmp$lambda, tmp$shift)
boxcox_inverse <- function(x, lambda, shift) {
  if (lambda != 0) {
    partial_invert <- (x * lambda + 1)^(1 / lambda)
  } else {
    partial_invert <- exp(x)
  }

  partial_invert + shift - 1
}
