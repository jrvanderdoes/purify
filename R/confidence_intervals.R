#' Bootstrapped Confidence Intervals
#'
#' Method for the computation of bootstrapped confidence intervals.
#'
#' @param data Data.frame (or vector) to apply cross-validation where the rows
#'  are the observations and the columns are the variables.
#' @param pred_model Model to predict the data where the first argument in the
#'  data and the second the length of prediction. Additional parameters can be
#'  passed using ... . Returns a vector of the predictions.
#' @param h Number of observations to predict into the future.
#' @param train Minimum amount of data (0,1) to train model. After this point, a
#'  sliding window approach is taken.
#' @param M Numeric. Number of bootstrap iterations.
#' @param alpha Significance level for the intervals
#' @param output Numeric or name of the column for the output when using a
#'  data.frame for \code{data}. If not given, the first column is used.
#' @param ... Additional parameters to pass to \code{pred_model}.
#'
#' @returns Data.frame with columns for the forecasts and lower/upper confidence
#'  intervals.
#' @export
#'
#' @examples
#' data <- cumsum(rnorm(100))
#' pred_model <- function(x, h) {
#'   predict(forecast::ets(x), h = h)$mean
#' }
#' h <- 10
#' ets_model <- predict(forecast::ets(data[1:90]), h)
#' ints <- confidence_intervals(data = data[1:90], pred_model = pred_model, h = h, M = 50)
#'
#' plot(ets_model)
#' lines(x = 91:100, y = ints$lower, col = "red")
#' lines(x = 91:100, y = ints$upper, col = "red")>
confidence_intervals <- function(data, pred_model, h, train = 0.8, M = 1000,
                                 alpha = 0.05, output = NULL, ...) {
  if (length(h) != 1 ||
      !is.numeric(h) ||
      !is.finite(h) ||
      h < 1 ||
      h != round(h)) {
    stop("`h` must be a positive integer.", call. = FALSE)
  }
  h <- as.integer(h)

  if (length(M) != 1 ||
      !is.numeric(M) ||
      !is.finite(M) ||
      M < 1 ||
      M != round(M)) {
    stop("`M` must be a positive integer.", call. = FALSE)
  }
  M <- as.integer(M)

  if (length(alpha) != 1 ||
      !is.numeric(alpha) ||
      !is.finite(alpha) ||
      alpha <= 0 ||
      alpha >= 1) {
    stop("`alpha` must be strictly between 0 and 1.", call. = FALSE)
  }

  if (length(train) != 1 ||
      !is.numeric(train) ||
      !is.finite(train) ||
      train < 0 ||
      train > 1) {
    stop("The parameter `train` should be between 0 and 1.", call. = FALSE)
  }
  # Data size
  n <- nrow(data)
  data.vector <- FALSE
  if (is.null(n)) {
    n <- length(data)
    data.vector <- TRUE
  }

  train_n <- floor(train * n)

  if (train_n < 1) {
    stop("`train` must provide at least one training observation.",
         call. = FALSE)
  }

  # Create sliding h chunks
  i <- 1
  cv_groups <- list()
  while (train_n + i + h <= n) {
    cv_groups[[i]] <- train_n + i + 1:h
    i <- i + 1
  }

  if (length(cv_groups) == 0) {
    stop(
      "The combination of `train`, `h`, and data length leaves no validation windows.",
      call. = FALSE
    )
  }

  # Get forecast errors
  resids <- matrix(NA, ncol = h, nrow = length(cv_groups))

  if (data.vector) {
    for (i in 1:length(cv_groups)) {
      preds <- pred_model(data[1:(min(cv_groups[[i]]) - 1)], h, ...)
      resids[i, ] <- preds - data[cv_groups[[i]]]
    }
  } else {
    if (is.null(output)) {
      warning("No output specified, using first column")
      output <- 1
    }

    for (i in 1:length(cv_groups)) {
      preds <- pred_model(
        data[1:(min(cv_groups[[i]]) - 1), ],
        h,
        ...
      )
      resids[i, ] <- preds - data[cv_groups[[i]], output]
    }
  }

  # Resample
  resampled_resids <- do.call(
    cbind,
    lapply(seq_len(h), function(j) {
      sample(resids[, j], M, replace = TRUE)
    })
  )

  # Significance
  quants <- apply(resampled_resids,
    MARGIN = 2,
    function(x) {
      stats::quantile(x, probs = c(alpha / 2, 1 - alpha / 2))
    }
  )
  rownames(quants) <- c("lower", "upper")

  # Predict
  ests <- pred_model(data, h, ...)
  data.frame(
    "forecasts" = ests,
    apply(quants, MARGIN = 1, function(x, preds) {
      x + preds
    }, preds = ests)
  )
}
