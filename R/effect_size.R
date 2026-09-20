#' Effect Size Statistics
#'
#' Compute the Cohen's d and Hedges' g effect size statistics.
#'
#' @param x Vector of values for the first group (if y specified) or a data.frame
#'  with the first column the values and the second column the group names (no y).
#'  If using a data.frame, ensure only two groups are given.
#' @param y Vector of values for the second group (if x is a vector), otherwise
#'  NULL if x is a data.frame.
#' @param var.type String indicating the variance assumption. Options include
#'  'unequal' (default), 'pooled', 'x', or 'y'.
#' @param hedges.correction Boolean indicating if Hedges correction should be
#'  applied
#' @param hedges.approx Boolean if Hedges correction should be approximated or
#'  computed using the gamma function
#'
#' @returns Numeric value for the effect size
#' @export
#'
#' @references Cohen, J. (1988). Statistical Power Analysis for the Behavioral
#'  Sciences (2nd ed.). Routledge.
#' @references Hedges, Larry & Olkin, Ingram. (1985). Statistical Methods in
#'  Meta-Analysis. 10.2307/1164953.
#' @references Theriault, R., (2023). rempsyc: Convenience functions for
#'  psychology. *Journal of Open Source Software*, *8*(87), 5466.
#'
#' @examples
#' x <- rnorm(10, mean = 1, sd = 1)
#' y <- rnorm(40, mean = 3, sd = 10)
#' cohens_d(x, y, var.type = "unequal")
#' cohens_d(x, y, var.type = "pooled", hedges.correction = TRUE)
cohens_d <- function(x, y = NULL, var.type = "unequal", hedges.correction = FALSE,
                     hedges.approx = TRUE) {
  valid_var_types <- c("unequal", "pooled", "x", "y")
  if (length(var.type) != 1 ||
      !is.character(var.type) ||
      is.na(var.type) ||
      !tolower(var.type) %in% valid_var_types) {
    stop("`var.type` must be one of: unequal, pooled, x, or y.",
         call. = FALSE)
  }
  var.type <- tolower(var.type)

  if (is.null(y)) {
    x <- .validate_group_data(x)
    X_tmp <- x
    groups <- unique(x[, 2])
    if (length(groups) != 2) stop("Cohen's d / Hedges' g only used for two groups")
    x <- X_tmp[X_tmp[, 2] == groups[1], 1]
    y <- X_tmp[X_tmp[, 2] == groups[2], 1]
  } else {
    if (!is.numeric(x) || !is.numeric(y) ||
        length(x) < 2 || length(y) < 2 ||
        any(!is.finite(x)) || any(!is.finite(y))) {
      stop("`x` and `y` must be finite numeric vectors with at least 2 observations.",
           call. = FALSE)
    }
  }

  lx <- length(x) - 1
  ly <- length(y) - 1

  if (var.type == "unequal") {
    var_est <- (stats::var(x) + stats::var(y)) / 2
  } else if (var.type == "pooled") {
    var_est <- (lx * stats::var(x) + ly * stats::var(y)) / (lx + ly)
  } else if (var.type == "x") {
    var_est <- stats::var(x)
  } else if (var.type == "y") {
    var_est <- stats::var(y)
  } else {
    stop("Verify variance status")
  }

  if (!is.finite(var_est) || var_est <= 0) {
    stop("The selected variance estimate must be positive and finite.",
         call. = FALSE)
  }

  est <- abs(mean(x) - mean(y)) / sqrt(var_est)

  if (hedges.correction) {
    df <- lx + ly
    if (hedges.approx) {
      est <- est * (1 - 3 / (4 * (df) - 1))
    } else {
      est <- est * (gamma(df / 2) / (sqrt(df / 2) * gamma((df - 1) / 2)))
    }
  }

  est
}


#' Eta Squared
#'
#' Compute the Eta squared effect size statistic.
#'
#' @param data Data.frame with the first column the values and the second column
#'  the group names
#'
#' @returns Table with eta-squared attached to classic ANOVA decomposition
#' @export
#'
#' @references Navarro, D. J. (2015) Learning statistics with R: A tutorial for
#'  psychology students and other beginners. (Version 0.6) University of New
#'  South Wales. Sydney, Australia
#'
#' @examples
#' data <- data.frame(
#'   "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
#'   "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
#' )
#' eta_squared(data)
eta_squared <- function(data) {
  data <- .validate_group_data(data)

  ## General Info
  col_names <- colnames(data)
  if (is.null(col_names)) {
    colnames(data) <- c("value", "group")
    col_names <- colnames(data)
  }
  data[, 2] <- as.factor(data[, 2])
  groups <- unique(data[, 2])

  total_variance <- stats::var(data[[col_names[1]]])
  if (!is.finite(total_variance) || total_variance <= 0) {
    stop("The response variable must have positive, finite variance.",
         call. = FALSE)
  }

  ## ANOVA
  form <- stats::reformulate(
    termlabels = col_names[2],
    response = col_names[1]
  )

  anova_res <- stats::aov(form, data)

  ## Eta^2
  eta2 <- stats::var(stats::predict(anova_res)) / stats::var(data[[col_names[1]]])

  cbind("eta squared" = c(eta2, 1 - eta2), summary(anova_res)[[1]])
}


#' Resampled Mean Differences
#'
#' Estimate pairwise mean differences and percentile bootstrap confidence
#' intervals between groups. The returned \code{se} column is the ordinary
#' standard error of a difference between two independent means, estimated from
#' the bootstrap group variances.
#'
#' @param data Data.frame with the first column the values and the second column
#'  the group names
#' @param alpha Significance for confidence intervals, defaults to 0.05
#' @inheritParams resample
#'
#' @returns A data frame with one row for each pair of groups. The \code{diff}
#'  column contains the mean difference (second group minus first group),
#'  \code{lwr} and \code{upr} contain the percentile bootstrap confidence
#'  limits, and \code{se} contains the estimated standard error.
#' @export
#'
#' @examples
#' data <- data.frame(
#'   "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
#'   "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
#' )
#' # Be sure to increase M for real use cases
#' resample_differences(data, M = 50)
resample_differences <- function(data, alpha = 0.05, M = 1000) {
  if (length(alpha) != 1 ||
      !is.numeric(alpha) ||
      !is.finite(alpha) ||
      alpha <= 0 ||
      alpha >= 1) {
    stop("`alpha` must be strictly between 0 and 1.", call. = FALSE)
  }

  if (length(M) != 1 ||
      !is.numeric(M) ||
      !is.finite(M) ||
      M < 1 ||
      M != round(M)) {
    stop("`M` must be a positive integer.", call. = FALSE)
  }
  M <- as.integer(M)

  data <- .validate_group_data(data)

  grp <- data[, 2]
  obs <- data[, 1]

  # Group Combinations (Interactions)
  combs <- utils::combn(unique(grp), 2)

  # Summary Statistics
  ns <- tapply(obs, grp, length)
  groups <- length(ns)
  means <- tapply(obs, grp, mean)

  statistics <- sapply(1:ncol(combs),
    function(x, ns, groups, means, alpha, M) {
      # resample
      g1 <- resample(data[data[, 2] == combs[1, x], 1],
        replace = T, M = M
      )
      g2 <- resample(data[data[, 2] == combs[2, x], 1],
        replace = T, M = M
      )

      mean_diffs <- rowMeans(g2) - rowMeans(g1)
      vars1 <- apply(g1, MARGIN = 1, stats::var) / ns[combs[1, x]]
      vars2 <- apply(g2, MARGIN = 1, stats::var) / ns[combs[2, x]]

      mean_diff <- means[combs[2, x]] - means[combs[1, x]]

      c(
        paste0(combs[2, x], "-", combs[1, x]),
        mean_diff, sqrt(mean(vars1) + mean(vars2)),
        as.numeric(stats::quantile(mean_diffs, probs = c(alpha / 2, 1 - alpha / 2)))
      )
    },
    ns = ns, groups = groups, means = means, alpha = alpha, M = M
  )



  # Create dataframe from flattened list
  results <- data.frame(t(statistics))

  # Select columns set as factors that should be numeric and change with as.numeric
  results[-1] <-
    as.numeric(as.matrix(results[-1]))

  # Rename data frame columns
  colnames(results) <- c("groups", "diff", "se", "lwr", "upr")
  rownames(results) <- results$groups

  results[, c(2, 4:5, 3)]
}
