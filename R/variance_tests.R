#' Variance Tests for Homogeneity
#'
#' Compute and return information on tests for the homogeneity of data.
#'
#' @details
#' Tests require independent, quantitative data unless otherwise specified.
#' *Ratio*: Looks at the ratio of largest to smallest variance. Rule of thumb is
#'  that if this ratio is less than 4, no major violations are observed
#' *Levene*: No other major assumptions
#' *Bartlett*: Requires normally distributed data and no major outliers
#' *Fligner*: Also called Fligner-Killeen. Non-parametric test
#' *Hartley*: Requires normal distribution and equal number of observations.
#'
#' By default, ratio, Levene, Bartlett, and Fligner tests are run. Use `tests`
#' to select a subset. The tests have different assumptions, so results should
#' be interpreted in light of the study design rather than by selecting the
#' smallest p-value.
#'
#'
#' @param data Data.frame with the first column the values and the second column
#'  the group names
#' @param tests Vector of strings, or a string, indicating the tests to check.
#'  Options include 'ratio', 'levene', 'bartlett', 'fligner', and 'hartley'
#' @param ratio.threshold Numeric threshold for comparision of the variance ratio
#'
#' @returns Lists of variances and desired variance tests
#' @export
#'
#' @seealso [car::leveneTest()], [stats::bartlett.test()],
#'  [stats::fligner.test()], [PMCMRplus::hartleyTest()]
#'
#' @examples
#' data <- data.frame(
#'   "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
#'   "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
#' )
#' variance_tests(data)
variance_tests <- function(data, tests = c("ratio", "levene", "bartlett", "fligner"),
                           ratio.threshold = 4) {
  tests <- .validate_tests(
    tests,
    c("ratio", "levene", "bartlett", "fligner", "hartley")
  )

  # Prepare Data
  tmp <- .prepare_data(data)
  data <- tmp$data
  groups <- tmp$groups
  form <- tmp$form
  .validate_group_variances(data)

  # Variance Tests
  vars <- tapply(data[, 1], data[, 2], stats::var, na.rm = TRUE)
  res <- list("var" = vars)

  if ("ratio" %in% tests) {
    ratio <- max(vars) / min(vars)
    res <- append(res, list("ratio" = ratio, "ratio.threshold" = ratio.threshold))
  }
  if ("levene" %in% tests) {
    levene <- car::leveneTest(form, data = data)
    res <- append(res, list("levene_pvalue" = levene$`Pr(>F)`[1]))
  }
  if ("bartlett" %in% tests) {
    bartlett <- stats::bartlett.test(formula = form, data = data)
    res <- append(res, list("bartlett_pvalue" = bartlett$p.value))
  }
  if ("fligner" %in% tests) {
    fligner <- stats::fligner.test(formula = form, data)
    res <- append(res, list("fligner_pvalue" = fligner$p.value))
  }
  if ("hartley" %in% tests) {
    hartley <- suppressWarnings(PMCMRplus::hartleyTest(formula = form, data = data))
    res <- append(res, list("hartley_pvalue" = hartley$p.value))
  }


  res
}


#' Resample Variance
#'
#' Estimate variance for each group and bootstrap the alpha-confidence intervals.
#'
#' @param data Data.frame with the first column the values and the second column
#'  the group names
#' @param alphas Vector of significances for confidence intervals, defaults to 0.05
#' @param M Numeric for the number of iterations in the resampling
#'
#' @returns Matrix with variance and alpha-confidence intervals for each group
#' @export
#'
#' @examples
#' data <- data.frame(
#'   "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
#'   "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
#' )
#' resample_variance(data, M = 50)
resample_variance <- function(data, alphas = 0.05, M = 1000) {
  if (length(M) != 1 ||
      !is.numeric(M) ||
      !is.finite(M) ||
      M < 1 ||
      M != round(M)) {
    stop("`M` must be a positive integer.", call. = FALSE)
  }
  M <- as.integer(M)

  if (length(alphas) < 1 ||
      !is.numeric(alphas) ||
      any(!is.finite(alphas)) ||
      any(alphas <= 0) ||
      any(alphas >= 1)) {
    stop("`alphas` must contain values strictly between 0 and 1.",
         call. = FALSE)
  }

  # Prepare Data
  tmp <- .prepare_data(data)
  data <- tmp$data
  groups <- tmp$groups

  grp_unique <- unique(data[, 2])

  # True
  vars <- tapply(data[, 1], data[, 2], stats::var, na.rm = TRUE)

  # Sim data
  statistics <- vapply(seq_len(M),
    function(x, data, grp_unique) {
      # resample
      dat <- data.frame()
      for (i in 1:length(grp_unique)) {
        dat <-
          rbind(
            dat,
            data.frame(
              "value" =
                stats::var(sample(data[data[, 2] == grp_unique[i], 1],
                  replace = TRUE
                )),
              "group" = grp_unique[i]
            )
          )
      }
      as.numeric(dat$value)
    },
    FUN.VALUE = numeric(length(grp_unique)),
    data = data, grp_unique = grp_unique
  )

  # Bootstrapped confidence intervals
  rbind(
    vars,
    apply(statistics,
      MARGIN = 1, stats::quantile,
      prob = c(alphas / 2, rev(1 - alphas / 2))
    )
  )
}
