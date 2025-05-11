#' ANOVA (and similar) Tests for Mean/Median Differences
#'
#' Compute and return information on tests for the homogeneity of data.
#'
#' @details
#' Tests require independent data unless otherwise specified.
#' *anova*: Classic ANOVA. Requires normal data with homogeneous variance and
#'  similar group sizes
#' *welch*: ANOVA modification that allows nonhomogeneous variances and is more
#'  robust to varying group sizes
#' *bayes*: Bayesian approach to group differences
#' *kruskal*: Also called the Krukal-wallis test. Non-parametric test
#' *median*: Also called Brown-Mood median test. Non-parametric test with
#'  Bonferroni correction
#'
#' @param data Data.frame with the first column the values and the second column
#'  the group names
#' @param tests Vector of strings, or a string, indicating the tests to check.
#'  Options include 'anova', 'welch', 'bayes', 'kruskal', and 'median'
#'
#' @returns A list of means, medians, and anova test results
#' @export
#'
#' @seealso [stats::aov()], [stats::oneway.test()], [BayesFactor::anovaBF()],
#'  [stats::kruskal.test()], [PMCMRplus::medianTest()]
#'
#' @examples
#' data <- data.frame(
#'   "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
#'   "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
#' )
#' anova_tests(data)
anova_tests <- function(data,
                        tests = c("anova", "welch", "bayes", "kruskal", "median")) {
  # Prepare Data
  tmp <- .prepare_data(data)
  data <- tmp$data
  groups <- tmp$groups
  form <- tmp$form

  means <- tapply(data[, 1], data[, 2], mean)
  medians <- tapply(data[, 1], data[, 2], stats::median)

  # ANOVA Tests
  res <- list(
    "means" = means,
    "medians" = medians
  )

  if ("anova" %in% tolower(tests)) {
    anova_res <- stats::aov(form, data)
    res <- append(res, list("anova" = summary(anova_res)[[1]]$`Pr(>F)`[1]))
  }
  if ("welch" %in% tolower(tests)) {
    welch_res <- stats::oneway.test(form, data = data, var.equal = FALSE)
    res <- append(res, list("welch" = welch_res$p.value))
  }
  if ("bayes" %in% tolower(tests)) {
    bayes_res <- BayesFactor::anovaBF(form, data = data, progress = FALSE)
    res <- append(res, list("bayes" = BayesFactor::extractBF(bayes_res)[[1]]))
  }
  if ("kruskal" %in% tolower(tests)) {
    kruskal <- stats::kruskal.test(form, data)
    res <- append(res, list("kruskal" = kruskal$p.value))
  }
  if ("median" %in% tolower(tests)) {
    medn <- suppressWarnings(PMCMRplus::medianTest(formula = form, data = data, p.adjust.method = "bonferroni"))
    res <- append(res, list("median" = medn$p.value))
  }


  res
}


#' Resample Welch ANOVA
#'
#' Resample data for significance of groups when running an ANOVA [stats::oneway.test()]
#'
#' @param data Data.frame with the first column the values and the second column
#'  the group names
#' @param var.equal Boolean if we can assume equal variance over the groups
#' @param M Numeric for the number of iterations in the resampling
#'
#' @returns p-value for the effect of the groups
#' @export
#'
#' @examples
#' data <- data.frame(
#'   "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
#'   "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
#' )
#' resample_welch_anova(data)
resample_welch_anova <- function(data, var.equal = FALSE, M = 1000) {
  grp <- as.factor(data[, 2])
  obs <- data[, 1]

  grp_unique <- unique(grp)

  # Summary Statistics
  ns <- tapply(obs, grp, length)
  groups <- length(ns)
  means <- tapply(obs, grp, mean)
  data_null <- data
  data_null[, 1] <- data_null[, 1] - means[grp]

  # True
  stat_true <- stats::oneway.test(data[, 1] ~ data[, 2], var.equal = FALSE)$statistic

  # Sim Null
  statistics <- sapply(1:M,
    function(x, data_null, grp_unique, var.equal) {
      # resample
      dat <- data.frame()
      for (i in 1:length(grp_unique)) {
        dat <-
          rbind(
            dat,
            data.frame(
              "value" =
                sample(data_null[data_null[, 2] == grp_unique[i], 1],
                  replace = TRUE
                ),
              "group" = grp_unique[i]
            )
          )
      }

      stats::oneway.test(value ~ group, dat, var.equal = var.equal)$statistic
    },
    data_null = data_null, grp_unique = grp_unique, var.equal = var.equal
  )

  # Bootstrapped p-value
  mean(stat_true <= statistics)
}
