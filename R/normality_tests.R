#' Normality Tests
#'
#' Compute and return information on tests for the normality of data.
#'
#' @details
#' Tests require independent data unless otherwise specified.
#' *shapiro*: Shapiro-Wilk test
#' *ks*: Kolmogorov-Smirnov test against the standard normal distribution
#'  \eqn{N(0, 1)}
#' *ad*: Anderson-Darling test
#' *cvm*: Cramer-von Mises test
#' *lilliefors*: lilliefors test
#' *pearson*: Pearson chi-square test
#' *sf*: Shapiro-Francia test
#'
#' The input must be a finite numeric vector with at least three observations.
#' The Shapiro-Wilk test additionally supports at most 5000 observations.
#'
#' @param x Vector of numerics to check normality
#' @param tests Vector of strings, or a string, indicating the tests to check.
#'  Options include 'shapiro', 'ks', 'ad', 'cvm', 'lilliefors', 'pearson', and
#'  'sf'
#'
#' @returns A list of qqplot and normality test results
#' @export
#'
#' @seealso [stats::shapiro.test()], [stats::ks.test()], [nortest::ad.test()],
#'  [nortest::cvm.test()], [nortest::lillie.test()], [nortest::pearson.test()],
#'  [nortest::sf.test()]
#'
#' @examples
#' x <- c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2))
#' normality_tests(x)
normality_tests <- function(x,
                            tests = c(
                              "shapiro", "ks", "ad", "cvm", "lilliefors",
                              "pearson", "sf"
                            )) {
  if (!is.numeric(x) || !is.null(dim(x))) {
    stop("`x` must be a numeric vector.", call. = FALSE)
  }

  if (length(x) < 3) {
    stop("`x` must contain at least 3 observations.", call. = FALSE)
  }

  if (any(!is.finite(x))) {
    stop("`x` cannot contain missing, NaN, or infinite values.", call. = FALSE)
  }

  if (!is.character(tests) || length(tests) < 1) {
    stop("`tests` must contain one or more test names.", call. = FALSE)
  }

  tests <- tolower(tests)
  valid_tests <- c("shapiro", "ks", "ad", "cvm", "lilliefors", "pearson", "sf")

  if (any(!tests %in% valid_tests)) {
    stop(
      paste0("Unknown normality test. Choose from: ",
             paste(valid_tests, collapse = ", "), "."),
      call. = FALSE
    )
  }

  if ("shapiro" %in% tests && length(x) > 5000) {
    stop("The Shapiro-Wilk test supports at most 5000 observations.",
         call. = FALSE)
  }

  # Normality Tests
  res <- list(
    "qqplot" = distribution_plot(x)
  )

  if ("shapiro" %in% tests) {
    shapiro <- stats::shapiro.test(x)
    res <- append(res, list("shapiro" = shapiro$p.value))
  }
  if ("ks" %in% tests) {
    ks <- stats::ks.test(x, "pnorm")
    res <- append(res, list("ks" = ks$p.value))
  }
  if ("ad" %in% tests) {
    ad <- nortest::ad.test(x)
    res <- append(res, list("ad" = ad$p.value))
  }
  if ("cvm" %in% tests) {
    cvm <- nortest::cvm.test(x)
    res <- append(res, list("cvm" = cvm$p.value))
  }
  if ("lilliefors" %in% tests) {
    lillie <- nortest::lillie.test(x)
    res <- append(res, list("lilliefors" = lillie$p.value))
  }
  if ("pearson" %in% tests) {
    pearson <- nortest::pearson.test(x)
    res <- append(res, list("pearson" = pearson$p.value))
  }
  if ("sf" %in% tests) {
    sf <- nortest::sf.test(x)
    res <- append(res, list("sf" = sf$p.value))
  }


  res
}
