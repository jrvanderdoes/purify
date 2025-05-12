#' Normality Tests
#'
#' Compute and return information on tests for the normality of data.
#'
#' @details
#' Tests require independent data unless otherwise specified.
#' *shapiro*: Shapiro-Wilk test
#' *ks*: Kolmogorov-Smirnov test
#' *ad*: Anderson-Darling test
#' *cvm*: Cramer-von Mises test
#' *lilliefors*: lilliefors test
#' *pearson*: Pearson chi-square test
#' *sf*: Shapiro-Francia test
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
  # Normality Tests
  res <- list(
    "qqplot" = distribution_plot(x)
  )

  if ("shapiro" %in% tolower(tests)) {
    shapiro <- stats::shapiro.test(x)
    res <- append(res, list("shapiro" = shapiro$p.value))
  }
  if ("ks" %in% tolower(tests)) {
    ks <- stats::ks.test(x, "pnorm")
    res <- append(res, list("ks" = ks$p.value))
  }
  if ("ad" %in% tolower(tests)) {
    ad <- nortest::ad.test(x)
    res <- append(res, list("ad" = ad$p.value))
  }
  if ("cvm" %in% tolower(tests)) {
    cvm <- nortest::cvm.test(x)
    res <- append(res, list("cvm" = cvm$p.value))
  }
  if ("lilliefors" %in% tolower(tests)) {
    lillie <- nortest::lillie.test(x)
    res <- append(res, list("lilliefors" = lillie$p.value))
  }
  if ("pearson" %in% tolower(tests)) {
    pearson <- nortest::pearson.test(x)
    res <- append(res, list("pearson" = pearson$p.value))
  }
  if ("sf" %in% tolower(tests)) {
    sf <- nortest::sf.test(x)
    res <- append(res, list("sf" = sf$p.value))
  }


  res
}
