test_that("Variance Tests", {
  set.seed(341)
  data1 <- data.frame(
    "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
    "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
  )
  tmp <- variance_tests(data1)
  expect_equal(length(tmp), 6)
  tmp <- variance_tests(data1, tests = c("ratio", "levene", "bartlett", "fligner", "hartley"))
  expect_equal(round(tmp$fligner_pvalue, 2), 0.01)
})

test_that("Resample Variances", {
  set.seed(341)
  data1 <- data.frame(
    "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
    "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
  )
  tmp <- resample_variance(data1, alphas = c(0.05, 0.1))
  expect_equal(dim(tmp), c(5, 3))
  expect_equal(round(tmp[1, 3], 2), 1.14)
})

test_that("resample_variance validates alphas and M", {
  dat <- data.frame(
    value = c(1, 2, 3, 4),
    group = rep(c("A", "B"), each = 2)
  )

  expect_error(
    resample_variance(dat, alphas = 0),
    "strictly between 0 and 1"
  )
  expect_error(
    resample_variance(dat, M = 1.5),
    "`M` must be a positive integer"
  )
})

test_that("resample_variance preserves dimensions when M is one", {
  dat <- data.frame(
    value = c(1, 2, 3, 4),
    group = rep(c("A", "B"), each = 2)
  )

  result <- resample_variance(dat, M = 1)
  expect_equal(dim(result), c(3, 2))
})

test_that("variance_tests rejects unknown test names", {
  dat <- data.frame(value = 1:6, group = rep(c("A", "B"), each = 3))

  expect_error(
    variance_tests(dat, tests = "unknown"),
    "Unknown test"
  )
})

test_that("variance_tests rejects zero-variance groups", {
  dat <- data.frame(value = c(1, 1, 2, 3),
                    group = rep(c("A", "B"), each = 2))

  expect_error(variance_tests(dat), "positive, finite variance")
})
