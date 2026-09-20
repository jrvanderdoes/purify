test_that("Effect Size Test", {
  set.seed(123)
  x <- rnorm(10, mean = 1, sd = 1)
  y <- rnorm(40, mean = 3, sd = 10)
  expect_equal(round(cohens_d(x, y, var.type = "unequal"), 3), 0.328)
  expect_equal(round(cohens_d(x, y, var.type = "pooled", hedges.correction = TRUE), 3), 0.254)

  data <- data.frame(c(x, y), c(rep("x", 10), rep("y", 40)))
  expect_equal(round(cohens_d(data, var.type = "unequal"), 3), 0.328)
})

test_that("cohens_d rejects a zero selected variance estimate", {
  expect_error(
    cohens_d(c(1, 1, 1), c(2, 2, 2)),
    "variance estimate must be positive and finite"
  )
})

test_that("cohens_d validates inputs", {
  expect_error(
    cohens_d(c(1, NA), c(2, 3)),
    "finite numeric vectors"
  )
  expect_error(
    cohens_d(c("a", "b"), c(2, 3)),
    "finite numeric vectors"
  )
  expect_error(
    cohens_d(1, c(2, 3)),
    "finite numeric vectors"
  )
  expect_error(
    cohens_d(c(1, 2), c(2, 3), var.type = "invalid"),
    "var.type"
  )
  expect_error(
    cohens_d(data.frame(value = 1:4, group = c("A", "A", "B", "C"))),
    "at least 2 observations"
  )
})

test_that("Eta Squared", {
  set.seed(123)
  data1 <- data.frame(
    "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
    "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
  )
  expect_equal(round(eta_squared(data1)[1, 1], 3), 0.324)
})

test_that("eta_squared rejects a constant response", {
  dat <- data.frame(
    value = rep(1, 6),
    group = rep(c("A", "B"), each = 3)
  )

  expect_error(
    eta_squared(dat),
    "response variable must have positive, finite variance"
  )
})

test_that("Resampled Differences", {
  set.seed(123)
  data1 <- data.frame(
    "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
    "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
  )
  tmp <- resample_differences(data1, M = 50)
  expect_equal(round(sum(tmp$diff), 3), 3.086)
  expect_true(all(tmp$lwr <= tmp$upr))
})

test_that("resample_differences validates alpha and M", {
  dat <- data.frame(
    value = c(1, 2, 3, 4),
    group = rep(c("A", "B"), each = 2)
  )

  expect_error(
    resample_differences(dat, alpha = 0),
    "strictly between 0 and 1"
  )
  expect_error(
    resample_differences(dat, M = 1.5),
    "`M` must be a positive integer"
  )
})
