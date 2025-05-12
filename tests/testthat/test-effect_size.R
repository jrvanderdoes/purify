test_that("Effect Size Test", {
  set.seed(123)
  x <- rnorm(10, mean = 1, sd = 1)
  y <- rnorm(40, mean = 3, sd = 10)
  expect_equal(round(cohens_d(x, y, var.type = "unequal"), 3), 0.328)
  expect_equal(round(cohens_d(x, y, var.type = "pooled", hedges.correction = TRUE), 3), 0.254)

  data <- data.frame(c(x, y), c(rep("x", 10), rep("y", 40)))
  expect_equal(round(cohens_d(data, var.type = "unequal"), 3), 0.328)
})

test_that("Eta Squared", {
  set.seed(123)
  data1 <- data.frame(
    "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
    "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
  )
  expect_equal(round(eta_squared(data1)[1, 1], 3), 0.324)
})

test_that("Resampled Differences", {
  set.seed(123)
  data1 <- data.frame(
    "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
    "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
  )
  tmp <- resample_differences(data1, M = 50)
  expect_equal(round(sum(tmp$diff), 3), 3.086)
})
