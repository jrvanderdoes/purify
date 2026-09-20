test_that("Box-Cox Testing", {
  set.seed(2124)
  data1 <- data.frame(
    "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
    "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
  )
  tmp <- boxcox_transformation(data1)
  expect_equal(round(tmp$lambda, 3), 1.303)
  expect_equal(tmp$shift, min(data1[, 1]))

  tmp1 <- boxcox_inverse(tmp$data[, 1], tmp$lambda, tmp$shift)
  expect_equal(tmp1, data1[, 1])
})

test_that("Box-Cox functions validate inputs", {
  dat <- data.frame(value = 1:6, group = rep(c("A", "B"), each = 3))

  expect_error(
    boxcox_transformation(dat, lambdas = numeric(0)),
    "non-empty finite numeric vector"
  )
  expect_error(
    boxcox_inverse(1:3, lambda = NA, shift = 0),
    "single finite numeric value"
  )
})
