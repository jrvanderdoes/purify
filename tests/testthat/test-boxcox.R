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
