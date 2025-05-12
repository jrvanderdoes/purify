test_that("Variance Tests", {
  set.seed(341)
  data1 <- data.frame(
    "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
    "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
  )
  tmp <- variance_tests(data1)
  expect_equal(length(tmp), 6)
  tmp <- variance_tests(data1, tests = c("ratio", "levene", "bartlett", "fligner", "ha"))
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
