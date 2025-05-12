test_that("ANOVA-like Tests", {
  set.seed(1232)
  data1 <- data.frame(
    "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
    "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
  )
  tmp <- anova_tests(data1)

  expect_equal(length(tmp), 7)
  expect_equal(round(tmp$anova, 3), 0)

  tmp <- anova_tests(data1, tests = "anova")
  expect_equal(length(tmp), 3)
})


test_that("Resample ANOVA", {
  set.seed(1232)
  data1 <- data.frame(
    "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
    "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
  )
  expect_equal(round(resample_welch_anova(data1), 3), 0.001)

  set.seed(1232)
  data1 <- data.frame(
    "value" = c(rnorm(20, sd = 4), rnorm(10), rnorm(15, sd = 2)),
    "group" = c(rep("A", 20), rep("B", 10), rep("C", 15))
  )
  expect_equal(round(resample_welch_anova(data1), 3), 0.839)
})
