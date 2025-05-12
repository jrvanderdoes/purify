test_that("Resample Distribution Test", {
  set.seed(213)
  tmp <- resample_distribution(rnorm(100), fn = mean)
  expect_equal(class(tmp)[1], "patchwork")
  tmp <- resample_distribution(
    data.frame(
      "data" = c(rnorm(100), rnorm(50, mean = 10)),
      "strata" = c(rep("A", 100), rep("B", 50))
    ),
    strata = "strata"
  )
  expect_equal(class(tmp)[1], "patchwork")
})
