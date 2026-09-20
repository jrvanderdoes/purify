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

test_that("resample_distribution rejects empty inputs", {
  expect_error(
    resample_distribution(numeric(0), M = 10),
    "must be non-empty"
  )
  expect_error(
    resample_distribution(1:3, resampled_data = list()),
    "must be non-empty"
  )
  expect_no_error(resample_distribution(rep(1, 10), M = 2))
})
