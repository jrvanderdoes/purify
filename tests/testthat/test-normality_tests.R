test_that("normality_tests validates input", {
  expect_error(
    normality_tests(c(1, NA, 3), tests = "ks"),
    "cannot contain missing"
  )
  expect_error(
    normality_tests(c(1, 2), tests = "ks"),
    "at least 3 observations"
  )
  expect_error(
    normality_tests(c("a", "b", "c"), tests = "ks"),
    "must be a numeric vector"
  )
  expect_error(
    normality_tests(1:3, tests = "unknown"),
    "Unknown normality test"
  )
})
