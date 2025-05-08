test_that("Distribution Plot", {
  set.seed(213)
  expect_equal(class(distribution_plot(rnorm(100)))[2],'ggplot')
  expect_equal(class(distribution_plot(rexp(100), dist='exp'))[2],'ggplot')
})
