test_that("Plot Strata", {
  dat <- data.frame("B" = rbinom(100, 2, 0.5), "A" = rnorm(100))
  tmp <- plot_strata_bar(dat[, 1])
  expect_equal(class(tmp)[1], "gg")

  tmp <- plot_strata_box(dat)
  expect_equal(class(tmp)[1], "gg")
})
