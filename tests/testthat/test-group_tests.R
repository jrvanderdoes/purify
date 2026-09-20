test_that("Two group tests", {
  set.seed(1231)
  data1 <- data.frame(
    "value" = c(rnorm(14, sd = 2), rnorm(10, mean = 2)),
    "group" = c(rep("A", 14), rep("B", 10))
  )
  tmp <- two_group_tests(data1)
  expect_equal(length(tmp), 6)
  expect_equal(round(tmp$ttest_welch$pvalue, 3), 0.008)
})


test_that("Group tests", {
  set.seed(1231)
  data1 <- data.frame(
    "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
    "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
  )
  tmp <- group_tests(data1)
  expect_equal(length(tmp), 19)
  expect_equal(round(sum(tmp$means), 4), 2.5661)
})

test_that("group_tests returns non-pooled pairwise t-test p-values", {
  dat <- data.frame(
    value = c(1, 2, 3, 10, 20, 30),
    group = rep(c("A", "B"), each = 3)
  )

  result <- group_tests(dat, tests = "bt")
  expected <- stats::pairwise.t.test(
    dat$value,
    dat$group,
    pool.sd = FALSE,
    p.adjust.method = "bonferroni"
  )$p.value

  expect_equal(result$bt$pvalues_nonpool, expected)
})

test_that("group test functions validate alpha", {
  dat <- data.frame(
    value = c(1, 2, 3, 10, 20, 30),
    group = rep(c("A", "B"), each = 3)
  )

  expect_error(
    group_tests(dat, tests = "bt", alpha = 0),
    "strictly between 0 and 1"
  )
  expect_error(
    two_group_tests(dat, tests = "t", alpha = 1),
    "strictly between 0 and 1"
  )
})

test_that("group test functions reject unknown test names", {
  dat <- data.frame(value = c(1, 2, 3, 10, 20, 30),
                    group = rep(c("A", "B"), each = 3))

  expect_error(group_tests(dat, tests = "unknown"), "Unknown test")
  expect_error(two_group_tests(dat, tests = "unknown"), "Unknown test")
})
