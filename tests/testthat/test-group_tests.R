test_that("Two group tests", {
  set.seed(1231)
  data1 <- data.frame('value'=c(rnorm(14,sd = 2), rnorm(10,mean=2)),
                     'group'=c(rep('A',14),rep('B',10)) )
  tmp <- two_group_tests(data1)
  expect_equal(length(tmp), 6)
  expect_equal(round(tmp$ttest_welch$pvalue, 3), 0.008)
})


test_that("Group tests", {
  set.seed(1231)
  data1 <- data.frame('value'=c(rnorm(14,sd = 2), rnorm(6), rnorm(20,mean=2)),
                     'group'=c(rep('A',14),rep('B',6), rep('C',20)))
  tmp <- group_tests(data1)
  expect_equal(length(tmp), 18)
  expect_equal(round(sum(tmp$means),4), 2.5661)
})
