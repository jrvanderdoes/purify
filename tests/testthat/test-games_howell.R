test_that("Games Howell", {
  set.seed(1234)
  data1 = data.frame('value'=c(rnorm(14,sd = 2), rnorm(6), rnorm(20,mean=2)),
                    'group'=c(rep('A',14),rep('B',6), rep('C',20)))
  tmp <- games_howell(data1)
  expect_equal(dim(tmp), c(3,7))
  expect_equal(round(tmp[1,1],3), 1.027)
})
