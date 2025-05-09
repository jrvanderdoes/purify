test_that("Multinomial Distribution Tests", {
  # pmultinomial
  tmp <- pmultinomial(c(2,2,2),c(1/3,1/3,1/3),6)
  expect_equal(round(tmp,4), 0.1235)
  tmp <- pmultinomial(c(3,1,2),c(2,4,4),4)
  expect_equal(round(tmp,4), 0.2944)

  # dmultinomial
  tmp <- dmultinomial(c(1,2,3),c(1,2,3))
  expect_equal(round(tmp,4), 0.1389)
  tmp <- dmultinomial(c(1,2,3),c(3,3,3))
  expect_equal(round(tmp,4), 0.0823)

  # rmultinomial
  set.seed(1234)
  tmp <- rmultinomial(10, 5, c(1/2,1/4,1/4))
  expect_equal(dim(tmp), c(3,10))
  expect_equal(sum(tmp), 50)
})
