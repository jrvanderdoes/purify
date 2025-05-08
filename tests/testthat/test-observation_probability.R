test_that("Observation Probability Verified", {
  res <- observation_probability(
    data.frame('counts'=c(15,5),c(0.5,0.5))
  )

  expect_equal(round(res,4), 0.0414)

  res <- observation_probability(
    data.frame('counts'=c(19,1),c(0.99,0.01))
  )
  expect_equal(round(res,4), 0.1821)

  res <- observation_probability(
    data.frame('counts'=c(19,1),c(1,0))
  )
  expect_equal(res, 0)

  res <- observation_probability(
    data.frame('counts'=c(20,0),c(1.95, 0.05))
  )
  expect_equal(res, 1)
})
