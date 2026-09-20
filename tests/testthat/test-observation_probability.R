test_that("Observation Probability Verified", {
  res <- observation_probability(
    data.frame("counts" = c(15, 5), c(0.5, 0.5))
  )

  expect_equal(round(res, 4), 0.0414)

  res <- observation_probability(
    data.frame("counts" = c(19, 1), c(0.99, 0.01))
  )
  expect_equal(round(res, 4), 0.1821)

  res <- observation_probability(
    data.frame("counts" = c(19, 1), c(1, 0))
  )
  expect_equal(res, 0)

  res <- observation_probability(
    data.frame("counts" = c(20, 0), c(1.95, 0.05))
  )
  expect_equal(res, 1)
})

test_that("observation probability handles fractional expected counts", {
  result <- observation_probability(
    data.frame(
      counts = c(1, 2),
      probability = c(0.5, 0.5)
    )
  )

  expect_true(is.numeric(result))
  expect_true(result >= 0)
  expect_true(result <= 1)
})

test_that("observation_probability validates counts and probabilities", {
  expect_error(
    observation_probability(data.frame(counts = c(10, -1), probs = c(1, 1))),
    "count column must contain nonnegative integers"
  )
  expect_error(
    observation_probability(data.frame(counts = c(10, 5), probs = c(-1, 2))),
    "probability column must contain nonnegative values"
  )
  expect_error(
    observation_probability(data.frame(counts = c(10.5, 5), probs = c(1, 1))),
    "count column must contain nonnegative integers"
  )
  expect_error(
    observation_probability(data.frame(counts = c(10, 5), probs = c(0, 0))),
    "probability column must contain nonnegative values"
  )
  expect_error(
    observation_probability(data.frame(counts = c(10, 5))),
    "2-column object"
  )
})
