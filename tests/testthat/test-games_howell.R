test_that("Games Howell", {
  set.seed(1234)
  data1 <- data.frame(
    "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
    "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
  )
  tmp <- games_howell(data1)
  expect_equal(dim(tmp), c(3, 7))
  expect_equal(round(tmp[1, 1], 3), 1.027)
})

test_that("games_howell validates grouped data and alpha", {
  dat <- data.frame(
    value = c(1, 2, 3, 4),
    group = rep(c("A", "B"), each = 2)
  )

  expect_error(games_howell(dat, alpha = 0), "strictly between 0 and 1")
  expect_error(
    games_howell(transform(dat, value = c(1, NA, 3, 4))),
    "cannot contain missing values"
  )
})

test_that("games_howell rejects zero-variance groups", {
  dat <- data.frame(
    value = c(1, 1, 2, 3),
    group = rep(c("A", "B"), each = 2)
  )

  expect_error(games_howell(dat), "positive, finite variance")
})
