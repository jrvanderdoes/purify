test_that("Confidence Intervals works", {
  set.seed(123)
  data <- cumsum(rnorm(150))
  pred_model <- function(x, h) {
    predict(forecast::ets(x), h = h)$mean
  }
  h <- 10
  train <- 0.8
  M <- 1000
  alpha <- 0.05
  output <- NULL

  ets_model <- predict(forecast::ets(data[1:140]), h)
  ints <- confidence_intervals(data = data[1:140], pred_model = pred_model, h = 10)

  # plot(ets_model)
  # lines(x=141:150,y=ints$Lower, col='red')
  # lines(x=141:150,y=ints$Upper, col='red')
  expect_equal(round(ints$lower[1], 3), -0.686)
  expect_equal(round(ints$upper[3], 3), 3.672)

  ###################

  set.seed(1234)
  data <- data.frame("y" = NA, "x" = 1:150)
  data$y <- 2 * c(0, data$x[-150]) + rnorm(150)
  pred_model <- function(data, h) {
    as.numeric(predict(lm(y ~ x, data = data), newdata = data.frame("x" = nrow(data) + 1:h)))
  }
  h <- 8

  lm_model <- predict(lm(y ~ x, data = data[1:(150 - h), ]), newdata = data.frame("x" = (150 - h + 1):150))
  ints <- confidence_intervals(
    data = data[1:(150 - h), ], pred_model = pred_model,
    train = train, h = h, M = 750, alpha = 0.1,
    output = "y"
  )

  expect_equal(round(ints$forecasts[2], 3), 286.137)
  expect_equal(round(ints$lower[4], 3), 288.506)
  expect_equal(ints$upper[9], as.numeric(NA))

  # plot(x=1:nrow(data), y=data$y, type='l')
  # lines(x=(150-h+1):150,y=lm_model, col='blue')
  # lines(x=(150-h+1):150,y=ints$lower, col='red')
  # lines(x=(150-h+1):150,y=ints$upper, col='red')
})

test_that("confidence_intervals passes extra arguments for data frames", {
  dat <- data.frame(value = 1:20)

  pred_model <- function(x, h, offset) {
    rep(offset, h)
  }

  expect_no_error(
    confidence_intervals(
      data = dat,
      pred_model = pred_model,
      h = 2,
      train = 0.5,
      M = 10,
      output = "value",
      offset = 0
    )
  )
})

test_that("confidence intervals handles fractional training sizes", {
  dat <- data.frame(value = 1:10)

  pred_model <- function(x, h) {
    rep(mean(x[[1]]), h)
  }

  expect_no_error(
    confidence_intervals(
      data = dat,
      pred_model = pred_model,
      h = 2,
      train = 0.75,
      M = 10,
      output = "value"
    )
  )
})

test_that("confidence_intervals handles a single forecast horizon", {
  dat <- data.frame(value = 1:10)

  pred_model <- function(x, h) {
    rep(mean(x[[1]]), h)
  }

  expect_no_error(
    confidence_intervals(
      data = dat,
      pred_model = pred_model,
      h = 1,
      train = 0.5,
      M = 10,
      output = "value"
    )
  )
})

test_that("confidence_intervals validates h, M, and alpha", {
  pred_model <- function(x, h) {
    rep(0, h)
  }

  expect_error(
    confidence_intervals(1:10, pred_model, h = 0, M = 10),
    "`h` must be a positive integer"
  )
  expect_error(
    confidence_intervals(1:10, pred_model, h = 1.5, M = 10),
    "`h` must be a positive integer"
  )
  expect_error(
    confidence_intervals(1:10, pred_model, h = 1, M = 0),
    "`M` must be a positive integer"
  )
  expect_error(
    confidence_intervals(1:10, pred_model, h = 1, M = 10, alpha = 0),
    "`alpha` must be strictly between 0 and 1"
  )
})

test_that("confidence_intervals rejects settings with no validation windows", {
  pred_model <- function(x, h) {
    rep(0, h)
  }

  expect_error(
    confidence_intervals(
      data = 1:10,
      pred_model = pred_model,
      train = 0.9,
      h = 2,
      M = 10
    ),
    "leaves no validation windows"
  )
})

test_that("confidence_intervals validates train", {
  pred_model <- function(x, h) {
    rep(0, h)
  }

  expect_error(
    confidence_intervals(1:10, pred_model, h = 1, M = 10, train = NA_real_),
    "train.*between 0 and 1"
  )
  expect_error(
    confidence_intervals(1:10, pred_model, h = 1, M = 10, train = c(0.5, 0.6)),
    "train.*between 0 and 1"
  )
  expect_error(
    confidence_intervals(1:10, pred_model, h = 1, M = 10, train = 1.1),
    "train.*between 0 and 1"
  )
})
