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
