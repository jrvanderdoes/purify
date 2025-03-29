test_that("Cross-validation Tests", {
  set.seed(123)
  data <- cumsum(rnorm(150))
  pred_fn <- function(x_fit, x_pred){ predict(forecast::ets(data),h=length(x_pred))$mean }
  tmp <- cross_validation(data, pred_fn, cv_group_number = 10,
                          cv.dependent = TRUE, sliding.start = 100)
  expect_equal(round(tmp,3), 225.096)

  set.seed(123)
  data <- cumsum(rnorm(150))
  pred_fn <- function(x_fit, x_pred){ predict(forecast::ets(data),h=length(x_pred))$mean }
  tmp <- cross_validation(data, pred_fn, cv_group_number = 10,
                          cv.dependent = TRUE)
  expect_equal(round(tmp,3), 429.058)

  set.seed(1234)
  n <- 50
  data <- data.frame(
    Y = NA,
    X = rnorm(n)
  )
  data$Y <- 2*data$X + stats::rnorm(nrow(data))
  pred_fn <- function(x_fit, x_pred){
    as.numeric(predict(lm(Y~X,data=data[-c(1:10),]),data[1:10,]))
  }
  tmp <- cross_validation(data, pred_fn, cv_group_number = 10,
                          cv.dependent = FALSE)
  expect_equal(round(tmp,3), 52.875)
})
