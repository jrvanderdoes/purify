test_that("Summarize Resample", {
  set.seed(1234)
  # Define a custom function to calculate MSE
  mse_function <- function(data) {
    pred <- as.numeric(predict(
      glm(as.factor(Survived) ~ .,
        data = data,
        family = binomial(link = "logit")
      ),
      type = "response"
    ) > 0.5)
    mean(as.numeric(data$Survived != pred))
  }

  # Simple resampling
  results <- resample(
    data = titanic, fn = mse_function,
    M = 1000, strata = "Pclass"
  )

  tmp <- summarize_resample(results)
  expect_equal(round(tmp$estimates, 3), 0.226)
  expect_equal(dim(tmp), c(1, 4))
})
