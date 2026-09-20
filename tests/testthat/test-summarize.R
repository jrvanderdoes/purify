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

test_that("summarize_resample validates alpha", {
  expect_error(
    summarize_resample(data.frame(x = 1:3), alpha = 0),
    "strictly between 0 and 1"
  )
})

test_that("summarize_resample rejects empty or nonnumeric data", {
  expect_error(
    summarize_resample(data.frame(x = numeric(0))),
    "non-empty"
  )
  expect_error(
    summarize_resample(data.frame(x = c("a", "b"))),
    "numeric matrix"
  )
  expect_error(
    summarize_resample(data.frame(x = c(1, NA))),
    "missing or non-finite"
  )
})
