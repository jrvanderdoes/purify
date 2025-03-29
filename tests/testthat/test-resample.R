test_that("Resample check", {
  set.seed(1234)
  results <- resample(data = 1:100, replace=FALSE)
  expect_equal( mean(sapply(results, mean)), 50.5)

  set.seed(1234)
  results1 <- resample(data = 1:100, fn=mean, resample_blocks='sliding', blocksize=3)
  tmp <- summarize_resample(results1)
  expect_equal( round(tmp$estimates,4), 50.4814)

  set.seed(1234)
  n <- 50
  data <- data.frame(
    output = NA,
    predictor1 = rnorm(n),
    predictor2 = rnorm(n)
  )
  data$group <- as.factor(rbinom(nrow(data),1,0.5))
  data$output <- 2*data$predictor1-data$predictor2+stats::rnorm(nrow(data))
  set.seed(1234)
  results0 <- resample(data, strata='group', resample_blocks='sliding',
                      blocksize=2, ignore.columns='output', stratify_sizes=20)
  expect_equal(round(sum(results0[[1]]$output),3), -33.85)

  set.seed(1234)
  results00 <- resample(data, strata='group', resample_blocks='sliding',
                      blocksize=2, ignore.columns='output', stratify_sizes=100)
  expect_equal(round(sum(results00[[1]]$output),3), -142.369)

})
