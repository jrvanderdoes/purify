test_that("Resample check", {
  set.seed(1234)
  results <- resample(data = 1:100, replace = FALSE)
  expect_equal(mean(sapply(results, mean)), 50.5)

  set.seed(1234)
  results1 <- resample(data = 1:100, fn = mean, resample_blocks = "sliding", blocksize = 3)
  tmp <- summarize_resample(results1)
  expect_equal(round(tmp$estimates, 4), 50.4814)

  set.seed(1234)
  n <- 50
  data <- data.frame(
    output = NA,
    predictor1 = rnorm(n),
    predictor2 = rnorm(n)
  )
  data$group <- as.factor(rbinom(nrow(data), 1, 0.5))
  data$output <- 2 * data$predictor1 - data$predictor2 + stats::rnorm(nrow(data))
  set.seed(1234)
  results0 <- resample(data,
    strata = "group", resample_blocks = "sliding",
    blocksize = 2, ignore.columns = "output", sizes = 20
  )
  expect_equal(round(sum(results0[[1]]$output), 3), -33.85)

  set.seed(1234)
  results00 <- resample(data,
    strata = "group", resample_blocks = "sliding",
    blocksize = 2, ignore.columns = "output", sizes = 100
  )
  expect_equal(round(sum(results00[[1]]$output), 3), -142.369)

  set.seed(1234)
  dat <- data.frame(
    value = 1:6,
    id = letters[1:6]
  )
  results000 <- resample(
    dat,
    M = 1,
    replace = FALSE,
    resample_blocks = "separate",
    blocksize = 2
  )
  expect_equal(results000[[1]],
               data.frame('value'=c(3:6,1:2),
                          'id'=c(letters[3:6],letters[1:2])))

})

test_that("stratified ignored columns handle unchanged group sizes", {
  dat <- data.frame(
    value = 1:6,
    strata = rep(c("A", "B"), each = 3),
    id = letters[1:6]
  )

  set.seed(123)
  result <- resample(
    dat,
    M = 1,
    strata = "strata",
    sizes = c(3, 2),
    ignore.columns = "id"
  )[[1]]

  expect_equal(as.integer(table(result$strata)), c(3L, 2L))
  expect_false(anyNA(result$id))
  expect_setequal(result$id[result$strata == "A"], letters[1:3])
  expect_true(all(result$id[result$strata == "B"] %in% letters[4:6]))
})

test_that("stratified size functions can return one common size", {
  dat <- data.frame(
    value = 1:6,
    strata = rep(c("A", "B"), each = 3)
  )

  result <- resample(
    dat,
    M = 1,
    strata = "strata",
    sizes = mean
  )[[1]]

  expect_equal(as.integer(table(result$strata)), c(3L, 3L))

  unequal <- data.frame(
    value = 1:5,
    strata = c("A", "A", "A", "B", "B")
  )
  expect_warning(
    resample(unequal, M = 1, strata = "strata", sizes = mean),
    "rounding"
  )
  result <- suppressWarnings(
    resample(unequal, M = 1, strata = "strata", sizes = mean)
  )[[1]]
  expect_equal(as.integer(table(result$strata)), c(2L, 2L))
})

test_that("resample validates basic arguments", {
  expect_error(resample(1:10, M = 0), "`M` must be a positive integer")
  expect_error(resample(1:10, M = 2.5), "`M` must be a positive integer")
  expect_error(resample(1:10, blocksize = 0), "`blocksize` must be a positive integer")
  expect_error(resample(1:10, blocksize = 1.5), "`blocksize` must be a positive integer")
  expect_error(
    resample(1:10, resample_blocks = "invalid"),
    "`resample_blocks` must be either 'separate' or 'sliding'"
  )
  expect_error(resample(numeric(0)), "`data` must contain at least one observation")
  expect_error(
    resample(1:10, sizes = 0),
    "`sizes` must be one positive integer for non-stratified resampling"
  )
  expect_error(
    resample(1:10, sizes = 11, replace = FALSE),
    "`sizes` cannot exceed the number of observations"
  )

  dat <- data.frame(
    value = 1:6,
    strata = rep(c("A", "B"), each = 3)
  )
  expect_error(
    resample(dat, strata = "strata", sizes = c(1, 2, 3)),
    "one value per stratum"
  )
  expect_error(
    resample(dat, strata = "missing"),
    "`strata` must refer to an existing column"
  )
  expect_error(
    resample(dat, strata = 0),
    "`strata` must refer to an existing column"
  )
  expect_error(
    resample(
      dat,
      strata = "strata",
      resample_blocks = "sliding",
      blocksize = 4
    ),
    "`blocksize` cannot exceed the number of observations in any stratum"
  )
})
