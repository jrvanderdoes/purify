test_that("ANOVA-like Tests", {
  set.seed(1232)
  data1 <- data.frame(
    "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
    "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
  )
  tmp <- anova_tests(data1)

  expect_equal(length(tmp), 7)
  expect_equal(round(tmp$anova, 3), 0)

  tmp <- anova_tests(data1, tests = "anova")
  expect_equal(length(tmp), 3)
})


test_that("Resample ANOVA", {
  set.seed(1232)
  data1 <- data.frame(
    "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
    "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
  )
  expect_equal(round(resample_welch_anova(data1), 3), 0.001)

  set.seed(1232)
  data1 <- data.frame(
    "value" = c(rnorm(20, sd = 4), rnorm(10), rnorm(15, sd = 2)),
    "group" = c(rep("A", 20), rep("B", 10), rep("C", 15))
  )
  expect_equal(round(resample_welch_anova(data1), 3), 0.839)
})

test_that("resample_welch_anova uses var.equal for the observed statistic", {
  dat <- data.frame(
    value = c(1, 2, 3, 10, 20, 30),
    group = rep(c("A", "B"), each = 3)
  )

  M <- 100

  set.seed(123)
  result <- resample_welch_anova(
    dat,
    var.equal = TRUE,
    M = M
  )

  # Reproduce the function's null simulation.
  grp_unique <- unique(dat[, 2])
  means <- tapply(dat[, 1], dat[, 2], mean)

  data_null <- dat
  data_null[, 1] <- data_null[, 1] - means[dat[, 2]]

  set.seed(123)

  simulated_statistics <- sapply(seq_len(M), function(i) {
    simulated_data <- data.frame()

    for (group_name in grp_unique) {
      simulated_data <- rbind(
        simulated_data,
        data.frame(
          value = sample(
            data_null[data_null[, 2] == group_name, 1],
            replace = TRUE
          ),
          group = group_name
        )
      )
    }

    stats::oneway.test(
      value ~ group,
      data = simulated_data,
      var.equal = TRUE
    )$statistic
  })

  observed_statistic <- stats::oneway.test(
    value ~ group,
    data = dat,
    var.equal = TRUE
  )$statistic

  expected_result <- mean(
    observed_statistic <= simulated_statistics
  )

  expect_equal(result, expected_result)
})

test_that("resample_welch_anova validates M", {
  dat <- data.frame(value = 1:6, group = rep(c("A", "B"), each = 3))

  expect_error(
    resample_welch_anova(dat, M = 1.5),
    "`M` must be a positive integer"
  )
})

test_that("resample_welch_anova validates grouped data and var.equal", {
  dat <- data.frame(value = 1:6, group = rep(c("A", "B"), each = 3))

  expect_error(
    resample_welch_anova(dat, var.equal = NA),
    "single TRUE or FALSE"
  )
  expect_error(
    resample_welch_anova(dat[c(1, 4), ], M = 1),
    "at least 2 observations"
  )
})

test_that("resample_welch_anova rejects zero-variance groups", {
  dat <- data.frame(value = c(1, 1, 2, 3),
                    group = rep(c("A", "B"), each = 2))

  expect_error(
    resample_welch_anova(dat, M = 1),
    "positive, finite variance"
  )
})

test_that("analysis functions handle non-syntactic column names", {
  dat <- data.frame(
    `response value` = c(1, 2, 3, 4, 5, 6),
    `group condition` = rep(c("A", "B"), each = 3)
  )

  expect_no_error(
    anova_tests(dat, tests = "anova")
  )
})

test_that("grouped analysis functions validate missing and sparse data", {
  missing_data <- data.frame(
    value = c(1, NA, 3, 4),
    group = c("A", "A", "B", "B")
  )

  sparse_data <- data.frame(
    value = c(1, 2, 3),
    group = c("A", "B", "B")
  )

  expect_error(
    anova_tests(missing_data, tests = "anova"),
    "cannot contain missing values"
  )
  expect_error(
    anova_tests(sparse_data, tests = "anova"),
    "Each group must contain at least 2 observations"
  )
})

test_that("anova_tests rejects unknown test names", {
  dat <- data.frame(value = 1:6, group = rep(c("A", "B"), each = 3))

  expect_error(
    anova_tests(dat, tests = "unknown"),
    "Unknown test"
  )
})
