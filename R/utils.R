# Validate grouped numeric data used by the analysis functions.
.validate_alpha <- function(alpha) {
  if (length(alpha) != 1 ||
      !is.numeric(alpha) ||
      !is.finite(alpha) ||
      alpha <= 0 ||
      alpha >= 1) {
    stop("`alpha` must be strictly between 0 and 1.", call. = FALSE)
  }

  alpha
}

.validate_tests <- function(tests, allowed) {
  if (!is.character(tests) || length(tests) < 1) {
    stop("`tests` must contain one or more test names.", call. = FALSE)
  }

  tests <- tolower(tests)
  allowed <- tolower(allowed)

  if (any(!tests %in% allowed)) {
    stop(
      paste0("Unknown test. Choose from: ",
             paste(allowed, collapse = ", "), "."),
      call. = FALSE
    )
  }

  tests
}

.validate_group_data <- function(data, min_group_size = 2) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("`data` must be a data.frame or matrix.", call. = FALSE)
  }

  if (ncol(data) != 2) {
    stop("The parameter data must be a 2-column data.frame / matrix.",
         call. = FALSE)
  }

  if (!is.numeric(data[, 1])) {
    stop("The first column of `data` must be numeric.", call. = FALSE)
  }

  if (anyNA(data[, 1]) || anyNA(data[, 2])) {
    stop("`data` cannot contain missing values in its value or group columns.",
         call. = FALSE)
  }

  group_counts <- table(data[, 2])
  if (any(group_counts < min_group_size)) {
    stop(
      paste0("Each group must contain at least ", min_group_size,
             " observations."),
      call. = FALSE
    )
  }

  data
}

.validate_group_variances <- function(data) {
  variances <- tapply(data[, 1], data[, 2], stats::var)
  if (any(!is.finite(variances)) || any(variances <= 0)) {
    stop("Each group must have a positive, finite variance.", call. = FALSE)
  }

  variances
}

#' Prepare Data Utility
#'
#' @param data Data.frame with the first column the values and the second column
#'  the group names
#'
#' @returns List with data, groups, and formula
.prepare_data <- function(data) {
  ## General Info
  data <- .validate_group_data(data)

  col_names <- colnames(data)
  if (is.null(col_names)) {
    colnames(data) <- c("value", "group")
    col_names <- colnames(data)
  }
  data[, 2] <- as.factor(data[, 2])
  groups <- unique(data[, 2])

  form <- stats::reformulate(
    termlabels = col_names[2],
    response = col_names[1]
  )

  list(
    "data" = data,
    "groups" = groups,
    "form" = form
  )
}


#' Summarize Results From PMCMRplus Without Outputs
#'
#' @param x PMCMRplus data
#' @param alpha significance in \[0, 1\]
#'
#' @returns Summary data.frame
.PMCMRplus_summary <- function(x, alpha) {
  # Get p-values
  pval <- as.numeric(x$p.value)
  grp1 <- as.numeric(c(col(x$p.value)))
  cnam <- colnames(x$p.value)
  grp2 <- as.numeric(c(row(x$p.value)))
  rnam <- rownames(x$p.value)
  H0 <- paste(cnam[grp1], "-", rnam[grp2], sep = "")
  OK <- !is.na(pval)
  ppval <- pval[OK]
  names(ppval) <- H0[OK]

  # Get Table
  out.mcv <- multcompView::multcompLetters(ppval, threshold = alpha)
  dat <- x$model
  xmean <- tapply(dat$x, dat$g, mean)
  xn <- tapply(dat$x, dat$g, length)
  xsd <- tapply(dat$x, dat$g, stats::sd)
  xdf <- data.frame(
    round(xmean, 3), round(xsd, 3), xn,
    out.mcv$Letters
  )
  rownames(xdf) <- c(colnames(x$statistic)[1], rownames(x$statistic))
  names(xdf) <- c("mean", "sd", "n", "Sig. group")

  xdf
}
