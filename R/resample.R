#' Resample data
#'
#' Function to allow various resampling of data, and application to a function
#'  as desired. Choices for simple, stratified, and block (both sliding and
#'  separate) resampling. Bootstrap and permutation resampling can be performed.
#'
#' @param data Data.frame (or vector) to be resampled where the rows are the
#'  observations and the columns are the variables. Note, all variables are
#'  permuted not specified in \code{ignore.columns}.
#' @param M Numeric. Number of permutation iterations.
#' @param resample_blocks String indicating method of selecting resample blocks.
#'  Options are 'separate' and 'sliding'. When \code{blocksize} is 1, there is no
#'  difference. Sliding take sliding groups, thus repeating values, i.e. 1-4,
#'  2-5, 3-6, and so on. Separate does not repeat the values, i.e. 1-4, 5-8,
#'  9-12, and so on. Size of each block is defined using \code{blocksize}.
#' @param replace Boolean. Indicates if the data should be permuted (FALSE) or
#'  bootstrapped (TRUE). Note that permutation may be impossible with large
#'  \code{stratify_sizes}.
#' @param blocksize Numeric for the size of the blocks.
#' @param strata String or numeric. This indicate the column to stratify the
#'  data when \code{method} is stratify. This can be the column number or the
#'  column name. When NULL the data is not stratified. When given, strata are
#'  sampled separately.
#' @param stratify_sizes Option for selecting the sizes of each stata. Can be
#'  numeric (single value or a value for each strata), function (e.g. min or
#'  max), or NULL (original sizes).
#' @param fn Function to apply on the resampled data. When NULL, the resampled
#'  data are directly returned.
#' @param ignore.columns Name or column numbers to ignore when resampling data.
#'  These are not permuted. Note that if less/more samples are collected than
#'  the original, these are permuted separately.
#' @param ... Additional parameters for \code{fn}.
#'
#' @return Resampled data sets or function applied to resampled data.
#'
#' @export
#'
#' @examples
#' results <- resample(1:100)
#' results <- resample(1:100, fn = mean)
#'
#' n <- 50
#' data <- data.frame(
#'   output = NA,
#'   predictor1 = rnorm(n),
#'   predictor2 = rnorm(n)
#' )
#' data$output <- 2 * data$predictor1 - data$predictor2 + stats::rnorm(nrow(data))
#' results <- resample(data, ignore.columns = "output")
#'
#' fn <- function(data) {
#'   coef(lm(output ~ ., data = data))
#' }
#' # Warning, M is below recommendation for example speed
#' results <- resample(data = data, fn = fn, M = 10, ignore.columns = "output")
resample <- function(data, M = 1000,
                     resample_blocks = c("separate", "sliding"),
                     replace = TRUE, blocksize = 1, strata = NULL,
                     stratify_sizes = NULL, fn = NULL,
                     ignore.columns = NULL, ...) {
  # Setup and Verify Input
  types_poss <- c("separate", "sliding")
  resample_blocks <- types_poss[min(pmatch(resample_blocks, types_poss))]
  M <- round(M)
  if (M <= 0) stop("The variable `M` must be positive", call. = FALSE)
  # size <- round(size)
  # if(size <= 0) stop('The variable `size` must be positive', call. = FALSE)

  n <- nrow(data)
  data.vector <- FALSE
  if (is.null(n)) {
    n <- length(data)
    data.vector <- TRUE
  }

  ## Resample
  if (is.null(strata)) {
    # Get groups
    if (resample_blocks == "separate") {
      idxGroups <- .getChunks(1:n, n / blocksize)
    } else if (resample_blocks == "sliding") {
      idxGroups <- sapply(0:(n - blocksize), function(x, blocksize) {
        x + 1:blocksize
      }, blocksize = blocksize, simplify = FALSE)
    }

    if (data.vector) {
      X_resampled <-
        sapply(1:M, function(i, n, X1, replace = replace, blocksize = blocksize) {
          min_group <- min(sapply(X1, length))
          size_val <- ifelse(!replace, length(X1), ceiling(n / min_group) + 1)

          unlist(sample(X1, replace = replace, size = size_val))[1:n]
          # unlist(X1[sample(1:length(X1),replace = replace,
          #                         size = size_val)])[1:n]
        }, n = n, replace = replace, X1 = idxGroups, blocksize = blocksize, simplify = FALSE)
    } else {
      X_resampled <-
        sapply(1:M, function(i, n, X1, replace = replace, blocksize = blocksize,
                             idxGroups = idxGroups, ignore.columns = ignore.columns) {
          min_group <- min(sapply(idxGroups, length))
          size_val <- ifelse(!replace, length(idxGroups), ceiling(n / min_group) + 1)

          data_resample <- X1[
            sample(1:length(idxGroups),
              replace = replace,
              size = size_val
            ),
            !(colnames(X1) %in% ignore.columns)
          ]
          data_resample <- data_resample[1:n, ]

          if (!is.null(ignore.columns)) {
            data_resample <- cbind(data[ignore.columns], data_resample)
          }
          rownames(data_resample) <- NULL
          data_resample
        },
        n = n, replace = replace, X1 = data, idxGroups = idxGroups, blocksize = blocksize,
        ignore.columns = ignore.columns, simplify = FALSE
        )
    }
  } else {
    groups <- as.data.frame(table(data[[strata]]))

    # Group Strata together
    X_stacked_stata <- data.frame()
    for (i in 1:nrow(groups)) {
      X_stacked_stata <- rbind(X_stacked_stata, data[data[[strata]] == groups[i, "Var1"], ])
    }
    rownames(X_stacked_stata) <- NULL

    # Stratify sizes, based on given number, function, or leaving original
    sizes <- groups
    if (is.numeric(stratify_sizes)) {
      sizes$Freq <- stratify_sizes
    } else if (is.function(stratify_sizes)) {
      sizes$Freq <- stratify_sizes(sizes$Freq)
    }


    # Get groups
    idxGroups <- list()
    if (resample_blocks == "separate") {
      for (n_ind in 1:nrow(sizes)) {
        idxGroups[[n_ind]] <- .getChunks(1:groups$Freq[n_ind], groups$Freq[n_ind] / blocksize)
      }
    } else if (resample_blocks == "sliding") {
      for (n_ind in 1:nrow(sizes)) {
        idxGroups[[n_ind]] <-
          sapply(0:(groups$Freq[n_ind] - blocksize), function(x, blocksize) {
            x + 1:blocksize
          }, blocksize = blocksize, simplify = FALSE)
      }
    }

    ## Stratified sampling
    X_resampled <- sapply(1:M,
      function(i, n, X1, sizes = sizes, idxGroups = idxGroups,
               replace = replace, groups = groups,
               ignore.columns = ignore.columns) {
        return_idxs <- c()
        group_sizes_tmp <- c(0, cumsum(groups$Freq))
        for (j in 2:length(group_sizes_tmp)) {
          return_idxs <-
            c(
              return_idxs,
              group_sizes_tmp[j - 1] +
                unlist(sample(idxGroups[[j - 1]],
                  size = sizes$Freq[j - 1],
                  replace = replace
                ))[1:sizes[j - 1, 2]]
            )
        }

        ## Add back the ignore columns
        data_resample <- X1[
          return_idxs,
          !(colnames(X1) %in% ignore.columns)
        ]
        if (!is.null(ignore.columns)) {
          if (nrow(data_resample) == nrow(data)) {
            data_resample <- cbind(data[ignore.columns], data_resample)
          } else {
            tmp <- matrix(NA,
              ncol = length(ignore.columns),
              nrow = nrow(data_resample)
            )
            colnames(tmp) <- ignore.columns
            data_resample <- cbind(tmp, data_resample)
            for (j in 1:nrow(groups)) {
              idxs <- data_resample[[strata]] == groups$Var1[j]
              idxs_orig <- data[[strata]] == groups$Var1[j]
              # tmp <- data[idxs_orig,
              #             ignore.columns,drop=FALSE]
              if (sum(idxs) != sum(idxs_orig)) {
                if (sum(idxs) > sum(idxs_orig)) {
                  # resample longer than original
                  add_dat <- data[idxs_orig,
                    ignore.columns,
                    drop = FALSE
                  ]
                  add_dat <- add_dat[
                    sample(1:sum(idxs_orig),
                      size = sum(idxs),
                      replace = TRUE
                    ), ,
                    drop = FALSE
                  ]
                } else {
                  # resample shorter than original
                  add_dat <- data[idxs_orig,
                    ignore.columns,
                    drop = FALSE
                  ]
                  add_dat <- add_dat[sample(1:sum(idxs)), , drop = FALSE]
                }
              }

              data_resample[
                idxs,
                ignore.columns
              ] <- add_dat
            }
          }
        }
        rownames(data_resample) <- NULL
        data_resample
      },
      n = n, sizes = sizes, replace = replace, groups = groups,
      idxGroups = idxGroups, X1 = X_stacked_stata,
      ignore.columns = ignore.columns, simplify = FALSE
    )
  }


  ## Apply fn if given
  if (!is.null(fn)) {
    result <- lapply(X_resampled, fn, ...)

    if (is.list(result)) {
      result1 <- data.frame() # t(data.frame(result[[1]]))
      for (i in 1:length(result)) {
        result1 <- rbind(result1, t(data.frame(result[[i]])))
      }
      result <- result1
      rownames(result) <- NULL
    }
  } else {
    result <- X_resampled
  }

  result
}


#' Get Chunks
#'
#' This (internal) function splits the vector x into a \code{chunksN} number of
#'     subsegments. The values of x are kept in order (i.e. not scrambled).
#'
#' @param x Vector of values
#' @param chunksN Numeric indicating the number of chunks to split data into
#'
#' @return A list with chunksN items, each containing an similiar sized subset
#'     of the original vector
#'
#' @noRd
#' @keywords internal
.getChunks <- function(x, chunksN) {
  chunksN <- round(chunksN)

  if (chunksN < 2) {
    return(x)
  }
  split(x, cut(x, chunksN, labels = FALSE))
}
