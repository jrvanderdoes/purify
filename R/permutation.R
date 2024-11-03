
#' Resample and apply data
#'
#' Resample data and compute the related values from some function. This
#'  function is a convenient an extension of \code{resample()} and a similar
#'  function can be constructed using \code{resample()} when
#'  \code{resample_function()} is insufficient. The code permutes every
#'  column except the first ('output'), then applies the function on
#'  the permuted sample with the (non-permuted) output.
#'
#' @param fn Function. Function to use for each permuted sample. The output
#'  should be a data.frame with a row for each variable and the first column
#'  being the estimates.
#'
#' @param X Data.frame of to be resampled where the rows are the observations
#'  and the columns are the variables. Note, all variables except the first are
#'  permuted, where the first is used for the models given in fn.
#' @param fn Function to apply on each resampled data set. It should return
#'  a data,frame with the first column at the variable estimates (row as
#'  variables).
#' @param ... This is for the parameters of the resampling. See parameters of
#' \code{resample()}, if not given, the defaults are used.
#'
#' @return A list of items. The first is the results as a dataframe. It has the
#'  following columns with a row per variable.
#'  \itemize{
#'    \item estimates: Mean of the parameter estimates from fn over the
#'      resampled trials.
#'    \item sd: Standard deviations of the estimates from \code{fn} over the
#'      resampled trials.
#'    \item lowerNorm: Lower confidence interval assuming normality using alpha.
#'    \item upperNorm: Upper confidence interval assuming normality using alpha.
#'    \item lowerResamp: Lower confidence interval from data using alpha.
#'    \item upperResamp: Upper confidence interval from data using alpha.
#'  }
#'  The second entry is a list of the resampled data and estimates for reference.
#'  \itemize{
#'    \item samples: A list of the resampled data.
#'    \item function_values: A list of the function output values from the
#'      resampled data. Note, this includes those not used for the estimates.
#'    \item estimates: A data frame of the first column of each element in
#'      function_values. The row means are those used in \code{meanParams}.
#'  }
#'
#' @export
#'
#' @examples
resample_function <- function(X, fn, alpha=0.05, ...){
  X_M <- resample(X, ...)

  bs_values <- sapply(X_M,
                      function(X_i,fn, Y, .colnams, ...){
                        fn(X=X_i, ...) },
                      fn=fn, Y=X[,1], simplify = F)

  # Results
  estims <- data.frame('V1'=bs_values[[1]][,1])

  for(i in 2:length(bs_values)){
    estims[,i] <- bs_values[[i]][,1]
  }

  ## Organize overall bootstrapped estimates
  meanParms <- rowMeans(estims, na.rm = T)
  meanParamsSD <- apply(estims,MARGIN = 1, sd )
  results <- data.frame(
    'estimates' = meanParms,
    'sd' = meanParamsSD,
    'lowerNorm'= meanParms + qnorm(alpha/2)*meanParamsSD,
    'upperNorm'= meanParms - qnorm(alpha/2)*meanParamsSD,
    'lowerResamp'= apply(estims,MARGIN = 1,
                    function(x,y){ quantile(x,probs = c(y))}, y=alpha/2),
    'upperResamp'= apply(estims,MARGIN = 1,
                    function(x,y){ quantile(x,probs = c(y))}, y=1-alpha/2)
    )

  list(
    "estimates" = results,
    'permutation'=list(
      'samples'=X_M,
      'function_values'=bs_values,
      'estimates'=list('parameters'=estims)
    )
  )
}


#' Resample data
#'
#' Resample data for use in estimation or models.
#'
#' @param X Data.frame of to be resampled where the rows are the observations
#'  and the columns are the variables. Note, all variables are permuted, so be
#'  sure to remove the output variable if you wish to model it so that it is not
#'  permuted with the data.
#' @param M Numeric. Number of permutation iterations.
#' @param type String indicating type of permutation test. Options include
#'  'simple', 'stratify', 'sliding' and 'segment'. Simple permutes the entire
#'  data set at random. Stratify  segments the data based on some stratification
#'  variable, \code{stratify}, and resamples from each group. The groups are
#'  sampled to produce evenly sized groups of size \code{size}/number of groups.
#'  Sliding and segment take blocks of the data, thereby preserving some
#'  dependence. Sliding take sliding groups, thus repeating values, i.e. 1-4,
#'  2-5, 3-6, and so on. Segment does not repeat the values, i.e. 1-4, 5-8,
#'  9-12, and so on.
#' @param size Numeric for the size of the resampled data. Typically the same of
#'  the original data set. However, when type='stratify' and a group is
#'  extremely small, it may be valuable to decrease the size.
#' @param replace Boolean. Indicates if the data should be permuted (FALSE) or
#'  bootstrapped (TRUE). If the size to too large, permuting may be impossible.
#' @param blockSize Numeric for the size of the blocks when using \code{type} of
#'  sliding or segment. Otherwise it is unused.
#' @param strata String or numeric. This indicate the column to stratify the
#'  data when \code{type} is stratify. This can be the column number or the
#'  column name.
#'
#' @return List of resampled data sets.
#'
#' @export
#'
#' @examples
resample <- function(X, M=1000,
                     type = c('simple','stratify','sliding','segment'),
                     size = nrow(X),
                     replace=TRUE,
                     blockSize=1, strata=NULL){
  types_poss <- c('simple','stratify','sliding','segment')
  type <- types_poss[min(pmatch(type,types_poss))]
  # TODO:: Verify input

  n <- nrow(X)

  if(type=='simple'){
    if(size>n && !replace) stop('The variable size is too large to permute.',
                                call. = FALSE)

    X_resampled <- sapply(1:M, function(i,n,X1,replace=replace){
      X1[sample(1:n,replace = replace,size = size),]
    },n=n, replace = replace, X1=X, simplify = FALSE)

  } else if(type=='sliding'){
    # TODO:: Add error check

    idxGroups <- sapply(0:(n-blockSize), function(x,blockSize){
      x + 1:blockSize
    }, blockSize=blockSize, simplify = FALSE)

    # Get BS sample indices
    idxs <- sapply(1:M, function(i, m, indxs, replace, size1, n) {
      samps <- sample(x=1:m, size=size1, replace = replace)
      unlist(indxs[samps], use.names = FALSE)[1:n]
    }, m = length(idxGroups), indxs = idxGroups, replace = replace,
    size1=ceiling(size / blockSize), n=n)

    if (!(is.matrix(idxs) | is.data.frame(idxs))) {
      warning('Check this')
      idxs <- .convertSamplesToDF(idxs)
    }

    X_resampled <- sapply(as.data.frame(idxs),
                          function(loop_iter, Xdata) {
                            Xdata[stats::na.omit(loop_iter),]
                          }, Xdata=X,simplify = F
    )
  } else if(type=='segment'){
    # TODO:: Add error check

    idxGroups <- .getChunks(1:n, n / blockSize)

    # Get BS sample indices
    idxs <- sapply(1:M, function(i, m, indxs, replace, size1, n) {
      samps <- sample(x=1:m, size=size1, replace = replace)
      unlist(indxs[samps], use.names = FALSE)[1:n]
    }, m = length(idxGroups), indxs = idxGroups, replace = replace,
    size1=ceiling(size / blockSize), n=n)

    if (!(is.matrix(idxs) | is.data.frame(idxs))) {
      warning('Check this')
      idxs <- .convertSamplesToDF(idxs)
    }

    X_resampled <- sapply(as.data.frame(idxs),
                          function(loop_iter, Xdata) {
                            Xdata[stats::na.omit(loop_iter),]
                          }, Xdata=X, simplify = F
    )

  } else if(type=='stratify') {
    # TODO:: Add error check

    # X[[strata]] <- paste0(X[[strata]],'A')

    groups <- as.data.frame(table(X[[strata]]))
    # groups <- unique(X[[strata]])
    # group_sizes <- as.numeric(table(X[[strata]]))

    min_size <- floor(size/nrow(groups))

    # Group Strata together
    X_stacked_stata <- data.frame()
    for(i in 1:nrow(groups)){
      X_stacked_stata <- rbind(X_stacked_stata, X[X[[strata]]==groups[i,'Var1'],])
    }
    rownames(X_stacked_stata) <- NULL

    ## Stratified sampling
    X_resampled <- sapply(1:M,
                          function(i, n, X1, min_size=min_size,
                                   replace=replace, groups=groups){
      return_idxs <-c()
      group_sizes_tmp <- c(0, cumsum(groups$Freq))
      for(i in 2:length(group_sizes_tmp)){
        return_idxs <- c(return_idxs,
                         sample((group_sizes_tmp[i-1]+1):group_sizes_tmp[i],
                                size=min_size, replace = replace))
      }

      X1[return_idxs,]
    }, n=n, min_size=min_size, replace=replace, groups=groups,
    X1=X_stacked_stata, simplify=FALSE)

  } else{
    stop('Check `type` variable', call. = FALSE)
  }

  X_resampled
}


#' Get Chunks
#'
#' This (internal) function splits the vector x into a \code{chunksN} number of
#'     subsegments. The values of x are kept in order (i.e. not scrambled).
#'
#' @param x Vector of values
#' @param chunksN Numeric indicating the number of chunks to split X into
#'
#' @return A list with chunksN items, each containing an similiar sized subset
#'     of the original vector
#'
#' @keywords internal
#'
#' @examples
#' .getChunks(1:100, 1)
#' .getChunks(1:100, 2)
#' .getChunks(1:100, 5)
.getChunks <- function(x, chunksN) {
  chunksN <- round(chunksN)

  if (chunksN < 2) {
    return(x)
  }
  split(x, cut(x, chunksN, labels = FALSE))
}


#' Convert List of Samples into a Data Frame
#'
#' This (internal) function takes a list with differ length data.frames or
#'  vectors and pads them all to make a clean data.frame.
#'
#' This is an internal function and will not be viewable to user. See
#'  generalized_resampling for usage
#' @param data_list List of elements to be combined to a data.frame.
#'
#' @return Data.frame of the data in data_list
#'
#' @keywords internal
.convertSamplesToDF <- function(data_list) {
  m <- length(data_list)
  maxLen <- 0
  for (ii in 1:m) {
    maxLen <- max(maxLen, length(data_list[[ii]]))
  }

  data_df <- data.frame(matrix(nrow = maxLen, ncol = m))

  for (ii in 1:length(data_list)) {
    data_df[, ii] <- c(
      data_list[[ii]],
      rep(NA, maxLen - length(data_list[[ii]]))
    )
  }

  data_df
}

