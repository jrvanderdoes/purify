#' Summarize Resampled Data
#'
#' Summarize the results of function calls from resampled data. Such data is
#'  returned from \code{resample()} when \code{fn()} is given.
#'
#' @param data Data.frame of resampled data from [resample()].
#' @param alpha Significance level for confidence intervals.
#'
#' @return A dataframe. It has rows for each column in data. The columns are:
#'  \itemize{
#'    \item estimates: Mean of the parameter resampled estimates.
#'    \item sd: Standard deviations of the resampled estimates.
#'    \item lower: Lower confidence interval from empirical quantiles of the
#'      resampled data.
#'    \item upper: Upper confidence interval from empirical quantiles of the
#'      resampled data.
#'  }
#'
#' @export
#'
#' @examples
#' n <- 50
#' data <- data.frame(
#'   output = rnorm(n),
#'   predictor1 = rnorm(n),
#'   predictor2 = rnorm(n)
#' )
#'
#' fn <- function(data) { coef(lm(output ~ ., data = data)) }
#' # Warning, M is below recommendation for example speed
#' resample_data <- resample(data=data, fn=fn, M=10, ignore.columns='output')
#' results <- summarize_resample(resample_data)
summarize_resample <- function(data, alpha=0.05){

  # Results
  cnames <- colnames(data)
  # Estimate values
  meanParms <- colMeans(data, na.rm = TRUE)
  meanParamsSD <- apply(data, MARGIN = 2, stats::sd )
  lowSamp <- apply(data, MARGIN = 2,
                   function(x,y){ stats::quantile(x, probs = c(y))}, y=alpha/2)
  upSamp <- apply(data, MARGIN = 2,
                  function(x,y){ stats::quantile(x,probs = c(y))}, y=1-alpha/2)

  ## Organize overall bootstrapped estimates
  # Z_alpha <- stats::qnorm(alpha/2)
  results <- data.frame(
    'estimates' = meanParms,
    'sd' = meanParamsSD,
    # 'lowerNorm'= meanParms + Z_alpha * meanParamsSD,
    # 'upperNorm'= meanParms - Z_alpha * meanParamsSD,
    'lower'= lowSamp,
    'upper'= upSamp
  )
  rownames(results) <- cnames

  results
}

