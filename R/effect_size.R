#' Effect Size Statistics
#'
#' Compute the Cohen's d and Hedges' g effect size statistics.
#'
#' @param x Vector of values for the first group (if y specified) or a data.frame
#'  with the first column the values and the second column the group names (no y).
#'  If using a data.frame, ensure only two groups are given.
#' @param y Vector of values for the second group (if x is a vector), otherwise
#'  NULL if x is a data.frame.
#' @param var.type String indicating the variance assumption. Options include
#'  'unequal' (default), 'pooled', 'x', or 'y'.
#' @param hedges.correction Boolean indicating if Hedges correction should be
#'  applied
#' @param hedges.approx Boolean if Hedges correction should be approximated or
#'  computed using the gamma function
#'
#' @returns Numeric value for the effect size
#' @export
#'
#' @references Cohen, J. (1988). Statistical Power Analysis for the Behavioral
#'  Sciences (2nd ed.). Routledge.
#' @references Hedges, Larry & Olkin, Ingram. (1985). Statistical Methods in
#'  Meta-Analysis. 10.2307/1164953.
#' @references Theriault, R., (2023). rempsyc: Convenience functions for
#'  psychology. *Journal of Open Source Software*, *8*(87), 5466.
#'
#' @examples
#' x <- rnorm(10, mean = 1, sd = 1)
#' y <- rnorm(40, mean = 3, sd = 10)
#' cohens_d(x, y, var.type = 'unequal')
#' cohens_d(x, y, var.type = 'pooled', hedges.correction=TRUE)
cohens_d <- function(x, y=NULL, var.type='unequal', hedges.correction=FALSE,
                     hedges.approx=TRUE) {

  if(is.null(y)){
    X_tmp <- x
    groups <- unique(x[,2])
    if(length(groups)!=2) stop("Cohen's d / Hedges' g only used for two groups")
    x <- X_tmp[X_tmp[,2]==groups[1],1]
    y <- X_tmp[X_tmp[,2]==groups[2],1]
  }

  if(is.null(y)) stop("Cohen's d / Hedges' g used for two groups - see documentation")

  lx <- length(x) - 1
  ly <- length(y) - 1

  if(is.null(var.type) || var.type == 'unequal'){
    var_est <- (stats::var(x) + stats::var(y))/2
  }else if(var.type=='pooled'){
    var_est <- (lx * stats::var(x) + ly * stats::var(y) ) / (lx + ly)
  }else if(var.type=='x'){
    var_est <- stats::var(x)
  }else if(var.type=='y'){
    var_est <- stats::var(y)
  }else{
    stop('Verify variance status')
  }

  est <- abs(mean(x) - mean(y)) / sqrt(var_est)

  if(hedges.correction) {
    df <- lx+ly
    if(hedges.approx) {
      est <- est * (1 - 3/(4 * (df)-1) )
    } else {
      est <- est * ( gamma(df/2) / ( sqrt(df/2) * gamma((df-1)/2) ) )
    }
  }

  est
}


#' Eta Squared
#'
#' Compute the Eta squared effect size statistic.
#'
#' @param data Data.frame with the first column the values and the second column
#'  the group names
#'
#' @returns Table with eta-squared attached to classic ANOVA decomposition
#' @export
#'
#' @references Navarro, D. J. (2015) Learning statistics with R: A tutorial for
#'  psychology students and other beginners. (Version 0.6) University of New
#'  South Wales. Sydney, Australia
#'
#' @examples
#' data = data.frame('value'=c(rnorm(14,sd = 2), rnorm(6), rnorm(20,mean=2)),
#'                   'group'=c(rep('A',14),rep('B',6), rep('C',20)))
#' eta_squared(data)
eta_squared <- function(data){

  ## General Info
  if(dim(data)[2]!= 2){
    stop('The parameter data must be a 2-column data.frame / matrix.')
  }else{
    col_names <- colnames(data)
    if(is.null(col_names)){
      colnames(data) <- c('value','group')
      col_names <- colnames(data)
    }
  }
  data[,2] <- as.factor(data[,2])
  groups <- unique(data[,2])

  ## ANOVA
  form <- stats::as.formula(paste(col_names[1], "~", col_names[2]))

  anova_res <- stats::aov(form,data)

  ## Eta^2
  eta2 <- stats::var(stats::predict(anova_res)) / stats::var(data$value)

  cbind('eta squared'=c(eta2, 1-eta2), summary(anova_res)[[1]])
}


#' Effect Size by Resampling Differences
#'
#' Use with caution as this is still being tested. This function generates
#'  confidence intervals for the mean differences between groups.
#'
#' @param data Data.frame with the first column the values and the second column
#'  the group names
#' @param alpha Significance for confidence intervals, defaults to 0.05
#' @inheritParams resample
#'
#' @returns Table with the mean differences, confidence intervals and other
#'  information
#' @export
#'
#' @examples
#' data <- data.frame('value'=c(rnorm(14,sd = 2), rnorm(6), rnorm(20,mean=2)),
#'                    'group'=c(rep('A',14),rep('B',6), rep('C',20)))
#' # Be sure to increase M for real use cases
#' resample_differences(data, M=50)
resample_differences <- function(data, alpha=0.05, M=1000) {
  grp <- data[,2]
  obs <- data[,1]

  # Group Combinations (Interactions)
  combs <- utils::combn(unique(grp), 2)

  # Summary Statistics
  ns <- tapply(obs, grp, length)
  groups <- length(ns)
  means <- tapply(obs, grp, mean)

  statistics <- sapply(1:ncol(combs),
                       function(x, ns, groups, means, alpha, M) {
                         # resample
                         g1 <- resample(data[data[,2]==combs[1,x],1],
                                        replace = T, M=M)
                         g2 <- resample(data[data[,2]==combs[2,x],1],
                                        replace = T, M = M)

                         mean_diffs <- rowMeans(g2)-rowMeans(g1)
                         vars1 <- apply(g1, MARGIN = 1, stats::var) / ns[combs[1,x]]
                         vars2 <- apply(g2, MARGIN = 1, stats::var) / ns[combs[2,x]]

                         # # Summary
                         mean_diff <- means[combs[2,x]] - means[combs[1,x]]
                         # var1 <- vars[combs[1,x]] / ns[combs[1,x]]
                         # var2 <- vars[combs[2,x]] / ns[combs[2,x]]

                         # # Degrees of Freedom
                         # df <- (var1 + var2)^2 /
                         #   ( var1^2 / (ns[combs[1,x]] - 1) + var2^2 / (ns[combs[2,x]] - 1) )
                         #
                         # # t / p-values
                         # t <- abs(mean_diff) / sqrt( var1 + var2 )
                         # p <- stats::ptukey(sqrt(2) * t, groups, df, lower.tail = FALSE)
                         #
                         # # Sigma standard error
                         # sd12 <- sqrt( 0.5 * (var1 + var2) )
                         #
                         # # Confidence Intervals (not sure why don'y split alpha but matches elsewhere)
                         # int <- stats::qtukey(p = 1-alpha, nmeans = groups, df = df) * sd12
                         # upper.conf <-  mean_diff + int
                         # lower.conf <-  mean_diff - int


                         c(paste0(combs[2,x], '-', combs[1,x]),
                           mean_diff, sqrt( 0.5 * mean(vars1)+mean(vars2)),
                           as.numeric(stats::quantile(mean_diffs,probs=c(alpha/2,1-alpha/2))) )
                       },
                       ns=ns, groups=groups, means=means, alpha=alpha, M=M)



  # Create dataframe from flattened list
  results <- data.frame(  t(statistics) )

  # Select columns set as factors that should be numeric and change with as.numeric
  results[-1] <-
    as.numeric(as.matrix(results[-1]))

  # Rename data frame columns
  colnames(results) <- c('groups', 'diff', 'se', 'upr', 'lwr')
  rownames(results) <- results$groups

  results[,c(2,4:5,3)]
}
