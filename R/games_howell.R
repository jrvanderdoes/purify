#' Games-Howell Test
#'
#' Computes Games-Howell test on data. Similar to Tukey HSD, it computes the
#'  significance of the differences between groups. However, it requires far
#'  fewer assumptions. Per the original paper, non-normality it not a problems,
#'  groups can be different sizes, and there is assumption on the homogeneity of
#'  variances. Further, small sample sizes are okay (each group is typically
#'  recommended to have at least 6 observations).
#'
#' @param data Data.frame with the first column the values and the second column
#'  the group names
#' @param alpha Significance for confidence intervals, defaults to 0.05
#'
#' @returns Table showing groups, their differences, and significance, among other
#'  details
#' @export
#'
#' @references Games, P. A., & Howell, J. F. (1976). Pairwise Multiple
#'  Comparison Procedures with Unequal N’s and/or Variances: A Monte Carlo
#'  Study. Journal of Educational Statistics, 1(2), 113–125.
#' @references Games, P. A., Keselman, H. J., & Clinch, J. J. (1979). Tests for
#'  homogeneity of variance in factorial designs. Psychological Bulletin, 86(5),
#'  978–984.
#'
#' @seealso [stats::TukeyHSD()]
#'
#' @examples
#' data <- data.frame('value'=c(rnorm(14,sd = 2), rnorm(6), rnorm(20,mean=2)),
#'                    'group'=c(rep('A',14),rep('B',6), rep('C',20)))
#' games_howell(data)
games_howell <- function(data, alpha=0.05) {
  group_vector <- data[,2]
  sample_vector <- data[,1]

  # Group Combinations (Interactions)
  combs <- utils::combn(unique(group_vector), 2)

  # Summary Statistics
  ns <- tapply(sample_vector, group_vector, length)
  means <- tapply(sample_vector, group_vector, mean)
  vars <- tapply(sample_vector, group_vector, stats::var)

  # lapply faster than apply here
  # Games Howell
  statistics <- lapply(1:ncol(combs),
                       function(x, combs, ns, groups, means, vars, alpha) {
    # Summary
    mean_diff <- means[combs[2,x]] - means[combs[1,x]]
    var1 <- vars[combs[1,x]] / ns[combs[1,x]]
    var2 <- vars[combs[2,x]] / ns[combs[2,x]]

    # Degrees of Freedom
    df <- (var1 + var2)^2 /
      ( var1^2 / (ns[combs[1,x]] - 1) + var2^2 / (ns[combs[2,x]] - 1) )

    # t / p-values
    t <- abs(mean_diff) / sqrt( var1 + var2 )
    p <- stats::ptukey(sqrt(2) * t, groups, df, lower.tail = FALSE)

    # Sigma standard error
    sd12 <- sqrt( 0.5 * (var1 + var2) )

    # Confidence Intervals (not sure why don'y split alpha but matches elsewhere)
    int <- stats::qtukey(p = 1-alpha, nmeans = groups, df = df) * sd12
    upper.conf <-  mean_diff + int
    lower.conf <-  mean_diff - int


    list(paste0(combs[2,x], '-', combs[1,x]),
         mean_diff, sd12, t, df, p, upper.conf, lower.conf)
  },
  combs=combs, ns=ns, groups=length(ns), means=means, vars=vars, alpha=alpha)

  # # TukeyHSD
  # statistics1 <- lapply(1:ncol(combs),
  #                      function(x, ns, groups, means, vars, alpha) {
  #                        # Summary
  #                        mean_diff <- means[combs[2,x]] - means[combs[1,x]]
  #                        df <- sum(ns)-length(ns)
  #                        var12 <- sum( (ns - 1) * vars ) / df
  #
  #                        # t / p-values
  #                        t <- abs(mean_diff) / sqrt( var12 * sum(1/ns[combs[,x]]) )
  #                        p <- stats::ptukey(sqrt(2) * t, groups, df, lower.tail = FALSE)
  #
  #                        # Confidence Intervals (not sure why don't split alpha but matches elsewhere)
  #                        int <- stats::qtukey(p = 1-alpha, nmeans = groups, df = df) *
  #                          sqrt(var12/2*sum(1/ns[1:2]))
  #                        mean_diff - int
  #                        upper.conf <-  mean_diff + int
  #                        lower.conf <-  mean_diff - int
  #
  #                        list(paste0(combs[2,x], '-', combs[1,x]),
  #                             mean_diff, sd12, t, df, p, upper.conf, lower.conf)
  #                      },
  #                      ns=ns, groups=groups, means=means, vars=vars, alpha=alpha)
  # Unlist statistics collected earlier
  stats.unlisted <- lapply(statistics, function(x) {
    unlist(x)
  })

  # Create dataframe from flattened list
  results <- data.frame(matrix(unlist(stats.unlisted),
                               nrow = length(stats.unlisted),
                               byrow=TRUE))

  # Select columns set as factors that should be numeric and change with as.numeric
  results[c(2, 3:ncol(results))] <-
    as.numeric(as.matrix(results[c(2, 3:ncol(results))]))

  # Rename data frame columns
  colnames(results) <- c('groups', 'diff', 'se',
                         't', 'df', 'p', 'upr', 'lwr')
  rownames(results) <- results$groups

  results[,c(2,8,7,3:5,6)]
}


