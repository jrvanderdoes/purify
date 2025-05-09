#' Distribution Plot
#'
#' Plot data against a theoretical distribution. The typical comparision to a
#'  normal distribution is called a QQ-plot.
#'
#' @param x Vector of observations
#' @param distribution String of distribution as part of the 'qDIST' and 'dDIST'
#' @param alpha Numeric for the significance used for the bands. If it outside
#' (0,1) or NULL then no bands will be computed
#' @param labels Vector of labels of the same length as `x` for the string to
#'  add to the plot when a value exceeds the bounds
#' @param ... Additional parameters to pass to qDIST
#'
#' @returns A ggplot2 object for the values plotted against theoretical values
#' @export
#'
#' @examples
#' distribution_plot(rnorm(100))
#' distribution_plot(rexp(100), dist='exp')
distribution_plot <- function(x, distribution = "norm",
                            alpha = 0.05, labels = names(x), ...){
  # General Information
  q.function <- eval(parse(text = paste0("q", distribution)))
  d.function <- eval(parse(text = paste0("d", distribution)))

  x <- stats::na.omit(x)
  P <- stats::ppoints(length(x))

  data <- data.frame(sample = x[order(x)], z = q.function(P, ...))

  # Get Line Information
  quantile_data <- stats::quantile(data$sample, c(0.25, 0.75))
  quantile_theo <- q.function(c(0.25, 0.75), ...)
  b <- diff(quantile_data)/diff(quantile_theo)
  coef <- c(quantile_data[1] - b * quantile_theo[1], b)

  # Get Band Information
  if(!is.null(alpha) && alpha>0 && alpha<1){
    critical_val <- stats::qnorm(1 - alpha/2)
    SE <- (coef[2]/d.function(data$z)) * sqrt(P * (1 - P) / length(x))
    mean_est <- coef[1] + coef[2] * data$z
    data$upper <- mean_est + critical_val * SE
    data$lower <- mean_est - critical_val * SE
  } else{
    alpha <- NULL
  }

  # Add Label Information
  if(!is.null(labels)){
    data$label <- ifelse(data$sample > data$upper |
                           data$sample < data$lower,
                         labels[order(x)], "")
  }

  # Create Plot
  z <- lower <- upper <- label <- NULL
  plt <- ggplot2::ggplot(data, ggplot2::aes(x=z, y=sample)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(intercept = coef[1], slope = coef[2]) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title = ggplot2::element_text(size=18),
                   axis.text = ggplot2::element_text(size=16)) +
    ggplot2::ylab('Sample') +
    ggplot2::xlab('Theoretical')

  if(!is.null(alpha)){
    plt <- plt + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                                      alpha=0.2)
  }

  if(!is.null(labels)) {
    plt <- plt + ggplot2::geom_text( ggplot2::aes(label = label))
  }

  # coef
  plt
}
