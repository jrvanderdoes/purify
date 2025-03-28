#' Plot Barplot for Strata Sizes
#'
#' Produce a barplot of strata sizes based on data.
#'
#' @param data Vector of data with values of strata.
#'
#' @return A ggplot2 object of barplots for strata counts.
#' @export
#'
#' @examples
#' data <- data.frame('A'=rnorm(100),'B'=rbinom(100,2,0.5))
#' plot_strata_bar(data$B)
plot_strata_bar <- function(data){
  ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = data)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 16),
      axis.title = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 12),
      legend.position = "none") +
    ggplot2::xlab('Strata') +
    ggplot2::ylab('Number of Occurrences')
}


#' Plot Boxplot for Strata Sizes
#'
#' Produce a boxplot of strata sizes based on data.
#'
#' @param data Data.frame of the data, the first column is the strata and the
#'  second some value for the box plots.
#'
#' @return A ggplot2 object of boxplots for data based on strata.
#' @export
#'
#' @examples
#' plot_strata_box(data.frame('B'=rbinom(100,2,0.5),'A'=rnorm(100)))
plot_strata_box <- function(data){
  ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x = data[,1], y=data[,2],
                                       fill=data[,1], group=data[,1])) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 16),
      axis.title = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 12),
      legend.position = "none") +
    ggplot2::xlab('Strata') +
    ggplot2::ylab('Number of Occurrences')
}
