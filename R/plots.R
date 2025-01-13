#' Title
#'
#' @param data Data.frame of the data, where rows are observations and columns
#'  are the variables.
#' @param strata String of the column name in the data to investigate.
#'
#' @return A ggplot2 object of strata sizes
#' @export
#'
#' @examples
#' boxplot_strata(data.frame('A'=rnorm(100),'B'=rbinom(100,2,0.5)), 'B')
boxplot_strata <- function(data, strata){
  ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x=data[[strata]])) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 16),
      axis.title = ggplot2::element_text(size=14),
      axis.text = ggplot2::element_text(size=12),
      legend.title = ggplot2::element_text(size=12),
      legend.text = ggplot2::element_text(size=10)) +
    ggplot2::xlab(eval(strata)) +
    ggplot2::ylab('Number of Occurrences')
}
