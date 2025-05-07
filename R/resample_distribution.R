#' Resample Distributions
#'
#' @inheritParams resample
#' @param resampled_data Data from [resample()] or NULL to run [resample()] on
#'  data
#' @param ... Additional parameters for [resample()]
#'
#' @returns ggplot object showing original and resample distribution. Note that
#'  if a function is used, they many not be describing the same thing
#' @export
#'
#' @examples
#' resample_distribution(rnorm(100))
#' resample_distribution(
#'   data.frame('data'=c(rnorm(100),rnorm(50,mean=10)),
#'              'strata'=c(rep('A',100),rep('B',50))),
#'   strata = 'strata' )
#' resample_distribution(rnorm(100), fn = mean )
resample_distribution <- function(data, resampled_data = NULL,
                                strata = NULL,
                                ignore.columns = NULL, ...){
  if(is.null(resampled_data)){
    resampled_data <- resample(data, strata = strata,
                               ignore.columns = ignore.columns, ...)
  }

  if(!is.null(strata)){
    # When there are stratified
    density <- NULL
    dat <- data[,!(colnames(data) %in% c(strata, ignore.columns) ) ]
    original_hist <-
      ggplot2::ggplot() +
      ggplot2::geom_histogram(ggplot2::aes(
        x = dat,
        y=ggplot2::after_stat(density), group=data[,strata], fill=data[,strata]
      ), alpha=0.5, binwidth = max(0.2,2*sum(abs(range(dat)))/length(dat)),
      position="identity") +
      ggplot2::geom_density(ggplot2::aes(
        x = dat, y=ggplot2::after_stat(density), group=data[,strata], col=data[,strata]
      ), linewidth=1) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(hjust = 0.5, size = 16),
        axis.title = ggplot2::element_text(size = 14),
        axis.text = ggplot2::element_text(size = 12),
        legend.position = "none"
      ) +
      ggplot2::xlab("") +
      ggplot2::ylab("Original")

    stack_res <- do.call("rbind", resampled_data)
    dat_res <- stack_res[,!(colnames(stack_res) %in% c(strata, ignore.columns) ) ]
    resampled_hist <-
      ggplot2::ggplot() +
      ggplot2::geom_histogram(ggplot2::aes(
        x = dat_res,
        y=ggplot2::after_stat(density), group=stack_res[,strata], fill=stack_res[,strata]
      ), alpha=0.5, binwidth = max(0.2,2*sum(abs(range(dat_res)))/length(dat_res)),
      position="identity") +
      ggplot2::geom_density(ggplot2::aes(
        x = dat_res, y=ggplot2::after_stat(density), group=stack_res[,strata], col=stack_res[,strata]
      ), linewidth=1) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(hjust = 0.5, size = 16),
        axis.title = ggplot2::element_text(size = 14),
        axis.text = ggplot2::element_text(size = 12),
        legend.position = "none"
      ) +
      ggplot2::xlab("") +
      ggplot2::ylab("Resampled")
    plt <-
      patchwork::wrap_plots(original_hist,resampled_hist) +
      # dataPlot + bPlot +
      patchwork::plot_layout(nrow = 2, guides = "collect",
                             heights = c(1,1))
    return(plt)
  }


  if(is.data.frame(resampled_data) || is.matrix(resampled_data)){
    # Resampled M times of data length
    #   Base Case
    density <- NULL
    original_hist <-
      ggplot2::ggplot() +
      ggplot2::geom_histogram(ggplot2::aes(
        x = data, y=ggplot2::after_stat(density)
      ), alpha=0.5, binwidth = min(sum(abs(range(data)))/10,
                                   max(0.2,2*2*sum(abs(range(data)))/length(data)))) +
      ggplot2::geom_density(ggplot2::aes(
        x = data, y=ggplot2::after_stat(density)
      ), linewidth=1) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(hjust = 0.5, size = 16),
        axis.title = ggplot2::element_text(size = 14),
        axis.text = ggplot2::element_text(size = 12),
        legend.position = "none"
      ) +
      ggplot2::xlab("") +
      ggplot2::ylab("Original")

    resampled_hist <-
      ggplot2::ggplot() +
      ggplot2::geom_histogram(ggplot2::aes(
        x = as.matrix(resampled_data), y=ggplot2::after_stat(density)
      ), alpha=0.5,
      binwidth = min(sum(abs(range(resampled_data)))/10,
                     max(0.2, 2*sum(abs(range(resampled_data)))/length(resampled_data)))) +
      ggplot2::geom_density(ggplot2::aes(
        x = as.matrix(resampled_data), y=ggplot2::after_stat(density)
      ), linewidth=1) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(hjust = 0.5, size = 16),
        axis.title = ggplot2::element_text(size = 14),
        axis.text = ggplot2::element_text(size = 12),
        legend.position = "none"
      ) +
      ggplot2::xlab("") +
      ggplot2::ylab("Resampled")

    plt <-
      patchwork::wrap_plots(original_hist,resampled_hist) +
      # dataPlot + bPlot +
      patchwork::plot_layout(nrow = 2, guides = "collect",
                             heights = c(1,1))
      # patchwork::plot_annotation(
      #   title = title,
      #   subtitle = subtitle,
      #   theme = ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size=22),
      #                          plot.subtitle = ggplot2::element_text(hjust = 0.5))
      # ) &
      # theme(legend.position='bottom')
      # ggplot2::theme(legend.position='none')
  } else{
      # When there are stratad
    density <- NULL
      dat <- data[,!(colnames(data) %in% c(ignore.columns) ) ]
      original_hist <-
        ggplot2::ggplot() +
        ggplot2::geom_histogram(ggplot2::aes(
          x = dat,
          y=ggplot2::after_stat(density) ), alpha=0.5,
          binwidth = min(sum(abs(range(dat)))/10,
                         max(0.2,2*sum(abs(range(dat)))/length(dat))),
        position="identity") +
        ggplot2::geom_density(ggplot2::aes(
          x = dat, y=ggplot2::after_stat(density)
        ), linewidth=1) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(hjust = 0.5, size = 16),
          axis.title = ggplot2::element_text(size = 14),
          axis.text = ggplot2::element_text(size = 12),
          legend.position = "none"
        ) +
        ggplot2::xlab("") +
        ggplot2::ylab("Original")

      stack_res <- do.call("rbind", resampled_data)
      dat_res <- stack_res[,!(colnames(stack_res) %in% c(strata, ignore.columns) ) ]
      resampled_hist <-
        ggplot2::ggplot() +
        ggplot2::geom_histogram(ggplot2::aes(
          x = dat_res,
          y=ggplot2::after_stat(density)
        ), alpha=0.5, binwidth = min(sum(abs(range(dat_res)))/10,
                                     max(0.2,2*sum(abs(range(dat_res)))/length(dat_res))),
        position="identity") +
        ggplot2::geom_density(ggplot2::aes(
          x = dat_res, y=ggplot2::after_stat(density)
        ), linewidth=1) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(hjust = 0.5, size = 16),
          axis.title = ggplot2::element_text(size = 14),
          axis.text = ggplot2::element_text(size = 12),
          legend.position = "none"
        ) +
        ggplot2::xlab("") +
        ggplot2::ylab("Resampled")
      plt <-
        patchwork::wrap_plots(original_hist,resampled_hist) +
        # dataPlot + bPlot +
        patchwork::plot_layout(nrow = 2, guides = "collect",
                               heights = c(1,1))
    }

  plt
}
