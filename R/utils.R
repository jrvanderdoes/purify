#' Prepare Data Utility
#'
#' @param data Data.frame with the first column the values and the second column
#'  the group names
#'
#' @returns List with data, groups, and formula
.prepare_data <- function(data){
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

  form <- stats::as.formula(paste(col_names[1], "~", col_names[2]))

  list('data'=data,
       'groups'=groups,
       'form'=form)
}


#' Summarize Results From PMCMRplus Without Outputs
#'
#' @param x PMCMRplus data
#' @param alpha significance in \[0, 1\]
#'
#' @returns Summary data.frame
.PMCMRplus_summary <- function(x, alpha){
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
  out.mcv <-  multcompView::multcompLetters(ppval, threshold = alpha)
  dat <- x$model
  xmean <- tapply(dat$x, dat$g, mean)
  xn <- tapply(dat$x, dat$g, length)
  xsd <- tapply(dat$x, dat$g, stats::sd)
  xdf <- data.frame(round(xmean, 3), round(xsd, 3), xn,
                    out.mcv$Letters)
  rownames(xdf) <- c(colnames(x$statistic)[1], rownames(x$statistic))
  names(xdf) <- c("mean", "sd", "n", "Sig. group")

  xdf
}
