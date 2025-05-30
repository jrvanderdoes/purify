% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/variance_tests.R
\name{resample_variance}
\alias{resample_variance}
\title{Resample Variance}
\usage{
resample_variance(data, alphas = 0.05, M = 1000)
}
\arguments{
\item{data}{Data.frame with the first column the values and the second column
the group names}

\item{alphas}{Vector of significances for confidence intervals, defaults to 0.05}

\item{M}{Numeric for the number of iterations in the resampling}
}
\value{
Matrix with variance and alpha-confidence intervals for each group
}
\description{
Estimate variance for each group and bootstrap the alpha-confidence intervals.
}
\examples{
data <- data.frame(
  "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
  "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
)
resample_variance(data)
}
