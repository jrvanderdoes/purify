% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group_tests.R
\name{two_group_tests}
\alias{two_group_tests}
\title{Two Group Tests for Mean Differences}
\usage{
two_group_tests(
  data,
  tests = c("t", "wilcox", "bayes", "factor"),
  alpha = 0.05
)
}
\arguments{
\item{data}{Data.frame with the first column the values and the second column
the group names}

\item{tests}{Vector of strings, or a string, indicating the tests to check.
Options include 't', 'wilcox', 'bayes', and 'factor'}

\item{alpha}{Significance for confidence intervals, defaults to 0.05}
}
\value{
A list of two group difference statistics and related information
}
\description{
Compute and return information on tests for two group differences in the data.
}
\details{
Tests require independent data unless otherwise specified.
\emph{t}: t-test and a Welch modified t-test for uneven variances
\emph{wilcox}: A Wilcoxon test
\emph{bayes}: Bayesian t-test and a Welch modified Bayesian t-test for uneven
variances
\emph{factor}: Bayesian factor approach to a t-test
}
\examples{
data <- data.frame(
  "value" = c(rnorm(14, sd = 2), rnorm(10, mean = 2)),
  "group" = c(rep("A", 14), rep("B", 10))
)
two_group_tests(data)
}
\seealso{
\code{\link[=group_tests]{group_tests()}}, \code{\link[stats:t.test]{stats::t.test()}}, \code{\link[stats:wilcox.test]{stats::wilcox.test()}},
\code{\link[Bolstad:bayes.t.test]{Bolstad::bayes.t.test()}}, \code{\link[BayesFactor:ttestBF]{BayesFactor::ttestBF()}}
}
