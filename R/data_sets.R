#' Anatomical Data from Domestic Cats
#'
#' The heart and body weights of samples of male and female cats used for
#'  digitalis experiments. The cats were all adult, over 2 kg body weight.
#'
#' @format ## `cats`
#' A data frame with 144 rows and 3 columns:
#' \describe{
#'   \item{Sex}{Sex of cat as a factor with levels `F` and `M`}
#'   \item{Bwt}{Body weight in kilograms}
#'   \item{Hwt}{Heart weight in kilograms}
#' }
#' @source Fisher, R.A. (1947). The analysis of covariance method for the
#'  relation between a part and the whole. Biometrics, 3 2, 65-8.
#'
#' @references Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics
#'  with S. Fourth Edition. Springer, New York. ISBN 0-387-95457-0
"cats"


#' Subset of the Anatomical Data from Domestic Cats
#'
#' A subset of cats. The heart and body weights of samples of male and female
#'  cats used for digitalis experiments. The cats were all adult, over 2 kg body
#'  weight.
#'
#' @format ## `subcats`
#' A data frame with 116 rows and 3 columns:
#' \describe{
#'   \item{Sex}{Sex of cat as a factor with levels `F` and `M`}
#'   \item{Bwt}{Body weight in kilograms}
#'   \item{Hwt}{Heart weight in kilograms}
#' }
#' @source Fisher, R.A. (1947). The analysis of covariance method for the
#'  relation between a part and the whole. Biometrics, 3 2, 65-8.
#'
#' @references Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics
#'  with S. Fourth Edition. Springer, New York. ISBN 0-387-95457-0
"subcats"


#' Sample Titanic Data
#'
#' Data about passengers on the titanic.
#'
#' @format ## `titanic`
#' A data frame with 714 rows and 5 columns:
#' \describe{
#'   \item{Survived}{Bernoulli variable (0 or 1) if passenger survived.}
#'   \item{Pclass}{Class of passenger, values of 1,2,3.}
#'   \item{Sex}{Sex of passenger, 'male' or 'female'.}
#'   \item{Age}{Age of passenger}
#'   \item{Parch}{Number of Parents/Children Aboard}
#' }
#' @source Hendricks P (2015). _titanic: Titanic Passenger Survival Data Set_.
#'  R package version 0.1.0, <https://CRAN.R-project.org/package=titanic>.
"titanic"


#' German Credit Data
#'
#' Data on credit worthiness.
#'
#' @format ## `credit`
#' A data frame with 1000 rows and 14 columns:
#' \describe{
#'   \item{Class}{Credit worthiness as 'Good' or Bad'.}
#'   \item{Age}{Age of person.}
#'   \item{NumberExistingCredits}{Number of existing credits. }
#'   \item{Personal}{Sex and relationship status of person. }
#'   \item{CheckingAccountStatus}{Status of the checking account. }
#'   \item{CreditHistory}{Status of credit history. }
#'   \item{Purpose}{Reason for requesting credit. }
#'   \item{SavingsAccountBonds}{ Amount of Savings account bonds. }
#'   \item{EmploymentDuration}{Length of current employment. }
#'   \item{OtherDebtorsGuarantors}{Information on if there is a guarantor. }
#'   \item{Property}{Type of property. }
#'   \item{OtherInstallmentPlans}{Information on other installment plans.}
#'   \item{Housing}{Status of thier house. }
#'   \item{Job}{Type of job. }
#' }
#' @source UCI Machine Learning Repository
#' @references Kuhn, M. (2008). Building Predictive Models in R Using the caret
#'  Package. Journal of Statistical Software, 28(5), 1–26.
#'  https://doi.org/10.18637/jss.v028.i05
"credit"
