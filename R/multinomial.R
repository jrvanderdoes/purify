#' Multinomial Distribution
#'
#' Generate multinomially distributed random number vectors and compute
#'  multinomial probabilities.
#'
#' @param n Numeric for number of random vectors to draw
#' @param xs Numeric vector for number of each class that is a success
#' @param qs Numeric vector for the max number of successes in each class
#' @param size Integer for the total number of successes accross all classes
#' @param probs Numeric vector for the probabilities of each class
#'
#' @name multinomial
NULL

# #' #' CDF of multinomial
# #'
# #' @param qs
# #' @param probs
# #' @param size
# #'
# #' @returns
# #' @export
# #'
# #' @examples
# pmultinomial <- function(qs, probs, size=sum(qs)){
#   if(sum(probs)!=1) stop('Probs must sum to 1')
#   if(length(probs)!=length(qs)) stop('qs and probs must be same length')
#
#   ## Start recursion
#   possible_outcomes <- data.frame()
#   for(i in 0:min(qs[1], size)){
#     possible_outcomes <-
#       rbind(possible_outcomes,
#             cbind(i, .multinom_recursion(qs[-1], size - i) ) )
#   }
#   possible_outcomes <- na.omit(possible_outcomes)
#
#   sum(apply(possible_outcomes, MARGIN = 1, dmultinomial, probs=probs))
# }


#' @rdname multinomial
#'
#' @returns Probability that no more than `qs` successes are observed
#' @export
#'
#' @examples
#' pmultinomial(c(2,2,2),c(1/3,1/3,1/3),6)
#' pmultinomial(c(3,1,2),c(1/2,1/4,1/4),4)
pmultinomial <- function(qs, probs, size=sum(qs)){
  if(length(probs)!=length(qs)) stop('qs and probs must be same length')
  # Weight in case it does not sum to 1
  probs <- probs / sum(probs)

  ## Start recursion
  possible_outcomes <- expand.grid( sapply(qs, function(x){0:x},simplify = F))
  possible_outcomes <- possible_outcomes[rowSums(possible_outcomes)==size,]

  sum(apply(possible_outcomes, MARGIN = 1, dmultinomial, probs=probs))
}

# #' Recursion to find all possible options upto maximum
# #'
# #' @param maximum_amts
# #' @param n_remaining
# #'
# #' @returns
# .multinom_recursion <- function(maximum_amts, n_remaining ){
#   # End of recursion
#   if(length(maximum_amts)<=1) {
#     if(maximum_amts < n_remaining ) return(NA)
#
#     return(n_remaining)
#   }
#   # Loop through rest
#   tmp_data <- data.frame()
#   for(j in 0:min(maximum_amts[1], n_remaining ) ){
#     tmp_data <-
#       rbind(tmp_data,
#             cbind(j, multinom_recursion(maximum_amts[-1], n_remaining - j) ) )
#   }
#
#   tmp_data
# }

#' @rdname multinomial
#'
#' @returns Probability of `xs` successes under `probs` in a multinomial
#'  distribution
#' @export
#'
#' @examples
#' dmultinomial(c(1,2,3),c(1,2,3))
#' dmultinomial(c(1,2,3),c(1/3,1/3,1/3))
dmultinomial <- function(xs, probs){
  xs <- as.numeric(xs)

  if(length(probs)!=length(xs)) stop('xs and probs must be same length')
  # Weight in case it does not sum to 1
  probs <- probs / sum(probs)

  factorial(sum(xs)) / prod(sapply(xs, factorial)) *
    prod( apply(data.frame(probs,xs),MARGIN = 1, function(x){ x[1]^x[2] }) )
}


#' @rdname multinomial
#'
#' @returns Randomly observed multinomial distribution. Results are a matrix
#'  with each column indicating a separate trial and the rows for each possible
#'  outcome
#' @export
#'
#' @examples
#' rmultinomial(10, 5, c(1/2,1/4,1/4))
#' rmultinomial(5, 8, c(10,2,4))
rmultinomial <- function(n, size, probs) {
  stats::rmultinom(n, size, probs)
}
