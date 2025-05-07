#' Observation Probability
#'
#' Evaluates the probability that randomly drawing from the groups with given
#'  probabilities would return a distribution of results the same or more
#'  extreme (i.e. less likely). Since the same or more extreme outcomes always
#'  include the given outcome, this probability will always be equal to or larger
#'  than the probability of the given event.
#'
#' @param strata_info Data.frame with the first column having the number of
#'  observed occurrences and the second column being the probability of the
#'  events
#'
#' @returns Numeric probability in \[0,1\] indicating the probability of an equal
#'  to or more extreme distribution of events
#' @export
#'
#' @examples
#' observation_probability(
#'   data.frame('counts'=c(15,5),c(0.5,0.5))
#' )
#' observation_probability(
#'   data.frame('counts'=c(15,5),c(0.9,0.1))
#' )
#' observation_probability(
#'   data.frame('counts'=c(10,10),c(0.5,0.5))
#' )
observation_probability <- function(strata_info){
  if(ncol(strata_info)!=2) stop('Parameter strata_info must be a 2-column object.')
  # Get information from the df
  freq <- strata_info[,1]
  size <- sum(freq)

  probs <- strata_info[,2]
  # Weight in case it does not sum to 1
  probs <- probs / sum(probs)

  # if(!is.null(ncol(strata_info)) && ncol(strata_info)==2){
  #
  # }else if(is.vector(strata_info)){
  #   # Get information based on obsered strata_info
  #   size <- length(strata_info)
  #   groups <- as.data.frame(table(strata_info))
  #
  #   freq <- groups$Freq
  #   probs <- freq / size
  # }

  ideal_freq <- round(size * probs)
  if(sum(ideal_freq) != size)
    stop('Internal error. Please report so we can fix this.')

  diff_ideal <- abs(ideal_freq-freq)

  ## Compute possible more extreme outcomes
  possible_outcomes <-
    expand.grid( sapply(1:length(freq),
                        function(idx, size, ideal_freq, diff_ideal){
                          # Compute all possible counts
                          poss_diffs <- abs(0:size - ideal_freq[idx])

                          # Return values more extreme than those observed
                          (0:size)[poss_diffs >= diff_ideal[idx]]
                        },size=size, ideal_freq=ideal_freq, diff_ideal=diff_ideal,
                        simplify = F))
  possible_outcomes <- possible_outcomes[rowSums(possible_outcomes)==size,]

  ## Compute probabilities and only sum those that are <= to the observed
  true_prob <- dmultinomial(freq,probs)
  poss_probs <- apply(possible_outcomes, MARGIN = 1, dmultinomial, probs=probs)

  sum(poss_probs[poss_probs<=true_prob])
}
