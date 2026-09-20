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
#'   data.frame("counts" = c(15, 5), c(0.5, 0.5))
#' )
#' observation_probability(
#'   data.frame("counts" = c(15, 5), c(0.9, 0.1))
#' )
#' observation_probability(
#'   data.frame("counts" = c(10, 10), c(0.5, 0.5))
#' )
observation_probability <- function(strata_info) {
  if (!is.data.frame(strata_info) && !is.matrix(strata_info)) {
    stop("`strata_info` must be a data.frame or matrix.", call. = FALSE)
  }

  if (ncol(strata_info) != 2) {
    stop("Parameter strata_info must be a 2-column object.", call. = FALSE)
  }

  # Get information from the df
  freq <- strata_info[, 1]
  if (!is.numeric(freq) ||
      any(!is.finite(freq)) ||
      any(freq < 0) ||
      any(freq != round(freq)) ||
      sum(freq) < 1) {
    stop("The count column must contain nonnegative integers with a positive total.",
         call. = FALSE)
  }
  freq <- as.integer(freq)
  size <- sum(freq)

  probs <- strata_info[, 2]
  if (!is.numeric(probs) ||
      any(!is.finite(probs)) ||
      any(probs < 0) ||
      sum(probs) <= 0) {
    stop("The probability column must contain nonnegative values with a positive total.",
         call. = FALSE)
  }

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

  expected_freq <- size * probs
  ideal_freq <- floor(expected_freq)

  remaining <- size - sum(ideal_freq)

  if (remaining > 0) {
    fractional_parts <- expected_freq - ideal_freq
    add_to <- order(fractional_parts, decreasing = TRUE)[seq_len(remaining)]
    ideal_freq[add_to] <- ideal_freq[add_to] + 1
  }

  diff_ideal <- abs(ideal_freq - freq)

  ## Compute possible more extreme outcomes
  possible_outcomes <-
    expand.grid(sapply(1:length(freq),
      function(idx, size, ideal_freq, diff_ideal) {
        # Compute all possible counts
        poss_diffs <- abs(0:size - ideal_freq[idx])

        # Return values more extreme than those observed
        (0:size)[poss_diffs >= diff_ideal[idx]]
      },
      size = size, ideal_freq = ideal_freq, diff_ideal = diff_ideal,
      simplify = F
    ))
  possible_outcomes <- possible_outcomes[rowSums(possible_outcomes) == size, ]

  ## Compute probabilities and only sum those that are <= to the observed
  true_prob <- dmultinomial(freq, probs)
  poss_probs <- apply(possible_outcomes, MARGIN = 1, dmultinomial, probs = probs)

  sum(poss_probs[poss_probs <= true_prob])
}
