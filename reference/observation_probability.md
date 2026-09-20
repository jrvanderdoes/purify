# Observation Probability

Evaluates the probability that randomly drawing from the groups with
given probabilities would return a distribution of results the same or
more extreme (i.e. less likely). Since the same or more extreme outcomes
always include the given outcome, this probability will always be equal
to or larger than the probability of the given event.

## Usage

``` r
observation_probability(strata_info)
```

## Arguments

- strata_info:

  Data.frame with the first column having the number of observed
  occurrences and the second column being the probability of the events

## Value

Numeric probability in \[0,1\] indicating the probability of an equal to
or more extreme distribution of events

## Examples

``` r
observation_probability(
  data.frame("counts" = c(15, 5), c(0.5, 0.5))
)
#> [1] 0.04138947
observation_probability(
  data.frame("counts" = c(15, 5), c(0.9, 0.1))
)
#> [1] 0.0431745
observation_probability(
  data.frame("counts" = c(10, 10), c(0.5, 0.5))
)
#> [1] 1
```
