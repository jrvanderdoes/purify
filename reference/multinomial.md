# Multinomial Distribution

Generate multinomially distributed random number vectors and compute
multinomial probabilities.

## Usage

``` r
pmultinomial(qs, probs, size = sum(qs))

dmultinomial(xs, probs)

rmultinomial(n, size, probs)
```

## Arguments

- qs:

  Numeric vector for the max number of successes in each class

- probs:

  Numeric vector for the probabilities of each class

- size:

  Integer for the total number of successes accross all classes

- xs:

  Numeric vector for number of each class that is a success

- n:

  Numeric for number of random vectors to draw

## Value

Probability that no more than `qs` successes are observed

Probability of `xs` successes under `probs` in a multinomial
distribution

Randomly observed multinomial distribution. Results are a matrix with
each column indicating a separate trial and the rows for each possible
outcome

## Examples

``` r
pmultinomial(c(2, 2, 2), c(1 / 3, 1 / 3, 1 / 3), 6)
#> [1] 0.1234568
pmultinomial(c(3, 1, 2), c(1 / 2, 1 / 4, 1 / 4), 4)
#> [1] 0.625
dmultinomial(c(1, 2, 3), c(1, 2, 3))
#> [1] 0.1388889
dmultinomial(c(1, 2, 3), c(1 / 3, 1 / 3, 1 / 3))
#> [1] 0.08230453
rmultinomial(10, 5, c(1 / 2, 1 / 4, 1 / 4))
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,]    2    2    3    2    3    3    3    4    3     2
#> [2,]    0    2    0    0    1    2    0    1    1     0
#> [3,]    3    1    2    3    1    0    2    0    1     3
rmultinomial(5, 8, c(10, 2, 4))
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    5    5    4    6    4
#> [2,]    1    2    2    1    2
#> [3,]    2    1    2    1    2
```
