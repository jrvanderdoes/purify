# Games-Howell Test

Computes Games-Howell test on data. Similar to Tukey HSD, it computes
the significance of the differences between groups. However, it requires
far fewer assumptions. Per the original paper, non-normality it not a
problems, groups can be different sizes, and there is assumption on the
homogeneity of variances. Further, small sample sizes are okay (each
group is typically recommended to have at least 6 observations).

## Usage

``` r
games_howell(data, alpha = 0.05)
```

## Arguments

- data:

  Data.frame with the first column the values and the second column the
  group names

- alpha:

  Significance for confidence intervals, defaults to 0.05

## Value

Table showing groups, their differences, and significance, among other
details

## References

Games, P. A., & Howell, J. F. (1976). Pairwise Multiple Comparison
Procedures with Unequal N’s and/or Variances: A Monte Carlo Study.
Journal of Educational Statistics, 1(2), 113–125.

Games, P. A., Keselman, H. J., & Clinch, J. J. (1979). Tests for
homogeneity of variance in factorial designs. Psychological Bulletin,
86(5), 978–984.

## See also

[`stats::TukeyHSD()`](https://rdrr.io/r/stats/TukeyHSD.html)

## Examples

``` r
data <- data.frame(
  "value" = c(rnorm(14, sd = 2), rnorm(6), rnorm(20, mean = 2)),
  "group" = c(rep("A", 14), rep("B", 6), rep("C", 20))
)
games_howell(data)
#>           diff       lwr       upr        se        t        df            p
#> B-A -0.7423618 -2.289172 0.8044486 0.4122324 1.273381 12.510440 0.4347693354
#> C-A  2.1259349  1.060758 3.1911115 0.2957798 5.082371 18.471898 0.0002031086
#> C-B  2.8682968  1.450446 4.2861474 0.3358547 6.038898  6.632983 0.0015866548
```
