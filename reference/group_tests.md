# Group Tests for Mean/Median Differences

Compute and return information on tests for group differences in the
data.

## Usage

``` r
group_tests(
  data,
  tests = c("tukey", "snk", "lsd", "bt", "kramer", "duncan", "scheffe", "tamhaneT2",
    "uwh", "gh", "d3", "dunn", "dscf", "kwc", "kwd", "kwn", "median"),
  alpha = 0.05
)
```

## Arguments

- data:

  Data.frame with the first column the values and the second column the
  group names

- tests:

  Vector of strings, or a string, indicating the tests to check. Options
  include 'tukey','snk','lsd','bt','kramer', 'duncan', 'scheffe',
  'tamhaneT2', 'uwh', 'gh', 'd3', 'dunn', 'dscf', 'kwc', 'kwd', 'kwn',
  and 'median'

- alpha:

  Significance for confidence intervals / signficance of some tests,
  defaults to 0.05

## Value

A list of group difference statistics and related information

## Details

Tests require independent data unless otherwise specified.

Normally distributed data with equal variances and reasonably similarly
sized groups. *tukey*: Tukey honest significant differences *snk*:
Student-Newman-Keuls test *lsd*: Least significant difference test *bt*:
Bonferroni corrected pairwist t-tests (for pooled and unpooled
variances)

The following typically permit inbalance in the data group sizes.
*kramer*: Tukey-Kramer test *duncan*: Duncan's all-pairs comparisons
*scheffe*: Scheffe's test

The following typically permit more inbalance in data sizes and unequal
variances *tamhaneT2*: Tamhane's T2 all-pairs comparison test *uwh*:
Ury-Wiggins and Hochberg's all-pairs comparison test

The following typically permit inbalance in data sizes, unequal
variances, and some non-normality *gh*: Games-Howell test *d3*:
Dunnett's T3 test

The following are non-parametric *dunn*: Dunn's test of multiple
comparisions using rank sums *dscf*: Dwass, Steel, Critchlow and Fligner
all-pairs comparision test *kwc*: Kruskal-Wallis type, Conover's
non-parametric all-pairs comparison test *kwd*: Kruskal-Wallis type,
Dunn's non-parametric all-pairs comparison test *kwn*: Kruskal-Wallis
type, Nemeyi's non-parametric all-pairs comparison test *median*:
Brown-Mood all paris median test

By default, all listed post-hoc procedures are run. Use `tests` to
select only the procedures appropriate for the data and research
question. The collection of returned p-values is not a single
multiplicity-adjusted decision across all methods. References to
specific functions are given in the seealso section.

## See also

[`two_group_tests()`](https://jrvanderdoes.github.io/purify/reference/two_group_tests.md),
[`stats::TukeyHSD()`](https://rdrr.io/r/stats/TukeyHSD.html),
[`PMCMRplus::snkTest()`](https://rdrr.io/pkg/PMCMRplus/man/snkTest.html),
[`PMCMRplus::lsdTest()`](https://rdrr.io/pkg/PMCMRplus/man/lsdTest.html),
[`stats::pairwise.t.test()`](https://rdrr.io/r/stats/pairwise.t.test.html),
[`agricolae::HSD.test()`](https://rdrr.io/pkg/agricolae/man/HSD.test.html),
[`PMCMRplus::duncanTest()`](https://rdrr.io/pkg/PMCMRplus/man/duncanTest.html),
[`PMCMRplus::scheffeTest()`](https://rdrr.io/pkg/PMCMRplus/man/scheffeTest.html),
[`PMCMRplus::tamhaneT2Test()`](https://rdrr.io/pkg/PMCMRplus/man/tamhaneT2Test.html),
[`PMCMRplus::uryWigginsHochbergTest()`](https://rdrr.io/pkg/PMCMRplus/man/uryWigginsHochbergTest.html),
[`games_howell()`](https://jrvanderdoes.github.io/purify/reference/games_howell.md),
[`PMCMRplus::dunnettT3Test()`](https://rdrr.io/pkg/PMCMRplus/man/dunnettT3Test.html),
[`dunn.test::dunn.test()`](https://rdrr.io/pkg/dunn.test/man/dunn.test.html),
[`PMCMRplus::dscfAllPairsTest()`](https://rdrr.io/pkg/PMCMRplus/man/dscfAllPairsTest.html),
[`PMCMRplus::kwAllPairsConoverTest()`](https://rdrr.io/pkg/PMCMRplus/man/kwAllPairsConoverTest.html),
[`PMCMRplus::kwAllPairsDunnTest()`](https://rdrr.io/pkg/PMCMRplus/man/kwAllPairsDunnTest.html),
[`PMCMRplus::kwAllPairsNemenyiTest()`](https://rdrr.io/pkg/PMCMRplus/man/kwAllPairsNemenyiTest.html),
[`PMCMRplus::medianAllPairsTest()`](https://rdrr.io/pkg/PMCMRplus/man/medianAllPairsTest.html)

## Examples

``` r
data <- data.frame(
  "value" = c(rnorm(8, sd = 2), rnorm(6), rnorm(20, mean = 2)),
  "group" = c(rep("A", 8), rep("B", 6), rep("C", 20))
)
group_tests(data)
#>   Kruskal-Wallis rank sum test
#> 
#> data: x and group
#> Kruskal-Wallis chi-squared = 10.9558, df = 2, p-value = 0
#> 
#>                     Dunn's Pairwise Comparison of x by group                    
#>                                  (No adjustment)                                
#> 
#> Col Mean-│
#> Row Mean │          A          B
#> ─────────┼──────────────────────
#>        B │   0.604306
#>          │     0.2728 
#>          │
#>        C │  -2.376475  -2.836918
#>          │     0.0087*    0.0023*
#> 
#> α = 0.05
#> Reject Ho if p ≤ α/2, where p = Pr(Z ≥ |z|)
#> $means
#>          A          B          C 
#> -0.1458190  0.1743926  2.2638077 
#> 
#> $medians
#>          A          B          C 
#> -0.3041722  0.1555935  2.3151090 
#> 
#> $tukey
#> $tukey$details
#>          diff        lwr      upr       p adj
#> B-A 0.3202116 -1.6923521 2.332775 0.919170098
#> C-A 2.4096268  0.8507016 3.968552 0.001767713
#> C-B 2.0894151  0.3548033 3.824027 0.015516175
#> 
#> $tukey$groups
#>   A   B   C 
#> "a" "a" "b" 
#> 
#> 
#> $snk
#> $snk$pvalues
#>             A           B
#> B 0.698039552          NA
#> C 0.001767713 0.005783851
#> 
#> $snk$groups
#>     mean    sd  n Sig. group
#> A -0.146 2.743  8          a
#> B  0.174 1.075  6          a
#> C  2.264 0.815 20          b
#> 
#> 
#> $lsd
#> $lsd$pvalues
#>              A           B
#> B 0.6980395519          NA
#> C 0.0006274435 0.005783851
#> 
#> $lsd$groups
#>     mean    sd  n Sig. group
#> A -0.146 2.743  8          a
#> B  0.174 1.075  6          a
#> C  2.264 0.815 20          b
#> 
#> 
#> $bt
#> $bt$pvalues
#>            A          B
#> B 1.00000000         NA
#> C 0.00188233 0.01735155
#> 
#> $bt$pvalues_nonpool
#>           A          B
#> B 1.0000000         NA
#> C 0.1271438 0.01013846
#> 
#> 
#> $tukey_kramer
#>        value groups
#> C  2.2638077      a
#> B  0.1743926      b
#> A -0.1458190      b
#> 
#> $duncan
#> $duncan$pvalues
#>             A           B
#> B 0.660756868          NA
#> C 0.003067673 0.006950351
#> 
#> $duncan$groups
#>     mean    sd  n Sig. group
#> A -0.146 2.743  8          a
#> B  0.174 1.075  6          a
#> C  2.264 0.815 20          b
#> 
#> 
#> $scheffe
#> $scheffe$pvalues
#>             A          B
#> B 0.926368988         NA
#> C 0.002636467 0.02088313
#> 
#> $scheffe$groups
#>     mean    sd  n Sig. group
#> A -0.146 2.743  8          a
#> B  0.174 1.075  6          a
#> C  2.264 0.815 20          b
#> 
#> 
#> $tamhane_t2
#> $tamhane_t2$pvalues
#>           A          B
#> B 0.9878285         NA
#> C 0.1218314 0.01010424
#> 
#> $tamhane_t2$groups
#>     mean    sd  n Sig. group
#> A -0.146 2.743  8         ab
#> B  0.174 1.075  6          a
#> C  2.264 0.815 20          b
#> 
#> 
#> $uwh
#> $uwh$pvalues
#>            A          B
#> B 0.76997163         NA
#> C 0.08476254 0.01013846
#> 
#> $uwh$groups
#>     mean    sd  n Sig. group
#> A -0.146 2.743  8         ab
#> B  0.174 1.075  6          a
#> C  2.264 0.815 20          b
#> 
#> 
#> $games_howell
#>          diff        lwr      upr        se        t       df           p
#> B-A 0.3202116 -2.6179979 3.258421 0.7527062 0.300813 9.598639 0.951590843
#> C-A 2.4096268 -0.4497656 5.269019 0.6977241 2.442030 7.499887 0.095682858
#> C-B 2.0894151  0.6804493 3.498381 0.3361117 4.395681 6.819198 0.008224381
#> 
#> $dunnett_t3
#> $dunnett_t3$pvalues
#>           A           B
#> B 0.9860146          NA
#> C 0.1174914 0.009150262
#> 
#> $dunnett_t3$groups
#>     mean    sd  n Sig. group
#> A -0.146 2.743  8         ab
#> B  0.174 1.075  6          a
#> C  2.264 0.815 20          b
#> 
#> 
#> $dunn
#>   comparisons       adj.p
#> 1       A - B 0.272820017
#> 2       A - C 0.008739465
#> 3       B - C 0.002277563
#> 
#> $dscf
#> $dscf$pvalues
#>           A           B
#> B 0.9206520          NA
#> C 0.1162988 0.002916434
#> 
#> $dscf$groups
#>     mean    sd  n Sig. group
#> A -0.146 2.743  8         ab
#> B  0.174 1.075  6          a
#> C  2.264 0.815 20          b
#> 
#> 
#> $kw_conover
#> $kw_conover$pvalues
#>            A           B
#> B 0.75558272          NA
#> C 0.02208135 0.005673978
#> 
#> $kw_conover$groups
#>     mean    sd  n Sig. group
#> A -0.146 2.743  8          a
#> B  0.174 1.075  6          a
#> C  2.264 0.815 20          b
#> 
#> 
#> $kw_dunn
#> $kw_dunn$pvalues
#>            A          B
#> B 0.54564003         NA
#> C 0.03495786 0.01366538
#> 
#> $kw_dunn$groups
#>     mean    sd  n Sig. group
#> A -0.146 2.743  8          a
#> B  0.174 1.075  6          a
#> C  2.264 0.815 20          b
#> 
#> 
#> $kw_nemenyi
#> $kw_nemenyi$pvalues
#>            A          B
#> B 0.81776280         NA
#> C 0.04599453 0.01265734
#> 
#> $kw_nemenyi$groups
#>     mean    sd  n Sig. group
#> A -0.146 2.743  8          a
#> B  0.174 1.075  6          a
#> C  2.264 0.815 20          b
#> 
#> 
#> $median
#> $median$pvalues
#>           A           B
#> B 0.2718074          NA
#> C 0.3349985 0.007668285
#> 
#> $median$groups
#>     mean    sd  n Sig. group
#> A -0.146 2.743  8         ab
#> B  0.174 1.075  6          a
#> C  2.264 0.815 20          b
#> 
#> 
```
