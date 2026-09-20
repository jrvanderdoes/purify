# Summary

`purify` is an R package designed for resampling and testing of data,
aimed at researchers and practitioners who analyze complex datasets,
often with unbalanced strata, multiple predictors, or a dependent output
variable. This package enables users to perform robust statistical
analyses using test statistics and resampling of variables with and
without relation to the output and other stratification variables. The
resampling analysis can be conducted on statistical summaries (e.g. mean
square error); coefficient, forecast, and model statistic estimates; and
other custom defined statistics, `purify` offers versatile resampling
settings, including block, sliding window, and stratified resampling
with and without replacement. These methods are also extended to
cross-validation and confidence intervals. Each method can be tailored
to specific data structures and research questions. With its intuitive
interface and customizable options, `purify` streamlines the process of
hypothesis testing and estimation of statistical significance.

# Statement of need

Unbalanced data are widely observed, yet methods for analysis can be
unwieldy, overly complex, or missing implementations. `purify` fills
this gap by providing and organizing statistical tests robust to various
assumptions and an extensive collection of resampling methodology. These
tests include normality, anova, and two-sample tests along with
statistics to quantify stratification effects.

Resampling is a fundamental technique in estimating the distribution of
statistics, testing hypotheses, and deriving confidence
intervals–especially when analytical solutions are impractical. When
assumptions for statistical tests are under question, resampling
provides another tool to assess their effectiveness. Many R packages
provide basic resampling methods but lack specialized support for
complex structures. Such structures may contain dependence and variables
which should not be resampled. Analysis of these structures can be
unwieldy to investigate in other packages. `purify` offers a flexible
framework to enable data scientists and researchers to perform and
compare targeted, customizable resampling schemes that account for data
structure. The methods are compatible for user-defined functions and
outside models, making `purify` useful for rigorous hypothesis testing
and model evaluation.

Permutation tests can be computationally intensive, so speed is
considered throughout `purify`. Cross-validation, forecasting,
visualization, and summary functions extend the framework and support
interpretation.

# State of the field

Several R packages provide important components of resampling and
statistical analysis. The `boot` package provides a general framework
for bootstrap methods \[@R-boot\], while `rsample` supports resampling
objects for modeling workflows \[@R-rsample\]. Packages such as `caret`
provide cross-validation and model evaluation tools \[@caret\], and
packages such as `car` and `PMCMRplus` provide tests for variance
heterogeneity and group differences \[@R-car; @R-PMCMRplus\]. These
packages are valuable, but users often need to combine several
interfaces when analyzing structured, dependent, or unbalanced data.

`purify` centralizes and expands resampling capabilities within a common
framework. No single cited package combines structured block and
stratified resampling, unequal-group handling, resampling-based
statistical tests, forecasting intervals, and user-defined analysis
functions in one interoperable interface. Its contribution is therefore
not simply to collect existing tests, but to provide workflows for
structured resampling. In particular, `purify` supports separate and
sliding-window block resampling, stratified resampling with unequal
group sizes, resampling with or without replacement, and user-defined
functions to structured data. These features are implemented through
functions such as
[`resample()`](https://jrvanderdoes.github.io/purify/reference/resample.md)
and
[`observation_probability()`](https://jrvanderdoes.github.io/purify/reference/observation_probability.md).
The package also extends this framework to simulation-based hypothesis
testing, pairwise group comparisons, variance estimation, effect-size
estimation, forecast confidence intervals, and dependent
cross-validation. For example,
[`resample_welch_anova()`](https://jrvanderdoes.github.io/purify/reference/resample_welch_anova.md)
uses resampling to evaluate an ANOVA statistic under a simulated null
distribution, while
[`confidence_intervals()`](https://jrvanderdoes.github.io/purify/reference/confidence_intervals.md)
supports bootstrap forecasting with user-supplied prediction functions
and rolling training windows.
[`resample_differences()`](https://jrvanderdoes.github.io/purify/reference/resample_differences.md)
and
[`resample_variance()`](https://jrvanderdoes.github.io/purify/reference/resample_variance.md)
provide group-specific bootstrap estimates and intervals using the same
underlying resampling framework.

Some procedures included in `purify`, such as Games–Howell comparisons,
normality tests, and multinomial calculations, are established methods
rather than newly invented statistical procedures. Their inclusion
provides a consistent interface and allows them to be used alongside the
package’s structured resampling and simulation methods.

![Subcats. Body and heart weights of cats with respect to their
sex.](articles/articles/cat_overview.png)

**Subcats.** Body and heart weights of cats with respect to their sex.

# Package functionality

Assessing whether the data is homogeneous in its variance, exhibits
normality, or has significant differences between strata often requires
extensive testing. `purify` offers functions such as
[`normality_tests()`](https://jrvanderdoes.github.io/purify/reference/normality_tests.md),
[`variance_tests()`](https://jrvanderdoes.github.io/purify/reference/variance_tests.md),
and
[`group_tests()`](https://jrvanderdoes.github.io/purify/reference/group_tests.md)
to investigate several test statistics at once. Resampled versions of
statistics are provided to determine estimates and confidence intervals
with fewer assumptions, e.g. see
[`resample_variance()`](https://jrvanderdoes.github.io/purify/reference/resample_variance.md).

A primary function in `purify` is
[`resample()`](https://jrvanderdoes.github.io/purify/reference/resample.md).
This function offers clear input parameters to simplify the process of
selecting or evaluating the correct methodology, even for user-defined
functions. The multistep selection allows for combinations of dependent
data, unbalanced data, and resampling to be performed with and without
replacement. The flexibility enables users to adapt `purify` to diverse
data contexts and hypothesis-testing requirements. See also
[`cross_validation()`](https://jrvanderdoes.github.io/purify/reference/cross_validation.md)
and
[`confidence_intervals()`](https://jrvanderdoes.github.io/purify/reference/confidence_intervals.md).

# Software design

The package is organized around
[`resample()`](https://jrvanderdoes.github.io/purify/reference/resample.md),
which provides a common engine for ordinary, block, sliding-window, and
stratified resampling. Rather than creating separate functions for every
combination of sampling assumptions, the design exposes these choices as
parameters. This reduces duplicated code and allows the same resampling
logic to be reused for bootstrap estimation, permutation-style
procedures, simulation-based tests, and model evaluation.

The `fn` argument allows users to apply an arbitrary function to each
resampled data set. This supports summaries, fitted models, forecasts,
and user-defined test statistics without requiring the package to
anticipate every analysis. Arguments for strata, block size,
replacement, sample sizes, and columns that should not be resampled
provide control over common sources of dependence and imbalance.

Higher-level functions build on these design choices rather than
implementing independent resampling systems. For example, the resampling
functions for Welch ANOVA, pairwise differences, and variance estimates
use related data and simulation structures, while the forecasting and
cross-validation functions adapt them to sequential data. This design
prioritizes flexibility and reuse, with the trade-off that users must
understand their data structure and choose sampling parameters
appropriate to their analysis.

These choices involve deliberate trade-offs. A parameterized common
engine reduces duplicated implementations and keeps resampling behavior
consistent, but it gives users more responsibility for selecting valid
sampling settings. Allowing arbitrary functions makes the framework
useful for models, forecasts, and custom statistics, while also
requiring those functions to be appropriate for each resampled data set.
Block and sliding-window methods better preserve dependence in
sequential data, but can reduce the effective sample size and increase
computation.

# Research impact statement

`purify` has been developed in response to research analyses involving
unevenly sized groups, stratified data, dependence, and resampling-based
model evaluation. It was used to conduct the statistical analyses of
study data in projects reported by @tetui:etal:2022 and
@alexander:hall:chen:2024, focused on healthcare and citations. These
applications motivated the package’s emphasis on structured resampling
and user-defined analysis functions.

The repository also provides reproducible examples, package tests,
datasets, vignettes, and documentation that demonstrate the methods on
applied data. Together, these materials provide a starting point for
researchers who need to adapt resampling and simulation procedures to
their own data structures. The package is designed to support further
research use by allowing new summaries, models, and test statistics to
be supplied through user-defined functions without modifying the
resampling engine.

# Example

`purify` provides in-depth articles on the package
[website](https://jrvanderdoes.github.io/purify/). For example,

- The *purify* article describes the core features of `purify` and
  includes simulations and real data examples to demonstrate the
  functions.
- The *cats* article details investigation on real data.

We consider a subset of the cats (*subcats*) data set below. We use sex
and body weight to estimate heart weight; see . Similar to many
real-world examples, the data is highly imbalanced. Nonetheless, sex and
body weight are both useful in understanding heart weight. In
particular, female cats have lower body weights and have a lower heart
weight even for the same body weight when compared to male cats.

[TABLE]

**Subcats models.** Models of cats using body weight and sex to predict
heart weight. {.table style="width:97%;"}

Let the linear model be defined as heart weight predicted by an
intercept, body weight, and sex. Estimates for the coefficients and the
confidence intervals for each parameter are given in . When applying the
linear model directly on the data, only body weights appear to
significantly impact heart weight. For resampled data, where samples are
taken to create more evenly sized groups based on sex, both sex and body
weight are determined to be significant. The cost for this simple
example is that the confidence interval on body weight is larger. While
additional simulations or modifying the resampling scheme may mitigate
such losses, it is important to consider such effects. Often prediction
error, such as mean square error (MSE), is more important and in this
case, the resampled model also performs better. See articles for
information on other functions and additional analysis on this and other
data.

# AI usage disclosure

The software’s core research goals, statistical methods, architecture,
and initial implementation were developed by the human authors. During
revision, OpenAI Codex was used to assist with code review,
documentation improvements, and editorial drafting. AI assistance was
used as a review aid rather than a substitute for author decisions about
the methods or software design.

The authors reviewed, edited, and validated all AI-assisted suggestions,
including by inspecting the source code, adding and running tests,
checking package documentation, and running R package checks. The
authors remain responsible for the accuracy, originality, licensing, and
scientific content of the software and manuscript.

# Acknowledgements

Development of the `purify` package was inspired by foundational methods
in statistical resampling and permutation testing along with the rich
literature on stratified data. Special thanks to the open-source R
community for support and resources.

Contributions to `purify` are welcome and notable recognition is given
to all who raise awareness of deficiencies in the package via the GitHub
repository.

No specific funding was received for the development of this software.
The authors declare that they have no competing interests.

# References
