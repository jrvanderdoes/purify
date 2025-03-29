---
title: 'Purify: An R package for Bootstrapping'
tags:
  - R
  - Statistics
  - Permutation and Bootstrap Resampling
  - Simulation
authors:
  - name: Jeremy VanderDoes
    orcid: 0009-0001-9885-3073
    equal-contrib: true 
    affiliation: 1 # (Multiple affiliations must be quoted)
    corresponding: true
  - name: Yuling Max Chen
    equal-contrib: true # (to be editted)
    orcid: 0000-0000-0000-0000
    affiliation: 1
affiliations:
  - name: Department of Statistics, University of Waterloo, Waterloo, ON, Canada
    index: 1
date: 13 January 2025
nocite: | 
  @*
bibliography: paper.bib
---

# Summary

`Purify` is an R package designed for resampling and testing of data, aimed at 
researchers and practitioners who analyze complex datasets with a potentially dependent output 
variable and multiple predictors. This package enables users to perform robust 
statistical analyses by allowing resampling of variables with and without relation
to the output variable. Analyses can be focused on results such as 
statistical summaries (e.g. mean square error) and coefficient estimates of models. 
`Purify` offers versatile resampling methods, such as simple, block, sliding window, 
and stratified resampling. Each can be tailored to specific data structures and 
research questions. With its intuitive interface and customizable options, 
`Purify` streamlines the process of hypothesis testing and estimation of 
statistical significance.

# Statement of need

Resampling is fundamental technique in estimating the distribution of a 
statistic, testing hypotheses, and deriving confidence intervals, especially 
when analytical solutions are impractical. Many R packages often provide 
basic resampling methods but lack specialized support for complex structures.
Such structures may contain dependence and variable which should not be resampled.
Analysis of these structures can be unwieldy to investigate in other packages. 
`Purify` fills this gap by providing a flexible framework to enable data 
scientists and researchers to perform and compare targeted, customizable 
resampling schemes that account for data structure. Further, the methods allow 
for user-defined functions, allowing use of outside models, making `Purify` 
ideal for rigorous hypothesis testing and model evaluation. 

The package’s support for stratified and segmented sampling further allows users 
to address scenarios with grouped or ordered data (even under dependence), 
providing a critical resource for modern applied statistical research. By 
incorporating sophisticated resampling techniques, `Purify` enhances the 
robustness and reliability of statistical inferences drawn from complex 
(in particular unbalanced) datasets.

Permutation tests can be naturally computational intensive and speed is an 
important consideration throughout `Purify`. Users can use it in a variety of 
problems. Additional functions provide use of resampling in the context of
cross-validation. Supplemental functions provide visualizations and summary 
functions to illuminate the methods and results. Detailed documentation makes 
`Purify` accessible to users of varying statistical understanding.


# Package Functionality

A primary function in `Purify` is `resample()` with clear input parameters 
to simplify the process. The multistep selection allows for combinations of 
dependent data, inbalanced data, and resampling to be performed with and without 
replacment. The flexibility enables users to adapt `Purify` to diverse data 
contexts and hypothesis-testing requirements. See also `cross_validation()`.

Supporting functions such as `summarize_resample()` provide additional 
information to the user. Visualization such as `plot_strata_bar()` or 
`plot_strat_box()` visualize group sizes in stratified samples.


## Example

`Purify` provides several in-depth vignettes in the package or at its 
[website](https://jrvanderdoes.github.io/purify/).

- An *Introduction* vignette describes the core features of `purify' 
  and includes simulations and real data examples to demonstrate the functions.
- A *cats* vignette details a case scenario on real data.

We consider a subset of the cats data set below, where we use sex and body 
weight to estimate heart weight. In this subset, we drop some female cats so 
that the data is highly imbalanced, yet if the data is correct then sex and 
body weight are important. While the sample is small, this mismatch is common 
in many surveys.
```{r setup, echo=FALSE}
library(purify)
library(ggplot2)
```

```{r example_plot, echo=FALSE}
ggplot() +
  geom_point(aes(x=Bwt, y=Hwt, col=Sex,shape = Sex),data=subcats) +
  theme_bw() +
  theme(axis.title = element_text(size=22),
        axis.text = element_text(size=18),
        legend.position = c(.2, .8),
        legend.title = element_blank(),
        legend.text = element_text(size=14)) +
  scale_color_discrete(labels = c('Female', 'Male')) +
  scale_shape_manual(labels = c('Female', 'Male'),
                       values = c(16,3))
```

```{r example}  
summ_function <- function(data) {
  coef(summary(lm(Hwt ~ ., data = data)))
}

set.seed(1234)
summ_function(subcats)

# Perform resampling
results <- resample(data = subcats, fn = summ_function, M = 1000,
                             strata='Sex',stratify_sizes=mean)
summarize_resample(results)
```

Although the female cats clearly have a lower heart weight even for the same 
body weight, the traditional linear model does not detect this. The stratified 
approach does detect the difference without sacrificing estimation of the bodyweight.

# Implementation

`Purify` is implemented in R, following standard stylization and using 
vectorized operations for efficient computation. The package's modular design 
and clear documentation make it easy to adapt to various research needs, 
allowing users to integrate their own statistical functions or modify resampling 
parameters to meet specific analytical requirements. `Purify` has been used 
in @tetui:etal:2022, @scsrubook, and several upcoming papers.


# Acknowledgements

Development of the `Purify` package was inspired by foundational methods in 
statistical resampling and permutation testing. Special thanks to the 
open-source R community for support and resources.

Contributions to `Purify` are welcome and notable recognition is given to all 
who raise awareness of deficiencies in the package via the GitHub repository.


# References


<!--The paper should be between 250-1000 words.-->

<!--See an example paper at [website](https://joss.readthedocs.io/en/latest/example_paper.html).-->

<!--Format details at [website](https://joss.readthedocs.io/en/latest/paper.html), perhaps also see [website](https://joss.readthedocs.io/en/latest/submitting.html)-->


