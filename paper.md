---
title: 'Purify: A R package for Bootstrapping'
tags:
  - R
  - Statistics
  - Bootstrap Resampling
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
date: 4 November 2024
bibliography: paper.bib
nocite: davison:hinkley:1997 efron:tibshirani:1998 wickham:2019
---

# Summary

`Purify` is an R package designed for bootstrap resampling and permutation testing, aimed at researchers and practitioners who analyze complex datasets with a dependent output variable and multiple predictors. This package enables users to perform robust statistical analyses by allowing permutations of predictor variables while keeping the output variable unchanged. `Purify` offers four versatile resampling methods: `simple`, `stratify`, `sliding`, and `segment`, which can be tailored to specific data structures and research questions. With its intuitive interface and customizable options, `Purify` streamlines the process of hypothesis testing and estimation of statistical significance.

# Statement of need

Bootstrap resampling is fundamental for estimating the distribution of a statistic, testing hypotheses, and deriving confidence intervals, especially when analytical solutions are impractical. Standard R packages often provide basic resampling methods but lack specialized support for complex structures, such as dependent and independent variables with flexible resampling schemes. `Purify` fills this gap by enabling data scientists and researchers to perform targeted, customizable resampling while retaining dependencies between variables, making it ideal for rigorous hypothesis testing and model evaluation. The package’s support for stratified and segmented sampling further allows users to address scenarios with grouped or ordered data, providing a critical resource for modern applied statistical research. By incorporating sophisticated resampling techniques, `Purify` enhances the robustness and reliability of statistical inferences drawn from complex (in particular unbalanced) datasets.


# Package Functionality

A primary function in `Purify` is `resample_function()`, with four available resampling methods:

1. `Simple`: Standard permutation of predictor variables without additional structure.
2. `Stratify`: Resampling within specified strata to maintain group structure.
3. `Sliding`: Applying a sliding window to generate resamples over time-ordered data.
4. `Segment`: Dividing data into segments and resampling within each.

This flexibility enables users to adapt `Purify` to diverse data contexts and hypothesis-testing requirements. 

Other permutation functions such as \code{resample()} are included for more complex cases. The packages also includes functions for analyzing the resultant data such as `boxplot_strata()` for stratified samples.


## Example Usage

The following example demonstrates the use of `Purify` with simulated data, applying a mean squared error (MSE) calculation over 1,000 resamples using the simple method:

```{r eval=False}
library(purify)

# Simulate data
set.seed(123)
n <- 100
data <- data.frame(
  output = rnorm(n),
  predictor1 = rnorm(n),
  predictor2 = rnorm(n)
)

# Define a custom function to calculate MSE
mse_function <- function(data) {
  mean((data$output - predict(lm(output ~ ., data = data)))^2)
}

# Perform resampling with the 'simple' method
results <- resample_function(data = data, fn = mse_function, M = 1000, method = 'simple')
```

Model coefficients can also be examined under permutation; see documentation. However, if a use case is too complex for the implemented \code{resample_function()}, the function \code{resample()} can be used to directly return the resampled data.


# Implementation

`Purify` is implemented in R, using vectorized operations for efficient computation. The package's modular design and clear documentation make it easy to adapt to various research needs, allowing users to integrate their own statistical functions or modify resampling parameters to meet specific analytical requirements. `Purify` was used in @tetui:etal:2022 and several upcoming papers.


# Acknowledgements

Development of the `Purify` package was inspired by foundational methods in statistical resampling and permutation testing. Special thanks to the open-source R community for support and resources.


# References


# Contributing

Contributions to `Purify` are welcome. Please submit pull requests or open issues on the GitHub repository.



<!--The paper should be between 250-1000 words.-->

<!--See an example paper at [website](https://joss.readthedocs.io/en/latest/example_paper.html).-->

<!--Format details at [website](https://joss.readthedocs.io/en/latest/paper.html), perhaps also see [website](https://joss.readthedocs.io/en/latest/submitting.html)-->


