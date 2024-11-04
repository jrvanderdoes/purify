---
title: 'Purify: A R package for Bootstrapping'
tags:
  - R
  - Statistics
  - Bootstrap Resampling
  - Simulation
authors:
  - name: Jeremy VanderDoes
    orcid: 0000-0000-0000-0000
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
---

# Summary

`Purify` is an R package developed for bootstrap resampling and permutation testing, specifically designed to handle data with a dependent output variable and multiple predictors. This package provides an efficient implementation for resampling and applying custom functions across resampled datasets, enabling robust statistical analysis, hypothesis testing, and model evaluation. The core functionality of `Purify` lies in its ability to permute user-selected predictor variables while keeping the output variable unchanged, which is useful in studying the stability of statistical inferences, in particular with unbalanced dataset.

# Statement of need

Bootstrap resampling is a widely-used technique for estimating the distribution of a statistic by resampling from the data with replacement. This approach is crucial for performing accurate hypothesis testing and estimating confidence intervals, especially when analytical solutions are difficult to obtain. Existing R packages for resampling (e.g., `boot`, `rsample`) focus primarily on simple applications of the bootstrap but may lack flexibility for complex, dependent data structures.

`Purify` fills this gap by providing a customizable framework for permuting predictor variables while keeping the output variable constant, allowing users to apply various statistical functions and assess the impact of different predictor combinations on outcomes. This package is particularly valuable for researchers, data scientists, and statisticians working in fields such as econometrics, finance, and machine learning.


# Information

The paper should be between 250-1000 words.

See an example paper at [website](https://joss.readthedocs.io/en/latest/example_paper.html).

Format details at [website](https://joss.readthedocs.io/en/latest/paper.html), perhaps also see [website](https://joss.readthedocs.io/en/latest/submitting.html)


# Acknowledgements

# References
