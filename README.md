# pacta.multi.loanbook <a href="https://rmi-pacta.github.io/pacta.multi.loanbook"><img src="man/figures/logo.png" align="right" height="31" /></a>


<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/RMI-PACTA/pacta.multi.loanbook/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RMI-PACTA/pacta.multi.loanbook/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/pacta.multi.loanbook)](https://CRAN.R-project.org/package=pacta.multi.loanbook)
[![pacta.multi.loanbook status badge](https://rmi-pacta.r-universe.dev/badges/pacta.multi.loanbook)](https://rmi-pacta.r-universe.dev/pacta.multi.loanbook)
<!-- badges: end -->

The `pacta.multi.loanbook` package offers a standardized, user-friendly way to calculate climate alignment metrics for multiple loan books, based on the PACTA methodology.

Designed for financial supervisory contexts, it simplifies climate alignment analysis across many lending institutions. The package streamlines steps to prevent repetition while allowing flexibility to tailor the analysis for specific project needs, providing valuable insights into transition alignment and risk.

## Installation
Install the development version of the package from GitHub with:

``` r
# install.packages("pak")
pak::pak("RMI-PACTA/pacta.multi.loanbook")
```

Verify the installation with: 
``` r
library(pacta.multi.loanbook)
```

## Usage

Please consult the following resources for instructions on using the package. The cookbook provides a detailed overview covering the setup of the necessary software environment, obtaining and preparing input data, running the analysis, and interpreting the results.

- [Cookbook](https://rmi-pacta.github.io/pacta.multi.loanbook/articles/cookbook.html): A comprehensive guide for setting up and running the analysis.

In particular, we recommend focusing on the sections:

- [Preparatory Steps](https://rmi-pacta.github.io/pacta.multi.loanbook/articles/cookbook.html#preparatory-steps): for setting up required input data and software.
- [Running the Analysis](https://rmi-pacta.github.io/pacta.multi.loanbook/articles/cookbook.html#running-the-analysis) for generating outputs.


## Intepretation

Understanding your analysis outputs is key to drawing meaningful conclusions. The following sections of the cookbook guide how to read and interpret the generated outputs:

- [Interpretation of Results](https://rmi-pacta.github.io/pacta.multi.loanbook/articles/cookbook.html#interpretation-of-results): for guidance on how to interpret the generated outputs
- [Understanding the Data](https://rmi-pacta.github.io/pacta.multi.loanbook/articles/data_dictionary.html): for a precise description of every dataset. 
- [Advanced Use Cases](https://rmi-pacta.github.io/pacta.multi.loanbook/articles/cookbook.html#advanced-use-cases): for complex, real-world scenarios, with guidance on adapting analyses to address specific research questions.
