
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phecodemap

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

The phecodemap package builds a shiny app to visualize the hierarchy of
Phecode Mapping with ICD-9 and ICD-10-cm. The same Phecode hierarchy is
displayed in two ways: as a sunburst plot and as a tree.

## Installation

Install development version from GitHub:

``` r
install.packages("remotes")
remotes::install_github("celehs/phecodemap")
```

## Usage

This is a basic example which shows you how to run the `phecodemap` app.
Remember you need to get access to the data and save it to your local
computer. In order to guarantee some dependencies are loaded, you must
use `library(phecodemap)` beforehand, instead of directly running
`phecodemap::run_app()`.

``` r
library(phecodemap)
run_app()
```

## App instructions

See the [App
instructions](https://celehs.github.io/phecodemap/articles/main.html)
guide to learn how to use the app.

## Citations

  - Wei WQ, Bastarache LA, Carroll RJ, Marlo JE, Osterman TJ, Gamazon
    ER, Cox NJ, Roden DM, Denny JC. Evaluating phecodes, clinical
    classification software, and ICD-9-CM codes for phenome-wide
    association studies in the electronic health record. PLoS One. 2017
    Jul 7;12(7):e0175508. <https://doi.org/10.1371/journal.pone.0175508>

  - Wu, P., Gifford, A., Meng, X., Li, X., Campbell, H., Varley, T.,
    Zhao, J., Carroll, R., Bastarache, L., Denny, J. C., Theodoratou,
    E., & Wei, W. Q. (2019). Mapping ICD-10 and ICD-10-CM Codes to
    Phecodes: Workflow Development and Initial Evaluation. JMIR medical
    informatics, 7(4), e14325. <https://doi.org/10.2196/14325>
