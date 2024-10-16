# dropout <img src="man/figures/logo.png" align="right" width="150" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/hendr1km/dropout/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hendr1km/dropout/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/dropout)](https://CRAN.R-project.org/package=dropout)
<!-- badges: end -->

The `dropout` package offers tools for dropout analysis in survey data. It helps you identify and handle incomplete responses.

## dropout v2.2.0
The latest version of the dropout package introduces significant updates to the codebase, aimed at reducing unexpected behavior and minimizing dependencies by directly utilizing R's built-in C API.

If you need to access the previous version of the package, you can download it through [Figshare](https://figshare.com/articles/software/dropout/25355746/1?file=44902738).

## Installation
You can install the development version of `dropout` from GitHub using the following command:

```r
# Install the released version from CRAN
install.packages("dropout")

# development version from GitHub:
devtools::install_github("hendr1km/dropout")
```

## Features

- **drop_detect**: Detects participants who drop out of the survey by recognizing NA sequences up to the last question of the survey. Additionally, the function provides the column name and index where the dropout occurs.

- **dropout_summary**: Offers a high-level summary of dropout occurrences, providing key statistics to understand the patterns of participant dropouts across different survey questions.

For practical examples please refer to the package [vignette](https://hendr1km.github.io/dropout/articles/intro_to_dropout.html).

