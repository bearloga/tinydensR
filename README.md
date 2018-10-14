# tinydensR

An [RStudio add-in](https://shiny.rstudio.com/articles/gadgets.html) for playing with distribution parameters and visualizing the resulting probability density and mass functions.

## Installation

```R
if ( !('devtools' %in% installed.packages()) ) install.packages("devtools")

devtools::install_github("bearloga/tinydensR")
```

## Distributions

- Univariate
  - Discrete
    - [x] Binomial
    - [x] Hypergeometric
    - [x] Poisson
  - Continuous
    - [x] Beta
    - [x] Cauchy
    - [x] Chi-squared
    - [x] Exponential
    - [x] Gamma
    - [x] Inverse-gamma
    - [x] Normal
    - [x] Log-Normal
    - [x] Student-t
    - [x] Weibull
    - [x] [Exponentiated Weibull](https://en.wikipedia.org/wiki/Exponentiated_Weibull_distribution)
- Multivariate
  - Discrete
    - [ ] Multinomial
  - Continuous
    - [ ] Bivariate Normal

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
