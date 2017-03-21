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
    - [ ] Hypergeometric
    - [ ] Poisson
  - Continuous
    - [x] Beta
    - [x] Chi-squared
    - [ ] Exponential
    - [ ] Gamma
    - [x] Inverse-gamma
    - [x] Normal
- Multivariate
  - Discrete
    - [ ] Multinomial
  - Continuous
    - [ ] Bivariate Normal
    - [ ] Dirichlet

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
