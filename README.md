
<!-- README.md is generated from README.Rmd. Please edit that file -->
tectr
=====

[![Travis-CI Build Status](https://travis-ci.org/sflippl/tectr.svg?branch=master)](https://travis-ci.org/sflippl/tectr) [![Coverage Status](https://img.shields.io/codecov/c/github/sflippl/tectr/master.svg)](https://codecov.io/github/sflippl/tectr?branch=master)

R provides powerful opportunities to outsource everyday thought processes in data analysis. However, adapting these mechanisms for application-specific thought processes is expensive and difficult. Whenever a statistician builds a plot he needs to specify the variable name, scale transformations etc. `tectr` supports automation of these parameters. In particular, it supports automated visualization of complex datasets.

You can install tectr from github with:

``` r
# install.packages("devtools")
devtools::install_github("sflippl/tectr")
```

For now, an introduction to `tectr` can be found in my [Bachelor's Thesis](https://github.com/sflippl/bachelor-thesis/blob/master/_book/bachelor-thesis.pdf).

The package [`vdem.tectr`](https://github.com/sflippl/vdem.tectr) applies `tectr` to the [V-Dem database](v-dem.net).
