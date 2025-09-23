# amlrian

<!-- badges: start -->
[![R-CMD-check](https://github.com/SWFSC/amlrian/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SWFSC/amlrian/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

A collection of functions, especially for Shiny apps connected to AMLR databases, used by other [AMLR](https://www.fisheries.noaa.gov/about/antarctic-ecosystem-research-division-southwest-fisheries-science-center) packages. 

## Installation

You can install the development version of `amlrian` from [GitHub](https://github.com/) with the following. Using the [pak](https://pak.r-lib.org/) package may work around some GitHub/certificate issues on NOAA machines.

``` r
# install.packages("devtools")
devtools::install_github("SWFSC/amlrian")

### OR ###
# install.packages("pak")
pak::pkg_install("SWFSC/amlrian")
```

## Run Shiny App

``` r
amlrian::shiny_test()
```

## Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
