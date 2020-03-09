
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mapspam2globiom

<!-- badges: start -->

<!-- badges: end -->

The goal of mapspam2globiom is to facilitate the creation of crop
distribution maps for a selected country, which can be used as input by
GLOBIOM. A modified version of the Spatial Production Allocation Model -
SPAM (Liang et al., 2006, 2009, 2014) is used to ‘pixelate’ national and
subnational crop statistics on a spatial grid. Apart from crop
statistics, SPAM input includes crop suitability maps, population maps
and any other spatial information that provides an indication on the
likely location of crops. See Van Dijk et al (2020) for more information
on the methodology.

## Installation

You can install the released version of mapspam2globiom from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("mapspam2globiom")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("iiasa/mapspam2globiom")
```
