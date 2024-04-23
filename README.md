
<!-- README.md is generated from README.Rmd. Please edit that file -->

# slca

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/slca)](https://CRAN.R-project.org/package=slca)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/slca?color=blue)](https://r-pkg.org/pkg/slca)
[![R-CMD-check](https://github.com/kim0sun/slca/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kim0sun/slca/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`slca` provides comprehensive tools for the implementation of Structural
Latent Class Models (SLCM), including Latent Transition Analysis (LTA;
Linda M. Collins and Stephanie T. Lanza, 2009)
<doi:10.1002/9780470567333>, Latent Class Profile Analysis (LCPA; Hwan
Chung et al., 2010) <doi:10.1111/j.1467-985x.2010.00674.x>, and Joint
Latent Class Analysis (JLCA; Saebom Jeon et al., 2017)
<doi:10.1080/10705511.2017.1340844>, and any other extended models
involving multiple latent class variables.

## Installation

You can install the released version of slca from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("slca")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kim0sun/slca")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(slca)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
