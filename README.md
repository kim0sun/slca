
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

## Syntax

## Model Examples

``` r
library(slca)
lta <- slca(L1[3] ~ x1 + y1 + z1, L2[3] ~ x2 + y2 + z2, L3[3] ~ x3 + y3 + z3,
            L1 ~ L2, L2 ~ L3, constraints = c("L1", "L2", "L3"))
# plot(lta)
jlca <- slca(L1[3] ~ x1 + x2 + x3, L2[3] ~ y1 + y2 + y3, L3[3] ~ z1 + z2 + z3,
             JC[3] ~ L1 + L2 + L3)
# plot(jlca)
lcamg <- slca(G1[3] ~ g1 + g2 + g3, G2[3] ~ h1 + h2 + h3, G3[3] ~ z1 + z2 + z3,
              JG[3] ~ L1 + L2 + L3, LC[3] ~ y1 + y2 + y3, 
              JG ~ LC)
# plot(lcamg)
```

## Data example (`gss7677`)

``` r
data("gss7677")
```
