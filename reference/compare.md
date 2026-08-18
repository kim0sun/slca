# Compare Two Fitted `slca` Models

Conducts a relative model fit test between two fitted SLCM models using
the deviance statistic.

## Usage

``` r
compare(
  model1,
  model2,
  test = c("none", "chisq", "boot"),
  nboot = 50,
  method = c("hybrid", "em", "nlm"),
  plot = FALSE,
  maxiter = 1000,
  tol = 1e-08,
  verbose = FALSE
)
```

## Arguments

- model1:

  an object of class `slcafit`.

- model2:

  another object of class `slcafit` to be compared with `model1`.

- test:

  a character string specifying the type of test to be conducted. If
  `"chisq"`, a chi-squared test is conducted. If `"boot"`, a bootstrap
  test is conducted.

- nboot:

  a positive number specifying the number of bootstrap iterations to
  perform (used only when `test = "boot"`). Non-integer values are
  rounded up. The default is 50.

- method:

  a character string specifying the estimation method for bootstrapping.

- plot:

  a logical value indicating whether to display a histogram of G-squared
  statistics for the bootstrap samples (applicable only for
  `test = "boot"`). The default is `FALSE`.

- maxiter:

  an integer specifying the maximum number of iterations allowed during
  each bootstrap estimation round. The default is 100.

- tol:

  numeric value setting the convergence tolerance for each bootstrap
  iteration. The default is `1e-6`.

- verbose:

  a logical value indicating whether to print progress updates on
  completed bootstrap iterations. The default is `FALSE`.

## Value

A `data.frame` containing the number of parameters (Df), loglikelihood,
AIC, BIC, G-squared statistics, and the residual degree of freedom for
each object. If a statistical test is conducted (via `test`), the
resulting p-value for the comparison is also included. For
`test = "boot"`, failed bootstrap replicates are omitted from the
p-value calculation and recorded in the `bootFail`, `boot.fail.msgs`,
and `boot.all.msgs` attributes.

## See also

[gof](https://kim0sun.github.io/slca/reference/gof.md)

## Examples

``` r
library(magrittr)
data <- gss7677[gss7677$COHORT == "YOUNG", ]
stat2 <- slca(status(2) ~ PAPRES + PADEG + MADEG) %>%
   estimate(data = data, control = list(verbose = FALSE))
stat3 <- slca(status(3) ~ PAPRES + PADEG + MADEG) %>%
   estimate(data = data, control = list(verbose = FALSE))
stat4 <- slca(status(4) ~ PAPRES + PADEG + MADEG) %>%
   estimate(data = data, control = list(verbose = FALSE))

gof(stat2, stat3, stat4)
#> Analysis of Goodness of Fit Table
#> 
#>       Df  logLik    AIC    BIC     Gsq Res. Df
#> stat2 21 -1179.2 2400.4 2489.5 115.979      53
#> stat3 32 -1139.3 2342.6 2478.5  36.204      42
#> stat4 43 -1135.4 2356.8 2539.4  28.423      31
gof(stat2, stat3, stat4, test = "chisq")
#> Analysis of Goodness of Fit Table
#> 
#>       Df  logLik    AIC    BIC     Gsq Res. Df Pr(>Chi)    
#> stat2 21 -1179.2 2400.4 2489.5 115.979      53 1.34e-06 ***
#> stat3 32 -1139.3 2342.6 2478.5  36.204      42   0.7226    
#> stat4 43 -1135.4 2356.8 2539.4  28.423      31   0.5993    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# \donttest{
gof(stat2, stat3, stat4, test = "boot")
#> Analysis of Goodness of Fit Table
#> 
#>       Df  logLik    AIC    BIC     Gsq Res. Df Pr(Boot)    
#> stat2 21 -1179.2 2400.4 2489.5 115.979      53   <2e-16 ***
#> stat3 32 -1139.3 2342.6 2478.5  36.204      42     0.21    
#> stat4 43 -1135.4 2356.8 2539.4  28.423      31     0.44    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# }

compare(stat3, stat4)
#> Analysis of Relative Model Fit
#> 
#> Model H0: stat3
#> Model H1: stat4
#>       Df  logLik    AIC    BIC    Gsq Res. Df
#> stat3 32 -1139.3 2342.6 2478.5               
#> stat4 43 -1135.4 2356.8 2539.4 7.7805      11
compare(stat3, stat4, test = "chisq")
#> Analysis of Relative Model Fit
#> 
#> Model H0: stat3
#> Model H1: stat4
#>       Df  logLik    AIC    BIC    Gsq Res. Df Pr(>Chi)
#> stat3 32 -1139.3 2342.6 2478.5                        
#> stat4 43 -1135.4 2356.8 2539.4 7.7805      11   0.7328
# \donttest{
compare(stat3, stat4, test = "boot")
#> Analysis of Relative Model Fit
#> 
#> Model H0: stat3
#> Model H1: stat4
#>       Df  logLik    AIC    BIC    Gsq Res. Df Pr(Boot)
#> stat3 32 -1139.3 2342.6 2478.5                        
#> stat4 43 -1135.4 2356.8 2539.4 7.7805      11     0.12
# }
```
