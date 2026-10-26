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
  verbose = FALSE,
  nrep = 3
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
  each bootstrap estimation round. The default is 1000.

- tol:

  numeric value setting the convergence tolerance for each bootstrap
  iteration. The default is `1e-8`.

- verbose:

  a logical value indicating whether to print progress updates on
  completed bootstrap iterations. The default is `FALSE`.

- nrep:

  a positive number specifying the number of starting values used for
  each model in a bootstrap iteration. The fitted-model parameters are
  used first and additional starting values are generated randomly. The
  default is 3.

## Value

A `data.frame` containing the number of parameters (Df), loglikelihood,
AIC, BIC, G-squared statistics, and the residual degree of freedom for
each object. If a statistical test is conducted (via `test`), the
resulting p-value for the comparison is also included. For
`test = "boot"`, failed bootstrap replicates are omitted from the
p-value calculation and recorded in the `bootFail`, `boot.fail.msgs`,
and `boot.all.msgs` attributes.

Bootstrap samples retain the fitted data's missing-value pattern.
Nonconverged refits and negative bootstrap likelihood ratios are counted
as failures. Hybrid refits must converge in their final `nlm` stage.
With \\B\\ successful replicates and \\b\\ statistics at least as large
as observed, the p-value is \\(b + 1)/(B + 1)\\. When the observed
likelihood ratio is effectively zero, the p-value is one; no bootstrap
samples or histogram are generated.

## See also

[gof](https://kim0sun.github.io/slca/reference/gof.md)

## Examples

``` r
library(magrittr)
set.seed(20261017)
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
#> stat2 21 -1179.2 2400.4 2489.0 115.980      53
#> stat3 32 -1139.3 2342.6 2477.7  36.204      42
#> stat4 43 -1135.4 2356.8 2538.3  28.424      31
gof(stat2, stat3, stat4, test = "chisq")
#> Analysis of Goodness of Fit Table
#> 
#>       Df  logLik    AIC    BIC     Gsq Res. Df Pr(>Chi)    
#> stat2 21 -1179.2 2400.4 2489.0 115.980      53 1.34e-06 ***
#> stat3 32 -1139.3 2342.6 2477.7  36.204      42   0.7225    
#> stat4 43 -1135.4 2356.8 2538.3  28.424      31   0.5992    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# \donttest{
# Five replicates illustrate the interface only; use more for inference.
absolute_boot <- gof(stat2, stat3, stat4, test = "boot",
                     nboot = 5, maxiter = 5000, tol = 1e-6)
absolute_boot
#> Analysis of Goodness of Fit Table
#> 
#>       Df  logLik    AIC    BIC     Gsq Res. Df Pr(Boot)
#> stat2 21 -1179.2 2400.4 2489.0 115.980      53   0.1667
#> stat3 32 -1139.3 2342.6 2477.7  36.204      42   0.1667
#> stat4 43 -1135.4 2356.8 2538.3  28.424      31   0.5000
attr(absolute_boot, "bootFail")
#> stat2 stat3 stat4 
#>     0     0     0 
# }

compare(stat3, stat4)
#> Analysis of Relative Model Fit
#> 
#> Model H0: stat3
#> Model H1: stat4
#>       Df  logLik    AIC    BIC    Gsq Res. Df
#> stat3 32 -1139.3 2342.6 2477.7               
#> stat4 43 -1135.4 2356.8 2538.3 7.7805      11
compare(stat3, stat4, test = "chisq")
#> Analysis of Relative Model Fit
#> 
#> Model H0: stat3
#> Model H1: stat4
#>       Df  logLik    AIC    BIC    Gsq Res. Df Pr(>Chi)
#> stat3 32 -1139.3 2342.6 2477.7                        
#> stat4 43 -1135.4 2356.8 2538.3 7.7805      11   0.7328
# \donttest{
# Use EM refits here; always inspect convergence failures.
relative_boot <- compare(stat3, stat4, test = "boot", method = "em",
                         nboot = 5, maxiter = 5000, tol = 1e-6)
relative_boot
#> Analysis of Relative Model Fit
#> 
#> Model H0: stat3
#> Model H1: stat4
#>       Df  logLik    AIC    BIC    Gsq Res. Df Pr(Boot)
#> stat3 32 -1139.3 2342.6 2477.7                        
#> stat4 43 -1135.4 2356.8 2538.3 7.7805      11      0.5
attr(relative_boot, "bootFail")
#> [1] 0
# }
```
