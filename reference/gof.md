# Goodness-of-Fit Test for Fitted `slca` Model

Computes the AIC, BIC, and deviance statistic (G-squared) for assessing
the goodness-of-fit of a fitted `slca` model. If the `test` argument is
specified, absolute model fit can be evaluated using deviance
statistics.

## Usage

``` r
gof(object, ...)

# S3 method for class 'slcafit'
gof(
   object, ..., test = c("none", "chisq", "boot"),
   nboot = 100, plot = FALSE,
   maxiter = 100, tol = 1e-6, verbose = FALSE
)

# S3 method for class 'slcafit'
gof(
  object,
  ...,
  test = c("none", "chisq", "boot"),
  nboot = 100,
  plot = FALSE,
  maxiter = 100,
  tol = 1e-06,
  verbose = FALSE
)
```

## Arguments

- object:

  an object of class `slcafit`, or a list of `slcafit` objects. List
  names are used as model labels; unnamed elements are labeled `[[1]]`,
  `[[2]]`, and so on.

- ...:

  additional objects of class `slcafit` for comparison.

- test:

  a character string specifying the type of test to be conducted. If
  `"chisq"`, a chi-squared test is conducted. If `"boot"`, a bootstrap
  test is conducted.

- nboot:

  a positive number specifying the number of bootstrap rounds to be
  performed. Non-integer values are rounded up.

- plot:

  a logical value indicating whether to print histogram of G-squared
  statistics for boostrap samples, only for `test = "boot"`. The default
  is `FALSE`.

- maxiter:

  an integer specifying the maximum number of iterations allowed for the
  estimation process during each bootstrap iteration. The default is
  100.

- tol:

  a numeric value specifying the convergence tolerance for each
  bootstrap iteration. The default is `1e-6`.

- verbose:

  a logical value indicating whether to print progress updates on the
  number of bootstrapping rounds completed.

## Value

A `data.frame` containing the number of parameters (Df), loglikelihood,
AIC, BIC, G-squared statistics, and the residual degree of freedom for
each object. If a statistical test is performed (using `test`), the
result includes the corresponding p-value. For `test = "boot"`, failed
bootstrap replicates are omitted from the p-value calculation and
recorded in the `bootFail`, `boot.fail.msgs`, and `boot.all.msgs`
attributes. Absolute bootstrap samples retain the fitted data's
missing-value pattern. Nonconverged refits are counted as failures. With
\\B\\ successful replicates and \\b\\ statistics at least as large as
observed, the bootstrap p-value is \\(b + 1)/(B + 1)\\.

## See also

[compare](https://kim0sun.github.io/slca/reference/compare.md)

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
