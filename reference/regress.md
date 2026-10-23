# Regress Exogenous Variables on Latent Variables

Performs regression analysis to examine the influence of exogenous
(external) variables on latent class variables in an estimated `slca`
model. The function uses logistic regression with a three-step approach
to account for measurement error.

## Usage

``` r
regress(object, ...)

# S3 method for class 'slcafit'
regress(
   object, formula, data = parent.frame(),
   imputation = c("modal", "prop"),
   method = c("naive", "BCH", "ML"), ...,
   align = c("rownames", "position")
)

# S3 method for class 'slcafit'
regress(
  object,
  formula,
  data = parent.frame(),
  imputation = c("modal", "prop"),
  method = c("naive", "BCH", "ML"),
  ...,
  align = c("rownames", "position")
)
```

## Arguments

- object:

  an object of class `slcafit`.

- ...:

  additional arguments.

- formula:

  a formula specifying the regression model. The left-hand side must be
  a single latent class variable from the estimated model.

- data:

  an optional `data.frame` containing the exogenous variables of
  interest. Variables absent from the fitted data and `data` are taken
  from the formula's environment and aligned before formula
  transformations are evaluated. Such vectors and factors need
  observation IDs in [`names()`](https://rdrr.io/r/base/names.html);
  matrices and data frames need observation IDs in
  [`rownames()`](https://rdrr.io/r/base/colnames.html).

- imputation:

  a character string specifying the imputation method for latent class
  assignment. Options include:

  - `"modal"`: Assigns each individual to the latent class with the
    highest posterior probability.

  - `"prop"`: Uses posterior probabilities as proportional assignment
    weights.

- method:

  a character string specifying the method to adjust for bias in the
  three-step approach. Options include:

  - `"naive"`: A simple approach without correction for classification
    error.

  - `"BCH"`: The bias-adjusted Bolck, Croon, and Hagenaars method.

  - `"ML"`: A maximum likelihood approach that accounts for
    classification error.

- align:

  how supplied covariate rows are aligned with the fitted data. The
  default, `"rownames"`, requires unique, nonempty row names and matches
  them to the fitted observations, regardless of row count or order.
  Extra rows are allowed, but every fitted observation must be present.
  Automatic row numbers work only if they still identify the original
  observations; do not reset them after subsetting or reordering. Use
  `"position"` explicitly for data already in fitted-observation order;
  it requires exactly the same number of rows and ignores row names.
  There is no automatic positional fallback. These rules also apply
  separately to each environment covariate, even when its length already
  equals the fitted sample size. Unnamed environment covariates require
  explicit `align = "position"` and fitted-row order. After observations
  have been excluded, an unnamed original-length vector cannot be
  aligned safely: supply `data` with preserved row IDs, or name the
  vector with its original observation IDs. Matrix covariates remain
  matrices and factor levels are retained. Unnamed scalar formula
  parameters are left unchanged, not recycled as covariates. Non-scalar
  transformation settings (such as spline knots or factor levels) must
  be written inline or captured inside a transformation function;
  otherwise they are treated as row inputs. Transformation functions
  must use their explicit covariate arguments rather than retrieve
  hidden observation vectors. Use explicit covariate columns instead of
  dynamic [`get()`](https://rdrr.io/r/base/get.html) or
  [`eval()`](https://rdrr.io/r/base/eval.html) lookups with row-ID
  alignment.

## Value

A `list` of class `reg.slca` with the following components:

- coefficients:

  A matrix of regression coefficients representing the odds ratios for
  each latent class against the baseline class (the last class).

- std.err:

  A matrix of standard errors corresponding to the regression
  coefficients.

- vcov:

  The variance-covariance matrix of the regression coefficients.

- dim:

  The dimensions of the coefficients matrix.

- ll:

  The log-likelihood of the regression model.

The `summary` function can be used to display the regression
coefficients, standard errors, Wald statistics, and p-values. The
standard errors are derived by numerically calculated Hessian matrix
from `nlm` function.

## References

Vermunt, J. K. (2010). Latent Class Modeling with Covariates: Two
Improved Three-Step Approaches. Political Analysis, 18(4), 450–469.
http://www.jstor.org/stable/25792024

## Examples

``` r
library(magrittr)
names(nlsy97)
#>  [1] "SEX"     "RACE"    "ESMK_98" "FSMK_98" "DSMK_98" "HSMK_98" "EDRK_98"
#>  [8] "CDRK_98" "WDRK_98" "BDRK_98" "EMRJ_98" "CMRJ_98" "OMRJ_98" "SMRJ_98"
#> [15] "ESMK_03" "FSMK_03" "DSMK_03" "HSMK_03" "EDRK_03" "CDRK_03" "WDRK_03"
#> [22] "BDRK_03" "EMRJ_03" "CMRJ_03" "OMRJ_03" "SMRJ_03" "ESMK_08" "FSMK_08"
#> [29] "DSMK_08" "HSMK_08" "EDRK_08" "CDRK_08" "WDRK_08" "BDRK_08" "EMRJ_08"
#> [36] "CMRJ_08" "OMRJ_08" "SMRJ_08"
nlsy_jlcpa %>% regress(SMK_98 ~ SEX, nlsy97)
#> Coefficients:     
#> class  (Intercept)  SEXFemale
#>   1/3   0.3983      -0.4445  
#>   2/3  -0.6614       0.0804  
# \donttest{
nlsy_jlcpa %>% regress(PROF ~ SEX, nlsy97)
#> Coefficients:     
#> class  (Intercept)  SEXFemale
#>   1/4   0.436       -0.106   
#>   2/4   0.157       -0.210   
#>   3/4   0.443       -0.221   
# }
```
