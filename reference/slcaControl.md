# Control Parameters for `slca` Estimation

Specifies control parameters for estimating `slca` model.

## Usage

``` r
slcaControl(
  em.iterlim = 5000,
  em.tol = 1e-08,
  nlm.iterlim = 1000,
  nlm.tol = 1e-10,
  init.param = NULL,
  nrep = 1,
  test.iter = 500,
  hessian = FALSE,
  na.rm = FALSE,
  verbose = FALSE
)
```

## Arguments

- em.iterlim:

  an integer specifying the maximum number of iterations allowed for the
  EM algorithm. The default is `5000`.

- em.tol:

  a numeric value setting the tolerance for convergence of the EM
  algorithm. The default is `1e-8`.

- nlm.iterlim:

  an integer specifying the maximum number of iterations allowed when
  using the `nlm` function for estimation. The default is `1000`.

- nlm.tol:

  a numeric value setting the tolerance for convergence of the `nlm`
  function. The default is `1e-10`.

- init.param:

  a numeric vector specifying the initial parameter values for
  estimation.

- nrep:

  an integer specifying the number of estimation trials. The default is
  `1`. Details for generating initial parameter set is described below.

- test.iter:

  an integer specifying the maximum number of iterations allowed for
  parameter testing. The default is `500`.

- hessian:

  a logical value indicating whether to calculate Hessian via `nlm`
  function numerically, if so, `vcov` method can provide
  variance-covariance matrix with Hessian instead of
  outer-product-of-gradients (OPG). The default is `FALSE`.

- na.rm:

  a logical value indicating whether to remove observations containing
  missing values (`NA`). The default is `FALSE`. Details for treating
  missing data is described below.

- verbose:

  a logical value indicating whether to display progress updates during
  the estimation process. The default is `FALSE`.

  A `list` containing control parameters for `slca` estimation,
  including the specified iteration limits, tolerances, and additional
  options.

## Details

**Missing data:** Missing data are handled in two ways. If all manifest
variables for an observation are missing, the case is excluded (listwise
deletion). For partially missing data, the model assumes Missing At
Random (MAR) and uses following algorithm to integrate over the missing
values. In the E-step, posterior probabilities are computed using only
the observed items. In the M-step, parameter updates use these posterior
probabilities, with expected counts for missing responses distributed
according to current parameter estimates.

**Local maxima and multiple starting values:** The EM algorithm may
converge to a local rather than a global maximum. To reduce this risk,
the `nrep` option allows multiple repetitions with different initial
values. PI parameters are initialized equally (e.g., 0.5 for two
classes), while TAU and RHO parameters are given small random
perturbations to uniform probabilities, normalized within each parent
state. The `test.iter` option runs a limited number of iterations for
each initial set, and the best-performing set is then used for full
convergence, improving the chance of reaching the global maximum.

## See also

[slca](https://kim0sun.github.io/slca/reference/slca.md)
