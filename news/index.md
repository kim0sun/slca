# Changelog

## slca 1.4.3

- Added the reference to the accompanying Journal of Statistical
  Software article (Kim and Chung, 2026, <doi:10.18637/jss.v117.i08>) in
  `CITATION`, `DESCRIPTION`, and the package documentation.

## slca 1.4.2

CRAN release: 2026-10-27

- Filled missing defaults when reusing historical control objects, and
  ignored saturated-table convergence metadata when comparing otherwise
  identical datasets.

- Prevented undefined subtraction in posterior calculations when a
  latent-state subtree has zero likelihood, retaining zero posterior
  mass for that path.

- Restored the 1.4.0 internal response convention (`0` for missing,
  `1,...,K` for categories), retaining missing-score and range-check
  fixes. Continued estimation rebuilds saved response caches, and model
  comparison ignores derived encoding caches. Public labels and fitted
  numerical results are unchanged.

- Retained existing zero constraints when re-estimating fitted objects
  unless `fix2zero` is supplied explicitly, and rejected unknown,
  unnamed or duplicate control options before estimation.

- Added warnings for numerically rank-deficient information matrices
  without changing existing covariance calculations. Skipped bootstrap
  histograms when a zero likelihood ratio bypasses simulation.

- Fixed out-of-bounds reads when reordering or summarizing fitted
  objects saved with the older response encoding, including the bundled
  NLSY fits. Native likelihood calculations now reject out-of-range
  response codes.

- Regenerated the bundled NLSY objects with current preprocessing and
  calculation code at exactly the original CRAN 1.4.0 parameters,
  without refitting. Original convergence history and numerical results
  are retained; missing control defaults and current internal metadata
  are populated.

- Increased the default absolute-bootstrap iteration limit in
  [`gof()`](https://kim0sun.github.io/slca/reference/gof.md) from 100 to
  5000; explicit `maxiter` values and
  [`compare()`](https://kim0sun.github.io/slca/reference/compare.md)
  defaults are unchanged.

## slca 1.4.1

CRAN release: 2026-10-23

- Aligned formula-environment regression covariates by their observation
  IDs before transformations, including factors and matrices. Unnamed
  environment covariates now require explicit positional alignment;
  ambiguous original-length inputs receive actionable errors.

- Reported saturated-table EM convergence and withheld absolute fit
  statistics for unfinished iterations.

- Aligned regression covariates by row identity even with equal row
  counts; added explicit positional alignment.

- Clarified categorical level requirements for constrained indicators
  and added regression coverage.

- Required final-stage convergence for hybrid bootstrap refits.

- Declared testthat \>= 3.1.7 for the mocked-binding test helpers.

- Corrected missing-item scores used for standard errors and covariance
  matrices.

- Included compatible unobserved cells in large saturated response
  tables; oversized tables now warn and return unavailable absolute fit
  statistics.

- Preserved missingness and rejected nonconverged fits in absolute
  bootstrap tests, with finite-sample p-values; relative tests reject
  negative observed likelihood ratios.

- Corrected reference logits and Hessian transformations after class
  reordering.

- Fixed confidence interval subsets and regression odds-ratio intervals,
  multiple starts with zero constraints, and simulated log-likelihoods
  for multiple measurement blocks.

- Assigned modal ties to the first class consistently, rejected cyclic
  model structures, and excluded all-indicator-missing observations as
  documented.

- Fixed saturated-frequency handling for partially missing data when
  missing patterns map to candidate observed cells of equal length.

- Updated internal response encoding to use `-1` for missing values and
  zero-based category indices for C++ calculations.

- Kept simulated manifest responses as plain factor data frames without
  internal encoding attributes.

- Fixed
  [`predict.slcafit()`](https://kim0sun.github.io/slca/reference/predict.slcafit.md)
  when `newdata` is omitted.

- Added validation for
  [`predict()`](https://rdrr.io/r/stats/predict.html) inputs, simulation
  levels, and initial parameter lengths.

- Added validation for model formulas, regression latent outcomes,
  bootstrap counts, and simulation counts.

- Fixed simulated factor responses when some generated categories are
  absent in a sample.

- Fixed Hessian-based covariance extraction and regression confidence
  interval returns.

- Fixed regression standard errors and Wald p-values in
  `summary.reg.slca()`.

- Hardened [`gof()`](https://kim0sun.github.io/slca/reference/gof.md)
  list inputs, [`do.call()`](https://rdrr.io/r/base/do.call.html) calls,
  and bias-adjusted regression internals.

- Recorded bootstrap replicate failures in model-fit diagnostics instead
  of returning error objects.

- Strengthened the relative bootstrap likelihood-ratio test with
  convergence checks, multiple starting values, missing-pattern
  preservation, and finite-sample p-values.

- Clarified the documented structure of `convergence`,
  [`predict()`](https://rdrr.io/r/stats/predict.html), and simulated
  responses.
