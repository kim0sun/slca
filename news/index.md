# Changelog

## slca 1.4.1

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
  list calls and bias-adjusted regression internals.
- Recorded bootstrap replicate failures in model-fit diagnostics instead
  of returning error objects.
- Clarified the documented structure of `convergence`,
  [`predict()`](https://rdrr.io/r/stats/predict.html), and simulated
  responses.
