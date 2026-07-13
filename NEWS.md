# slca 1.4.1

* Fixed saturated-frequency handling for partially missing data when missing patterns map to candidate observed cells of equal length.
* Updated internal response encoding to use `-1` for missing values and zero-based category indices for C++ calculations.
* Kept simulated manifest responses as plain factor data frames without internal encoding attributes.
* Fixed `predict.slcafit()` when `newdata` is omitted.
* Added validation for `predict()` inputs, simulation levels, and initial parameter lengths.
* Added validation for model formulas, regression latent outcomes, bootstrap counts, and simulation counts.
* Fixed simulated factor responses when some generated categories are absent in a sample.
* Fixed Hessian-based covariance extraction and regression confidence interval returns.
* Recorded bootstrap replicate failures in model-fit diagnostics instead of returning error objects.
* Clarified the documented structure of `convergence`, `predict()`, and simulated responses.
