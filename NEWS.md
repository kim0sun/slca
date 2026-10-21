# slca 1.4.1

* Corrected missing-item scores used for standard errors and covariance matrices.
* Included compatible unobserved cells in large saturated response tables; oversized tables now warn and return unavailable absolute fit statistics.
* Preserved missingness and rejected nonconverged fits in absolute bootstrap tests, with finite-sample p-values; relative tests reject negative observed likelihood ratios.
* Corrected reference logits and Hessian transformations after class reordering.
* Fixed confidence interval subsets and regression odds-ratio intervals, multiple starts with zero constraints, and simulated log-likelihoods for multiple measurement blocks.
* Assigned modal ties to the first class consistently, rejected cyclic model structures, and excluded all-indicator-missing observations as documented.
* Fixed saturated-frequency handling for partially missing data when missing patterns map to candidate observed cells of equal length.
* Updated internal response encoding to use `-1` for missing values and zero-based category indices for C++ calculations.
* Kept simulated manifest responses as plain factor data frames without internal encoding attributes.
* Fixed `predict.slcafit()` when `newdata` is omitted.
* Added validation for `predict()` inputs, simulation levels, and initial parameter lengths.
* Added validation for model formulas, regression latent outcomes, bootstrap counts, and simulation counts.
* Fixed simulated factor responses when some generated categories are absent in a sample.
* Fixed Hessian-based covariance extraction and regression confidence interval returns.
* Fixed regression standard errors and Wald p-values in `summary.reg.slca()`.
* Hardened `gof()` list inputs, `do.call()` calls, and bias-adjusted regression internals.
* Recorded bootstrap replicate failures in model-fit diagnostics instead of returning error objects.
* Strengthened the relative bootstrap likelihood-ratio test with convergence checks, multiple starting values, missing-pattern preservation, and finite-sample p-values.
* Clarified the documented structure of `convergence`, `predict()`, and simulated responses.
