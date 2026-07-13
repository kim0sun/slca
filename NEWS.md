# slca 1.4.1

* Fixed saturated-frequency handling for partially missing data when missing patterns map to candidate observed cells of equal length.
* Updated internal response encoding to use `-1` for missing values and zero-based category indices for C++ calculations.
* Kept simulated manifest responses as plain factor data frames without internal encoding attributes.
* Fixed `predict.slcafit()` when `newdata` is omitted.
* Clarified the documented structure of `convergence`, `predict()`, and simulated responses.
