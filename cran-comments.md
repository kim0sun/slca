## slca 1.4.2

This is a bug-fix release addressing the additional-issue reports for
version 1.4.1 (clang-ASAN, gcc-ASAN, M1-SAN):

```
AddressSanitizer: heap-buffer-overflow ... src/upward.cpp:14 in upInit(...)
```

triggered by the `reorder.slcafit` example on the bundled `nlsy_jlcpa`
object.

### Cause

Version 1.4.1 changed the internal response encoding passed to the C++ code
(zero-based categories, `-1` for missing), while the bundled fitted objects
`nlsy_jlcpa` and `nlsy_jlta` still carried the response caches saved under
the 1.4.0 convention (one-based categories, `0` for missing). Reading an
observation with the maximal category code therefore indexed one element
past the end of the `rho` block.

### Fix

* Restored the 1.4.0 internal response convention (`0` = missing,
  `1,...,K` = categories) throughout `src/` and `R/`.
* `reorder()` and `estimate()` on fitted objects rebuild the internal
  response caches from the stored model frame instead of trusting a saved
  cache; `compare()` ignores derived caches when comparing datasets.
* `upInit()` now validates every response code against the number of
  categories and raises an R error (via `Rcpp::stop()`) instead of reading
  out of bounds, so objects saved under any encoding can no longer cause
  undefined memory access.
* The bundled NLSY objects were regenerated with the current code at exactly
  the original 1.4.0 parameters (no refitting); printed results are
  unchanged.
* Small unrelated fixes listed in NEWS.md (undefined `-Inf - (-Inf)` in
  posterior calculation for zero-likelihood subtrees, rank-deficiency
  warnings, control-list validation).

### Verification

* The 1.4.1 failure was reproduced locally under clang 18 ASan/UBSan and no
  longer occurs with 1.4.2.
* `R CMD check --run-donttest` of 1.4.2 with the package compiled under
  `-fsanitize=address,undefined` (clang 18, R 4.3.3, Ubuntu 24.04):
  all examples and tests pass with no sanitizer diagnostics.
* `R CMD check --as-cran --run-donttest` (ordinary build): 0 errors,
  0 warnings. One NOTE on installed size (bundled example fits in `data/`).

## Test environments

* Ubuntu 24.04, R 4.3.3, clang 18 (ASan/UBSan build and ordinary build)
* macOS arm64, R 4.5.0, Apple clang 21 (ordinary build; previous candidate
  checks)
