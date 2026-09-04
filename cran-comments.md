## Test environments

* macOS 26.5.2, R 4.5.0

## R CMD check results

0 errors | 1 warning | 1 note

The warning is a compiler warning emitted from the R framework header
`R_ext/Boolean.h` with Apple clang 21 on macOS:

```
warning: unknown warning group '-Wfixed-enum-extension', ignored
```

No package source file is named in the warning.

The note is due to the local HTML Tidy installation:

```
Skipping checking HTML validation: 'tidy' doesn't look like recent enough HTML Tidy.
```

## Reverse dependencies

There are no known reverse dependencies.

## Release summary

This is a maintenance release.

* Fixed regression standard errors and Wald p-values in `summary.reg.slca()`.
* Fixed confidence intervals for `reg.slca` objects.
* Hardened `gof()` list calls and bootstrap diagnostics.
* Corrected bias-adjusted regression internals for BCH and ML three-step methods.
* Added input validation and tests for the corrected behavior.
