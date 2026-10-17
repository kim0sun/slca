## Submission draft

Prepared locally for slca 1.4.1; not yet submitted. CRAN's public DESCRIPTION and
source package index reported version 1.4.0 when checked on 2026-10-17.

## Test environment

* macOS 26.5.2 arm64, R 4.5.0
* Apple clang 21, MacOSX26.5 SDK
* `R CMD build` followed by `R CMD check --as-cran --run-donttest`

## R CMD check results

0 errors | 1 warning | 1 note

The installation warning comes from the R framework header
`R_ext/Boolean.h` with Apple clang 21:

```
warning: unknown warning group '-Wfixed-enum-extension', ignored
```

The previous extended-example timing note has been resolved. The longitudinal
example now uses a reproducible 500-row subset and two-class models while
preserving the three-occasion structure and equality constraints. The text makes
clear that this is a syntax illustration, not full-data inference. Its `\donttest`
wrapper was removed so these examples run in ordinary checks. All examples passed
with no timing note. Diagnostic bootstrap examples had zero failed replicates;
their five illustrative replicates are not intended for statistical inference.

The remaining note is an environment limitation in HTML manual checks:

* The installed HTML Tidy is too old for HTML validation.
* Math rendering was skipped because V8 is unavailable.

The PDF manual, dependency checks, code/documentation consistency checks, compiled
code checks, and test suite passed. Tests reported 175 passes, no failures,
warnings, or skips. CRAN incoming feasibility passed in this local check.

## Reverse dependencies

A fresh CRAN source package index query found no reverse dependencies in Depends,
Imports, LinkingTo, Suggests, or Enhances. No reverse-dependency checks were run.

## Release summary

See NEWS.md for the full maintenance changelog. This candidate corrects missing
response scores and fit diagnostics, bootstrap handling, class reordering,
confidence intervals, and constrained starts. It adds regression coverage for
constrained categorical levels and clarifies that continuous indicators remain
unsupported. Runtime Imports are unchanged; testthat >= 3.1.7 is required by the
test helpers. Version 1.4.1 remains the proposed patch release.

## Before submission

Refresh these results on the final candidate. Windows, Linux, current R release,
R-devel, oldrel-1, and minimum dependency versions have not been checked for this
candidate. Resolve or explain the compiler warning and complete HTML validation
in a suitable environment before deciding to submit. Additional syntax-only checks parsed all 17 source files on installed R 3.5.3,
3.6.0, and 4.2.0; missing or old dependencies prevented full checks there. An
isolated install without the user Makevars also passed the test suite and removed
an additional deprecated-linker warning. No external builder upload,
CRAN submission, tag, or release has been performed.
