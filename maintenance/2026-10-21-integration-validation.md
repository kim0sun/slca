# Maintenance integration validation — 2026-10-21

[Maintenance workflow](README.md)

## Source and scope

The validated package source is local commit
`069284455388cb4d447b8e7ed11f28e3d475c39c`. It integrates fetched public master
`ff84ca1afab1424d8540ceb1ad39b8c079e06373` with the preserved review candidate
`0d63dd5ca56977d4fdcaf7f1f288e252c1a6b321`, retaining the latter's correction
history. The integration commit is
`8d6188c8616f96f805778548bedd6bf7a8285101`.

The comparison baseline called "1.4.1" in this review is Git commit
`aa5ed712b7e822053fc1b7b786d37896959603ee` ("Release 1.4.1"), not a claim that
CRAN accepted that version. DESCRIPTION remains version 1.4.1. This work does
not submit, tag, publish, or push a release.

The preserved candidate includes the final-nlm bootstrap convergence fix
(`02861a39fcabe2b717930752d7c6df51e2ab732f`) and the saturated-fit and covariate
row-identity corrections (`44d4eb5849f402dbdf89d94f49eb836e019e761f`). The root
CLAUDE.md was removed. Archive inspection found that R build included a Git
worktree's .git pointer file; the scoped `^\.git$` rule now excludes it. The
initial archive/check was abandoned before installation, then a fresh archive
was built and checked. No package algorithms were reimplemented for this
integration. See NEWS.md and the prior review records for the maintenance
changes; internal zero-based/-1 encoding predates the Git 1.4.1 baseline.

## Fresh validation

Environment: macOS 26.5.2 arm64, R 4.5.0, Apple clang 21.0.0, MacOSX26.5 SDK,
Rcpp 1.1.2, testthat 3.3.2. User Makevars, R profile, and R environment files
were disabled for build/install/check (`R_MAKEVARS_USER`, `R_PROFILE_USER`, and
`R_ENVIRON_USER` set to `/dev/null`). Build, installation libraries, check
output, and supplemental scripts were kept outside the worktree.

- `R CMD build`: passed.
- Archive inspection: 87 files; all 86 files other than build-normalized
  DESCRIPTION are byte-identical to the validated source. Required package
  directories and tests are present. Git metadata, CLAUDE.md, maintenance
  records, cran-comments.md, research directories, and compiled/check output
  are absent. Archive SHA-256:
  `adc32b04387df3f85ef78293b47acced6ebd27b07c034ee1e32768294a16ab27`.
- `R CMD check --as-cran --run-donttest`: **0 errors, 1 warning, 1 note**.
  CRAN incoming feasibility, dependencies, compiled-code checks, examples,
  code/documentation consistency, tests, and PDF manual passed. Examples took
  about 14 seconds and had no timing note.
- Check test suite: **201 passes, 0 failures, 0 warnings, 0 skips**. A separate
  installation with `R CMD INSTALL --install-tests` also passed all 201
  assertions across 45 test blocks using the installed namespace.
- Supplemental installed-package verification: **26 checks across six probes
  passed**. They cover public bootstrap propagation of final-nlm failure,
  selection among multiple starts, saturated-solver iteration boundaries and
  likelihood consistency, suppression of finite unfinished saturated results,
  row-ID alignment with shuffled/extra rows and covariate NA, and explicit
  covariate data after case exclusion. Scoped mocks test failure propagation;
  these probes do not establish general optimizer convergence or inferential
  validity.

The warning is from the installed R framework's `R_ext/Boolean.h`:
`unknown warning group '-Wfixed-enum-extension'`. It persists without user
Makevars and is not attributed to a package source pragma. The HTML-manual
note reports an old HTML Tidy and missing V8, so HTML validation and math
rendering were skipped. testthat also emits a startup message that it was
built under R 4.5.2; the test results above distinguish that environment
message from test warnings. None of these limitations was suppressed.

## Remaining issue and release limits

The supplemental probes reproduce a covariate-input limitation: after one
all-manifest-missing observation is excluded from a 100-row fit, calling
`regress(fit, l ~ x)` with omitted `data` and an original-length environmental
`x` fails with `variable lengths differ`. Supplying the original row-identified
covariate data explicitly via `data=` succeeds and aligns the retained 99
observations. The omitted-data case was recorded separately as a known issue,
not counted as a successful regression assertion. It remains unresolved in
this candidate and should be triaged before describing the maintenance work
as covering every regression input path.

Windows, Linux, other R versions, minimum dependencies, and a new platform
matrix have not been checked for this source. The successful historical or
baseline CI checks do not substitute for those runs. This is a locally
validated integration candidate, not an assertion of CRAN readiness.

This record and its maintenance index entry are excluded from source archives;
the follow-up documentation commit does not change the tested package files.
