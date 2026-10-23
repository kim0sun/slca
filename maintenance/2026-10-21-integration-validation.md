# Maintenance integration validation — 2026-10-21

[Maintenance workflow](README.md)

## Local master integration, 2026-10-23

At the maintainer's request, local `master` was fast-forwarded from
`5531aeac953f9b598929902e147e302dedf55d73` to
`5db21ad70561a39aa549b1f98e991ba968fa4678`, retaining the existing correction
history. No numerical implementation was edited during this integration.
`prop` remains posterior-probability weighting, not random assignment.
Package version remains 1.4.1. No remote push or submission was performed.

Fresh checks used R 4.5.0 on macOS arm64, with user Makevars, R profiles, and
R environment files disabled. The complete suite passed all 264 assertions
in 64 blocks, with no failures, errors, warnings, or skips. A clean source
archive made from `git archive 5db21ad` passed
`R CMD check --as-cran --run-donttest` with 0 errors, 1 warning, and 1 note.
All examples, installed-package tests (264 passes), and the PDF manual passed.
The warning is the R header's unknown `-Wfixed-enum-extension` warning group;
the note is unavailable modern HTML Tidy/V8. There was no example-time note.
Archive SHA-256:
`a86d3936b5c806c005a985ad2061dedbccae1e9ef0596798b25df33bc6d5e30e`.
The archive excludes Git metadata, maintenance records, private research,
review logs, and compiled artifacts. An earlier concurrent worktree build
captured a compiler temporary file and was discarded as validation evidence;
the clean Git export removed that build-environment artifact.

Compatibility was checked against the actual pre-integration master, not
against CRAN. With identical data and seeds, all six modal/prop by naive/BCH/ML
regression snapshots matched exactly (coefficients, SE, covariance, summary,
and confidence-interval output). The complete-data fit snapshot also matched
exactly. A missing-data fit retained identical estimates, likelihood, AIC,
BIC, posterior, score, and parameter output; G-squared changed from
2.8420410809897021 to 2.8420410930912112 because saturated likelihood is now
evaluated at the returned probabilities. Other data can show changes in the
last printed G-squared digits, so exact output parity is not claimed.

Intentional behavior changes were reproduced:

- An unfinished 20,000-iteration saturated fit now returns NA with a warning,
  rather than the former finite log likelihood of approximately -10.3559.
- Reversed equal-length covariate data are matched by row ID. A synthetic
  example's erroneous slope -0.002364741 becomes +0.002365126, equal to the
  correctly ordered result.
- Named formula-environment covariates are aligned after case exclusion.
  Unnamed observation vectors now require explicit `align = "position"` and
  the fitted observation order, rather than silent positional matching.
- A hybrid bootstrap refit with EM TRUE and nlm FALSE is now rejected.
  Failure counts and bootstrap p-values can therefore change.
- Existing candidate documentation changes also reduce diagnostic bootstrap
  examples to five replicates and use a 500-row, two-class NLSY syntax example.
  Their output is not comparable to the previous full-data examples.

Detailed local reproduction scripts, before/after snapshots, output comparisons,
and logs are retained in the package-excluded review bundle. Private research
outputs are not copied into this public maintenance record. Cross-platform CI
on the eventual published commit remains pending; local success is not a CRAN
acceptance claim. The historical validation below remains available for context.

## Source and scope

Validated package source: `208b2f69bc6deb5657ac4a4bb2aa50ed7c152d91`.
It integrates public master `ff84ca1afab1424d8540ceb1ad39b8c079e06373`
with review candidate `0d63dd5ca56977d4fdcaf7f1f288e252c1a6b321`,
retaining its correction history. Integration commit:
`8d6188c8616f96f805778548bedd6bf7a8285101`.

The baseline called Git 1.4.1 is
`aa5ed712b7e822053fc1b7b786d37896959603ee` ("Release 1.4.1"); this is
not a claim of CRAN acceptance. DESCRIPTION remains version 1.4.1. No version
bump, release tag, release publication, or CRAN submission is included.

The candidate retains final-nlm bootstrap convergence (`02861a39`), saturated
convergence and explicit covariate row identity (`44d4eb58`), and fixes formula
environment alignment (`208b2f69`). Root CLAUDE.md is removed. A scoped
`^\.git$` rule excludes linked-worktree metadata from source archives.
Internal zero-based categories and -1 missing encoding predate Git 1.4.1.

## Formula-environment regression correction

After all-manifest-missing observations are excluded, an original-length
external covariate previously failed with a model-frame length mismatch.
Equal-length external inputs with reordered IDs could silently pair the wrong
observations. Each external vector/factor is now matched through names, and
each matrix/data frame through row names, before formula transformations.
Extra observations are allowed; duplicate, absent, or missing fitted IDs fail.
Factors retain levels/contrasts and matrices retain dimensions and column names.

Unnamed row inputs require explicit `align = "position"` and exactly the fitted
row count. There is no inferred positional fallback after case exclusion.
Formula environments are not mutated. Supplied data and fitted columns take
precedence. Dynamic lookups and hidden argument-free providers are rejected
under ID alignment; transformation functions must use explicit covariate
arguments. Scalar parameters and namespace constants are supported. Non-scalar
transformation settings must be inline or captured inside a transformation
function, since their role cannot safely be distinguished from row inputs.
These constraints and recovery options are documented in `regress()`.

The first 12 new test blocks were run against the previous installed source
`06928445`, producing 3 failed assertions and 9 errors. The final 19 blocks
contain 63 passing assertions, including equal-length reorderings, dropped
observations, factors, matrices, independent inputs, transformed formulas,
missing covariates, positional alignment, immutable saved formula environments,
explicit-data precedence, dynamic access, qualified constants, and equality of
actual fitted coefficients and covariance with explicit row-identified data.

## Fresh validation

Environment: macOS 26.5.2 arm64, R 4.5.0, Apple clang 21.0.0, MacOSX26.5 SDK,
Rcpp 1.1.2, testthat 3.3.2. User Makevars, R profiles, and R environment files
were disabled with their respective environment variables set to `/dev/null`.
Build output, check directories, and installation libraries were outside the
worktree.

- `R CMD build`: passed. Archive has 88 files; 87 files other than build-normalized
  DESCRIPTION match source bytes. No Git metadata, CLAUDE.md, maintenance records,
  cran-comments.md, private research, compiled output, or check output is included.
  Archive SHA-256:
  `f04124596cb1b0b48c003f64145b6ada6eb48d66a1717a486bc225088eef3ce9`.
- `R CMD check --as-cran --run-donttest`: **0 errors, 1 warning, 2 notes**.
  Incoming feasibility, dependencies, compiled code, examples, tests,
  documentation consistency, and PDF manual passed.
- Full check and separate installed-package suite: **264 passes across 64 test
  blocks**, with zero failures, errors, test warnings, or skips.
- Supplemental installed-package checks: **29 assertions across six probes**,
  all passed. They cover hybrid bootstrap failure propagation and start selection,
  saturated-solver boundaries and likelihood, suppression of unfinished results,
  extra/reordered covariate rows and NA, explicit covariates after case exclusion,
  actionable unnamed-input rejection, and named-environment fit equivalence.
  Scoped failure mocks do not establish general optimizer convergence or
  inferential validity.

The warning originates in the installed R framework's `R_ext/Boolean.h`:
unknown warning group `-Wfixed-enum-extension`. One note reports old HTML Tidy
and unavailable V8, skipping HTML validation and math rendering. The other
reports the compare example taking 5.181 seconds elapsed (4.736 CPU seconds),
slightly above the five-second example threshold; total examples took 16 seconds.
The earlier `06928445` check had 0 errors, 1 warning, 1 note and 201 assertions;
these are historical results, superseded by the run above. testthat's startup
message about being built under R 4.5.2 is distinct from test warnings.

## Integration and release limits

The preserved local source changes were compared file by file with the validated
candidate. Their corrections are retained; subsequent convergence and row-ID
fixes supersede weaker earlier implementations. Original source commit ancestry
must be retained through normal merges, without replaying duplicate patches.

The new platform matrix must run on the PR and final master commit. Historical
CI does not substitute for current checks. Successful checks are not a claim of
CRAN readiness or general statistical validity. This record is excluded from
source archives; documentation-only follow-up commits do not change the tested
package bytes.
