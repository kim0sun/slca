# CRAN candidate preparation — 2026-10-17

[Maintenance workflow](README.md) · [Previous baseline](2026-10-16-baseline.md)

## Candidate scope

Public base: `40d7d79c9558912f565d9e86b0e6b7a701ad0774`.
Package-source candidate: `54d3cabb747c268ea1d61197233f6474c04051ba`.
Branch: `prep/cran-1.4.1-20261017` (local preparation only).
The subsequent preparation-record commit changes only build-excluded maintenance
records and `cran-comments.md`; it does not change the tested package contents.

The [CRAN DESCRIPTION](https://cran.r-project.org/web/packages/slca/DESCRIPTION)
and source package index both reported 1.4.0 at review time. Keep the existing
1.4.1 version as the proposed next patch release; no additional bump is needed.
This is a candidate, not a CRAN submission, tag, release, or public deployment.

[NEWS](../NEWS.md) lists the corrections. The candidate includes fixes for
missing-item scores, saturated response tables, bootstrap failures and missing
patterns, class-reordering covariance, confidence intervals, constrained starts,
simulation likelihoods, and cyclic model validation. Runtime Imports are unchanged.
The testthat floor is raised to 3.1.7 because the tests use
`local_mocked_bindings()`, introduced in that version. Documentation was regenerated
with roxygen2 8.1.0; DESCRIPTION records that generator version.

## Reproduction and regression evidence

- [Issue #1](https://github.com/kim0sun/slca/issues/1): the existing input check
  correctly rejects unequal constrained level vectors. The new
  [release regressions](../tests/testthat/test-release-regressions.R) test missing
  observed categories, explicit shared levels, and unequal labels/order with the
  same category counts. The [estimate help](../man/estimate.Rd) now explains the
  categorical-only model and explicit factor-level requirements. No support for
  continuous indicators is claimed.
- [Audit regressions](../tests/testthat/test-audit-regressions.R): two selected
  tests failed in five assertions on the public base (missing-item likelihood
  derivatives/covariance and incomplete large saturated tables). They pass on
  the candidate. Reproductions use synthetic data; the existing `addhealth` test
  uses the package's already-distributed dataset.
- Hybrid bootstrap convergence: the added test failed on the imported correction
  commit `973929feaa2dfa6fd1e0fb8fa01c06d7b8b9c912` because EM success incorrectly
  accepted a failed final `nlm` stage. The candidate requires the returned `nlm`
  fit to converge and passes the regression.
- The full local suite contains 41 test cases (175 passing assertions) and passed after all source changes.
  The development session emitted a testthat build-version warning (built under
  R 4.5.2, running R 4.5.0).

The first complete check of the corrected package exposed many nonconverged
bootstrap refits in the old examples. The shared diagnostic example now uses a
fixed seed, five explicitly illustrative replicates, larger iteration limits,
EM refits for relative comparison, and prints `bootFail`. Five replicates are
not sufficient for inference. Failed fits are reported, not silently accepted.

## Build and check evidence

Environment: macOS 26.5.2 arm64, R 4.5.0, Apple clang 21, MacOSX26.5 SDK.

```sh
R CMD build /path/to/candidate
R CMD check --as-cran --run-donttest slca_1.4.1.tar.gz
```

Build passed. Archive SHA-256:
`fe7898c95b5cb7eaef9c3561dd2beb57f854aea48a540e5a3d2f40eec33a950b`.
Archive inspection checked 77 tracked package files against the source candidate;
no compiled binaries are included. New regression files are present. Existing
scoped exclusions omit `maintenance/`, `paper/`, CI metadata, and
`cran-comments.md`. No build-rule change was needed.

Final check: **0 errors, 1 warning, 2 notes**. CRAN incoming feasibility,
dependency/code/documentation checks, all examples (including `donttest`),
175 test assertions (0 failures/warnings/skips), and the PDF manual passed.
Both diagnostic bootstrap examples reported zero failed replicates. The check
completed with exit code 0; that does not mean warning-free submission readiness.

- Warning: the R framework header `R_ext/Boolean.h` triggers Apple's clang
  unknown-warning-option diagnostic for `-Wfixed-enum-extension`.
- Note: the extended `nlsy97` example takes approximately 20 seconds under
  `--run-donttest`; its larger models are already in `donttest` blocks.
- Note: HTML validation was skipped because HTML Tidy is too old, and math
  rendering was skipped because V8 is unavailable.

The final check took about 34 seconds for all examples, compared with about
104 seconds before the diagnostic examples were revised. See
[cran-comments.md](../cran-comments.md) for the submission-draft explanation.

A fresh CRAN source index query found no reverse dependencies in Depends, Imports,
LinkingTo, Suggests, or Enhances. No downstream package checks were therefore run.

## Submission gate

The candidate is prepared locally, not approved for submission. Before submission:

1. Confirm 1.4.1 is still unused on CRAN and approve the candidate's NEWS/scope.
2. Run the existing [CI matrix](../.github/workflows/R-CMD-check.yaml) on the exact
   candidate, including Windows, Linux, current R release, devel, and oldrel-1.
   Those runs were not triggered by this local-only task; old successful baseline
   runs do not validate this candidate. Minimum supported dependency versions
   were not independently installed/tested.
3. Resolve or explain the compiler warning and HTML validation limitation in
   [cran-comments.md](../cran-comments.md), and refresh results after any changes.
4. Obtain explicit authorization for actual CRAN submission. No submission,
   remote push, release publication, or tagging has occurred in this preparation.
