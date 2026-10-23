# Maintenance

This is the entry point for maintaining the public `slca` R package. Start with
[the integration validation](2026-10-21-integration-validation.md) for evidence, status, and next
steps. This directory is repository documentation, excluded from source packages
by the scoped `^maintenance$` rule in [.Rbuildignore](../.Rbuildignore).

Latest update (2026-10-24): the CRAN sanitizer failure has been reproduced and
corrected locally, and the two bundled NLSY fits now use current response caches.
All examples and 300 assertions pass with ASan/UBSan enabled. The maintainer has
authorized committing and pushing these development changes on
`codex/bootstrap-iteration-limit`; CRAN resubmission remains a separate step.

Current status (2026-10-23): local `master` includes the reviewed maintenance
candidate through `5db21ad`. Fresh tests pass (264 assertions), and the clean
source check has 0 errors, 1 toolchain warning, and 1 HTML-tooling note.
Proportional assignment is unchanged. See the validation record for output
compatibility changes. Master through `a11c1b0` has since been pushed to GitHub;
the maintainer subsequently reported submitting to CRAN. CRAN's live DESCRIPTION
now reports 1.4.1, published 2026-10-23 06:50:02 UTC. All five R-CMD-check jobs
and pkgdown succeeded on the published commit. HTML Tidy 5.8.0 is accepted by R;
the remaining HTML note concerns unavailable V8 for math rendering.

Post-submission development: `codex/bootstrap-iteration-limit` increases only
the `gof()` absolute-bootstrap default from 100 to 5000 iterations. Explicit
limits, `compare()` defaults, and proportional assignment are unchanged. All
271 assertions in 65 test blocks pass. A fresh source check has 0 errors,
2 warnings (already-published version and local R-header/compiler compatibility),
and 3 notes (time verification unavailable, a slow example, and unavailable V8).
The original submission archive remains unchanged. These development changes
are included in the authorized GitHub update, not a new CRAN submission.
An additional four-model check with 100 bootstrap replicates per model reduced
nonconvergence from 289/400 to 15/400. All remaining failures occurred in the
five-class model, so the larger default is not a universal convergence remedy.
The original fitted parameters and likelihoods were exactly unchanged.

The post-submission synthetic audit also reproduces four pre-existing issues:
ignored regression offsets, unknown control names affecting valid values,
regression overflow after rescaling covariates, and missing rank-deficiency
diagnostics. These remain uncorrected pending separately scoped changes and
regression tests. Private manuscript comparisons are retained outside public
package documentation.

Local cleanup (2026-10-23): obsolete check output, generated pkgdown output,
and compiled objects were moved to the system Trash, not permanently deleted.
The root source archive was replaced by the clean, tested archive recorded in
the validation report. Source, tests, documentation inputs, and private paper
files were retained in place. At the maintainer's further request, the remaining
local review bundle was also moved to Trash; detailed logs and snapshots are
recoverable there, while the core validation results remain in this directory.
No top-level `.Rcheck` directory remains. Local review directories, the private
paper directory, and source archives are now ignored by Git. Regenerate website
output from its sources when needed; do not hand-edit generated HTML.

## CRAN sanitizer correction (2026-10-24)

The [CRAN check page](https://cran.r-project.org/web/checks/check_results_slca.html)
lists additional issues even though its regular platform checks are OK. The
1.4.1 clang-ASAN, gcc-ASAN and M1-SAN logs report a heap-buffer-overflow in
`upInit()` during the bundled `reorder(nlsy_jlcpa, ...)` example. The Valgrind
example log also reports an invalid score-vector write at `calculate.cpp:188`;
its overall check status of OK does not mean the memory diagnostics are clean.

Root cause: the bundled NLSY fits retain 1-based cached `y` and `yu`, whereas
the 1.4.1 native routines expect 0-based categories and -1 for missingness.
`reorder()` reused the incompatible `y`; `summary()` similarly reused `yu`.
The correction derives those native inputs from the unchanged, 1-based model
frame through one shared encoder. Native `upInit()` now rejects invalid codes
before indexing. The initial correction did not rewrite bundled data files;
the subsequent cache-only update is recorded below. Fitted parameters and
original manuscript files are not rewritten. Legacy cached-fit recalculations can change: the old
posterior, score/derived SE, and Pearson statistic calculations were unsafe.

Validation uses Apple clang 21 and macOS arm64 R 4.5.0. A separately installed
pre-fix build with `-fsanitize=address,undefined` reproduces the same overflow at
`upward.cpp:14`. The corrected build passes all manual examples, including
`donttest`, and all 294 assertions under those sanitizers. The regular suite
also passes 294 assertions in
68 blocks, including 23 new assertions for both NLSY fits, inverse reordering,
predictions from raw data, legacy missing values, summaries and invalid codes.
The fresh source check has 0 errors, 2 warnings (already-published version and
local R-header/compiler compatibility) and 2 notes (slow examples and missing
V8). Linux GCC sanitizer and Valgrind reruns still require their own environments;
local success does not update CRAN's external status.

For a fixed-seed missing-data model, parameter, likelihood, score, Hessian,
posterior, prediction and regression outputs match the pre-fix build exactly.
Printed summary, fit, parameter and regression output also match. Full objects
are equal apart from formula-environment identity across separate R processes.
This checks the encoding correction, not the previously authorized bootstrap
default change or a new reproduction of every manuscript analysis.

Reproduction: compile and install into a temporary library with C++ and linker
flags `-fsanitize=address,undefined -fno-omit-frame-pointer`, then run
[check-sanitizers.R](check-sanitizers.R) with library and repository paths as its
two arguments. Preload the compiler's ASan runtime on macOS; use
`ASAN_OPTIONS=detect_leaks=0` and `UBSAN_OPTIONS=halt_on_error=1`. This instruments
the package, not the entire R installation, and does not test leaks.
Logs from this run are under `/tmp/slca-cran-sanitizer.dIz2dS/`; the automated
regressions are in `tests/testthat/test-saved-fit-encoding.R`.
Before CRAN resubmission, choose a new package version, review the separate
bootstrap-default change, and rerun
the platform checks on the exact release candidate.

## Bundled response-cache update (2026-10-24)

At the maintainer's request, `data/nlsy_jlcpa.rda` and `data/nlsy_jlta.rda`
now store current 0-based `y` and `yu` caches. Only these two attributes are
transformed; no model is re-estimated. The original 1-based model-frame values,
category labels, fitted parameters, scores, posteriors and log-likelihoods are
preserved. Both files retain bzip2 compression and serialization version 2.
[update-example-cache.R](update-example-cache.R) performs the transformation,
verifies preservation before replacing each file, and is idempotent.

Comparison against the pre-update files confirms unchanged `print`, `param`,
`summary`, `gof`, reordered parameter output and posterior predictions when
using the corrected runtime. Direct native calculation from each new cache
also reproduces the saved per-observation log-likelihoods. Formula environments
are equivalent after serialization, although their in-memory identities differ.
The compatibility tests explicitly reconstruct legacy caches, so replacing the
bundled data does not remove coverage for old user-saved objects.
Backups and verification logs are under `/tmp/slca-example-cache.uPXoXk/`.
The original submission archive and manuscript files remain unchanged.
The updated archive passes `R CMD check --run-donttest` with 0 errors,
1 existing local R-header/compiler warning and 0 notes. All 300 assertions
pass with no test warnings, both normally and under ASan/UBSan. Manual examples
including `donttest` also complete under the sanitizers. The final test fixes
an initially incorrect PROF reorder in the JLTA compatibility test by using
SMK_98, which exists in both stored models. No runtime changes were needed.

## Sources of truth

- [Public issues](https://github.com/kim0sun/slca/issues): reports and discussion.
- [DESCRIPTION](../DESCRIPTION): package version, dependencies, and metadata.
- [NEWS](../NEWS.md): user-facing changes; do not duplicate the changelog here.
- [Regression tests](../tests/testthat/test-slca.R) and
  [test runner](../tests/testthat.R): executable correctness evidence.
- [R CMD check workflow](../.github/workflows/R-CMD-check.yaml) and
  [runs](https://github.com/kim0sun/slca/actions/workflows/R-CMD-check.yaml): macOS
  and Windows R release, Ubuntu R release, devel, and oldrel-1.
- [pkgdown workflow](../.github/workflows/pkgdown.yaml): documentation build and
  deployment; its success is separate from package correctness.
- [CRAN DESCRIPTION](https://cran.r-project.org/web/packages/slca/DESCRIPTION):
  published package metadata; local check logs do not prove acceptance.

## From report to release candidate

1. **Report / triage.** Link a public issue and record the affected package
   version and source SHA. Classify it as a defect, usage question, unsupported
   feature, or documentation gap. Record expected and observed behavior. Do not
   copy personal correspondence, private research, identifying data, or unpublished
   datasets into this public repository; use synthetic or already-public examples.
2. **Minimal reproduction.** Reduce to the smallest model and synthetic dataset.
   Fix random seeds where needed; record R/platform/dependency versions and the
   exact command and error. Distinguish reproduction of an input check from
   successful estimation, convergence, and statistically valid inference.
3. **Cause / correction.** Link the responsible source and public fix commit or
   PR. Label hypotheses as hypotheses. Do not mark a report fixed merely because
   it is closed or a workaround exists. Use a branch from current public master;
   never bundle unrelated local changes.
4. **Regression evidence.** Put durable assertions in the existing test suite,
   with a descriptive test name and a reference from the work record. For a
   behavioral fix, show failure before and success after. Run the relevant tests,
   then the full suite and package check. Record failures and environment limits.
5. **Release candidate.** Link the candidate SHA, relevant NEWS entry, regression
   test names, and CI/check results for that exact SHA. Review source archive
   contents and dependency/metadata changes. Resolve or explicitly account for
   every check warning/note and outstanding high-priority defect. A version field,
   historical successful run, or a closed issue alone does not make a candidate
   ready. Version bumps, CRAN submission, tags, and release publication are separate
   maintainer decisions.

## Routine commands

Run from a clean, disposable checkout with package dependencies and development
helpers (`pkgload`, `testthat`) available. Keep build/check output outside the
checkout. The existing CI workflow remains the platform-matrix definition.

```r
pkgload::load_all(".")
testthat::test_local(".", reporter = "summary")
```

```sh
# From a separate build directory; replace the path and archive version.
R CMD build /path/to/slca --no-manual
R CMD check --no-manual slca_<version>.tar.gz
```

Before committing documentation, run `git diff --check` and inspect the exact
file list. Inspect the built archive: `maintenance/` must be absent while `R/`,
`src/`, `man/`, `tests/`, `DESCRIPTION`, and `NEWS.md` remain. A documentation-only
change does not require duplicating tests or changing package source/version.

## Work-record convention

Add a short dated record only when there is new evidence worth preserving. Link
back here and include: public source, baseline/candidate SHA, classification,
reproduction, cause/fix status, regression evidence, check results, release gate,
and the next concrete action. Update status when evidence changes; keep historical
results labeled with their SHA. Do not create a parallel issue tracker or copy CI
logs and NEWS wholesale.
