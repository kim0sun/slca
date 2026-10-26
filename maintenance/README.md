# Maintenance

This is the entry point for maintaining the public `slca` R package. Start with
[the integration validation](2026-10-21-integration-validation.md) for evidence, status, and next
steps. This directory is repository documentation, excluded from source packages
by the scoped `^maintenance$` rule in [.Rbuildignore](../.Rbuildignore).

## Fixed-parameter NLSY regeneration (2026-10-25)

At the maintainer's request, the two bundled objects are now regenerated from
the pinned original 1.4.0 parameters using current code, without optimization.
This supersedes the byte-for-byte file-retention policy in the historical
records below. All fixed-parameter numeric checks are exactly unchanged, as
are summary and parameter/SE output. Original convergence history is retained.
See [the reproducible workflow and validation](nlsy-regeneration.md).
All 399 assertions pass in ordinary R and ASan/UBSan. All 11 example blocks
and 19 comparison cases, including warnings, are unchanged. Package check:
0 errors, 1 existing local compiler/header warning, 0 notes. The rebuilt
archive SHA256 is
`a4b96c4806ca26642c455937c8858ed05d4a95fd5959b74f04d22ebf65ef5a7f`.
No commit, push or submission; target-platform checking remains the next
release step. Manuscript sources and outputs are untouched.

## Version 1.4.2 candidate (2026-10-24)

Latest authorized fixes address the three failures from the recheck below,
using small changes only in `R/estimate.R`, `R/diagnostic.R` and
`src/downward.cpp`. Historical control objects inherit missing defaults;
comparison ignores only the added saturated-status metadata; zero subtree
likelihoods contribute zero posterior mass without undefined subtraction.
Original NLSY files, their parameters and private manuscript files are untouched.

All 385 assertions pass in ordinary R and under package-level ASan/UBSan.
The new tests fail on the previous candidate and pass after the fixes. They
include exhaustive enumeration of 32 latent configurations in a branching,
three-level tree with zeros and missing responses. All 11 example output
blocks and 19 regular comparison snapshots, including warnings, remain exactly
identical. The previously failing 41-zero-constraint NLSY experiment now
converges, but its numerical-rank warning remains; no such constraints were
applied to bundled data. Ordinary check: 0 errors, 1 existing compiler/header
warning, 0 notes. Details: [audit](1.4.2-audit.md).
Root archive SHA256:
`06904c8ed8a7ba350574cc3171a3f753f49a2710862559e0838529ce4d1894fc`.
Evidence: `/tmp/slca-minimal-fixes.WjLHQP/`; `before.tar.gz` preserves the prior
candidate. No commit, push or submission.

The following records precede these three fixes.

Latest recheck of the unchanged `355ca3d...` archive: 345 assertions pass in
ordinary R and in a fresh ASan/UBSan build with the original NLSY objects.
All 11 executable examples and 19 comparison snapshots retain identical
output/warnings. Fresh ordinary check: 0 errors, 1 existing compiler/header
warning, 0 notes. Evidence: `/tmp/slca-final-recheck.GZ0hYW/`.

Targeted checks still reproduce two compatibility errors: explicitly reusing
the historical `fit$control` fails because `hessian` is absent, and comparing
original and newly processed model frames rejects differing saturated-status
metadata despite identical data. The known boundary-zero NaN defect also
remains. Default continued estimation and native evaluation of the original
NLSY parameters succeed. See [the detailed audit](1.4.2-audit.md); a passing
suite is not a claim that these separate probes pass. This recheck changes
only maintenance records, not runtime code, data or the archive.

Maintenance scope: use CRAN 1.4.0 as the reference and apply narrowly scoped
fixes for reproduced defects. Do not redesign object structures, replace
stored fits, impose new constraints or alter scientific outputs merely for
internal consistency. Any necessary numerical-output changes must be reported
explicitly. Keep unrelated corrections separate and do not blanket-revert
verified bug fixes when restoring an original representation.

The original CRAN 1.4.0 NLSY files have now been restored byte-for-byte from
commit `072d02b`, superseding the cache-only reconstruction below. No new
saturated-convergence metadata was added to these stored objects. Runtime
bug fixes are unchanged. Verification of this data-only restoration is recorded
in [the audit](1.4.2-audit.md).

With these original objects, all 345 assertions pass; all 11 example output
blocks and 19 comparison snapshots (including warnings) are exactly identical
to the preceding candidate. Ordinary package check including donttest and the
PDF manual: 0 errors, 1 existing compiler/header warning, 0 notes. This
data-only revision was not separately rebuilt under sanitizers.
Current root archive SHA256:
`355ca3ddb37d6c02d0a067a0491fac7fd6e36b989c4a2f863d210a34c0010f78`.
Evidence: `/tmp/slca-original-fits.7CmOXZ/`. No commit, push or submission.

The following records describe the immediately preceding encoding restoration.

Latest update: restored the 1.4.0 internal convention, `0` for missing and
`1,...,K` for observed categories, retaining the missing-score fixes and native
range checks. Public inputs still use `NA` for missingness. Stored example
objects were recached without refitting; continuing estimation and reordering
rebuild caches from the unchanged model frame. See
[the current audit assessment](1.4.2-audit.md) and
[the encoding note](../inst/notes/internal-encoding.md).

All 345 assertions pass in ordinary R and under package-level ASan/UBSan.
All 11 executable manual examples pass under both builds, with output and
warnings identical to the previous candidate. The 19-case before/after
comparison in [check-response-encoding.R](check-response-encoding.R) is exactly
identical, including preexisting warnings. An exploratory tree-model absolute
bootstrap fails to converge in all three replicates in both versions; this
comparison is not evidence that every fitted model or bootstrap converges.
The mixed-category absolute and relative bootstrap cases complete without
failed replicates. The ordinary check, including donttest and the PDF manual,
has 0 errors, 1 existing local compiler/header warning and 0 notes.

Root archive SHA256:
`a4a3be215d7832b6377180e064b00d8a76e46099bc11ef713c38e7258df4e8d8`.
Evidence and previous archive: `/tmp/slca-restore-encoding.XU1PxA/`.
Manuscript files and the original 1.4.1 submission archive are unchanged.
The separate boundary-zero NaN defect and numerical-rank warnings remain;
no new zero constraints were imposed. No commit, push or submission was made.

The following candidate records precede the encoding restoration.

Latest scoped corrections: retained zero constraints during continued
estimation, validated control names, added numerical-rank warnings without
changing covariance calculations, and skipped zero-LR bootstrap histograms.
See the updated [audit status](1.4.2-audit.md). All 333 assertions pass; all
11 executable Rd example output blocks match the preceding candidate exactly.
The NLSY JLCPA confidence-interval example adds a numerical-rank warning
(73 of 117); numerical results are unchanged. A fresh ordinary check including
donttest and the PDF manual has 0 errors, 1 local compiler/header warning and
0 notes. Logs: `/tmp/slca-targeted-fixes.DOD63v/final/`.
The root `slca_1.4.2.tar.gz` now has SHA256
`6b0b2825d8d008545d0ebc63fa94046771b55c78f8bd51331886827e2584896a`.
The preceding root archive is preserved under `/tmp/slca-1.4.2.GYjSwc/`.
Offsets, changed-category refits, covariate-scale failures and named-matrix
validation remain discussion items; legacy compatibility was explicitly left
out of scope. There was no commit, push or CRAN submission.

The following expanded audit and initial build records precede these corrections.

Latest expanded audit: [1.4.2-audit.md](1.4.2-audit.md). The exact candidate
passes all examples and 300 assertions under package-level ASan/UBSan, but
nine reproduced edge-case defect categories remain. Independent likelihood,
posterior, score and six three-step objective comparisons pass on the tested
regular examples. No runtime code or archive was changed during that audit.
Review its findings before treating this candidate as fully cleared.

At the maintainer's request, DESCRIPTION and NEWS now identify version 1.4.2.
The reconstructed NLSY objects and runtime code are unchanged from the verified
1.4.1 working-tree build below. The new source archive is
`slca_1.4.2.tar.gz` in the repository root (ignored by Git), with SHA256
`7bb855445ae71bda2121fc3018103f761d1298a7bc5d2ae8cf6e491ef0ff4b29`.
`R CMD check --as-cran --run-donttest` on this archive completes with 0 errors,
1 warning (local R-header/compiler compatibility) and 3 notes (one day since
the last CRAN update, slow gof example, and unavailable V8). All 300 assertions
pass with no test warnings or skips; examples and the PDF manual pass. Freshly
installed package version and both bundled objects were verified. Detailed logs
are at `/tmp/slca-1.4.2.GYjSwc/`. The original 1.4.1 submission archive remains
unchanged. No commit, push or CRAN submission was performed. The previously
recorded mixed legacy/current-object comparison issue is still pending;
target-platform checks remain a separate release step.

Latest update (2026-10-24): the CRAN sanitizer failure has been reproduced and
corrected locally, and the two bundled NLSY fits now use current response caches.
All examples and 300 assertions pass with ASan/UBSan enabled. The maintainer has
authorized committing and pushing these development changes on
`codex/bootstrap-iteration-limit`; CRAN resubmission remains a separate step.
The pushed commit `b1f5ca4` has since been rechecked from a clean Git archive.
One additional legacy-cache comparison issue was reproduced; see below.

Fixed-parameter reconstruction (2026-10-24):
[`check-fixed-parameters.R`](check-fixed-parameters.R) rebuilt the two NLSY fits
from the `a11c1b0` saved objects using the installed `b1f5ca4` code. Original
category labels were restored before current preprocessing; parameters were
held exactly fixed without optimization. Scores, posteriors and likelihoods
were recalculated. Maximum absolute differences were zero for parameters,
posteriors, scores, default score-based SEs, log likelihood, AIC, BIC and
saturated log likelihood; G-squared and class assignments were also unchanged.
Summary and parameter-with-SE text matched exactly when both objects were
printed with current methods. This is not a comparison against old-version
printing code. The Hessian and convergence history were retained, not refitted.
Raw-data predictions and saved-object round trips were checked. Results and
separate `.rds` objects are in `/tmp/slca-fixed-parameter-check`; bundled data and
manuscript files are unchanged. The mixed legacy/current cache comparison issue
remains a separate pending runtime fix.

Bundled reconstruction build (2026-10-24): at the maintainer's request, the two
reconstructed `.rds` objects replaced `data/nlsy_jlcpa.rda` and
`data/nlsy_jlta.rda`, retaining bzip2 compression and serialization version 2.
Compared with the previously cache-updated objects, the model frames now also
contain `saturated.converged = TRUE` and `saturated.iterations = 0`; shared
attributes and all non-model-frame fields remain equivalent. No refit occurred.
The source archive at `/tmp/slca-rebuilt.dshohu/slca_1.4.1.tar.gz` has SHA256
`44855572327fa92755cfc3b7728adf58081e13d76b61ac6c2ae18e543810e960`.
Fresh installation from that archive reproduces the reconstructed objects.
`R CMD check --as-cran --run-donttest` completes with 0 errors, 2 warnings
(already-published version and local compiler/R-header compatibility) and 2
notes (slow examples and unavailable V8). All 300 assertions pass without test
warnings or skips, examples pass, and the PDF manual builds. The source archive
excludes maintenance and private manuscript files. The root submission archive
and the manuscript PDF, TeX and R script retain their previously recorded hashes.
This working-tree build is not a commit, push or CRAN resubmission. Before
resubmission, resolve the pending compatibility issue, select a new release
version and run the target-platform checks on the final candidate.

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

## Pushed-commit recheck (2026-10-24)

Checked `b1f5ca449684249c96ff19e285521459fc4ad0c6`, extracted with `git archive`
into `/tmp/slca-commit-recheck.4Fln3R/source`, without the working tree's local
ignore-file changes or deleted submission notes. The remote development branch
points to the same SHA; master remains at `a11c1b0`.

- Fresh `R CMD check --as-cran --run-donttest`: 0 errors, 2 warnings (version
  1.4.1 already published; local R-header/compiler compatibility), 2 notes
  (examples above five seconds; missing V8 math-rendering check).
- All 300 test assertions pass without test warnings. A separate ASan/UBSan
  install also passes all manual examples including donttest and 300 assertions.
- The old bundled objects from `a11c1b0` and updated ones preserve estimates,
  posterior predictions and displayed output when evaluated with the corrected
  runtime. No manuscript file or package implementation was edited in this audit.
- GitHub reports zero Actions runs for this SHA. Current workflows run on
  main/master pushes or pull requests, not standalone development-branch pushes.
  Cross-platform success from the earlier master commit does not validate this SHA.

Additional finding: `compare()` removes the cached `y` attribute before testing
model-frame equality, but retains `yu`. Comparing an old saved fit with its
reordered version, or with an updated bundled fit, incorrectly raises
"datasets used for models are different" because only the encoding of `yu`
differs. Both NLSY models reproduce this. Aligning only `yu` allows comparison
and gives G-squared 0 for otherwise identical models. This is a false rejection,
not changed likelihood estimation or a memory error. Current-to-current fits
are unaffected by this specific mismatch.

Minimal reproduction with an updated bundled object:

```r
legacy <- nlsy_jlcpa
attr(legacy$mf, "y") <- attr(legacy$mf, "y") + 1
attr(legacy$mf, "yu") <- attr(legacy$mf, "yu") + 1
changed <- reorder(legacy, SMK_98 = c(1, 3, 2))
compare(legacy, changed) # incorrectly rejects identical observed data
```

Next action: exclude or normalize `yu` when testing dataset equality, retaining
checks on actual responses and category levels, and add a regression test.
Then use a PR to run the configured platform matrix before merging or release.
This finding is documented only; no additional runtime fix, commit or push
was performed during the recheck. Earlier documented edge-case issues and the
five-class bootstrap nonconvergence remain separate outstanding work.

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

## CRAN 1.4.1 sanitizer report and 1.4.2 verification (2026-10-26)

CRAN flagged 1.4.1 under clang-ASAN, gcc-ASAN and M1-SAN (valgrind: OK):
`heap-buffer-overflow` in `upInit()` at `src/upward.cpp:14`, raised by the
`reorder.slcafit` example on `nlsy_jlcpa`. Cause: 1.4.1 switched the native
response encoding to zero-based/`-1`, but the bundled 1.4.0 fits still carried
one-based/`0` caches, so a maximal category code read one element past the
`rho` block. The 1.4.2 working tree (restored 1.4.0 convention, cache rebuild
in `reorder()`/`estimate()`, range check in `upInit()`, regenerated NLSY
objects) resolves it.

Verification in a Linux container (Ubuntu 24.04, R 4.3.3): the 1.4.1 failure
reproduces under clang 18 ASan/UBSan; the 1.4.2 tarball passes
`R CMD check --run-donttest` under clang 18 ASan/UBSan with zero sanitizer
diagnostics in examples and tests, and the ordinary `--as-cran` check shows
0 errors and only environment-specific notes (locale, proxy-blocked URL
check, missing `inconsolata.sty`, Ubuntu `-mno-omit-leaf-frame-pointer`).
`gof`/`compare` example timings (~8 s) are identical to 1.4.1 on the same
slow machine. DiagrammeR was a local stub (CRAN unreachable); plot output was
not exercised. `cran-comments.md` drafted for 1.4.2. Tarball SHA256:
`2769fe51653df307f548d179eee8c5360dc4a763fcc7b978430f6a8bed20fc89`.
