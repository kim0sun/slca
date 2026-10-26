# Fixed-parameter NLSY regeneration

## Policy

For a release, regenerate `nlsy_jlcpa` and `nlsy_jlta` using the release's
installed code and the pinned CRAN 1.4.0 data files from commit
`072d02b119e00019af4ff94c58652e063f495d58`. Do not use the previous regenerated
objects as the next baseline. Do not call EM, nlm or random initialization.

Keep parameters, data, category labels, class ordering, constraints and the
original convergence history unchanged. Rebuild preprocessing, model arguments,
skeleton, scores, posterior probabilities and likelihood at fixed parameters.
Fill only absent control defaults. Preserve an available Hessian; represent an
unavailable Hessian by an all-NA matrix, not a newly estimated matrix.

## Procedure

Run from the repository root. Use a temporary directory for generated evidence
and a separately installed current candidate library.

```sh
work=$(mktemp -d /tmp/slca-nlsy.XXXXXX)
mkdir "$work/baseline"
git archive 072d02b119e00019af4ff94c58652e063f495d58 \
  data/nlsy_jlcpa.rda data/nlsy_jlta.rda | tar -x -C "$work/baseline"
Rscript --vanilla maintenance/check-fixed-parameters.R \
  /absolute/path/to/candidate-library "$work/baseline/data" "$work/preview"
```

The script checks baseline Git hashes, reconstructs both objects, and compares
parameters, logits, scores, posteriors, covariance, SEs, likelihood, AIC, BIC,
saturated likelihood, assignments, convergence history and printed output.
Any nonzero numerical difference or changed output stops replacement. Inspect
`comparison.csv`, `warnings.txt`, before/after text and provenance. This is a
comparison of old and new objects under the same current methods, not an
assertion that all historical package versions produce identical output.

After review, repeat the command with `--update`. Both objects must pass before
either bundled file is copied. The saved objects are round-trip checked.
Then build and check a fresh source archive, run the full tests, and compare
`check-example-output.R` and `check-response-encoding.R` snapshots against the
pre-replacement candidate. Archive replacement, commit, push and submission
are separate steps; the script does not perform them.

If a necessary bug fix changes numerical results, stop and document the
scientific/output impact before revising the baseline or acceptance rule.
Do not relax the exact gate merely to complete a release.

## 2026-10-25 Validation

Both objects passed the fixed-parameter comparison with zero differences in
all numeric checks and identical summary and parameter/SE text. Original
`EM = TRUE, nlm = NA` history is retained. Controls gain `hessian = FALSE`;
the unavailable Hessian has the current all-NA representation. Saturated-table
status is populated (`TRUE`, zero iterations for these complete-data objects).

Warnings are retained, not suppressed in package methods: information ranks
73/117 and 70/110, and explicit Hessian requests fall back to score covariance.
These warnings also occur for the original objects under current methods.
No model is re-estimated and no new zero constraints are imposed.

Evidence for this run: `/tmp/slca-regenerate-nlsy.UZ1OBh/`.
The full manuscript bootstrap analysis is outside this regeneration check.

Final verification:

- All 399 assertions pass under ordinary R and package-level ASan/UBSan,
  with zero failures, test warnings or skips. No sanitizer diagnostics.
- All 11 executable example output blocks and 19 regular comparison cases
  are exactly identical before/after replacement, including warnings.
  Sanitized example outputs also exactly match ordinary execution.
- `R CMD check --run-donttest`, including PDF manual: 0 errors, 1 existing
  local R-header/compiler warning (`-Wfixed-enum-extension`), 0 notes.
- Repeating regeneration with the final installed candidate produces
  byte-identical `.rda` files. An attempted update using the regenerated files
  instead of the pinned baseline is rejected without modifying bundled data.
- Private manuscript PDF, TeX and analysis script, and the original 1.4.1
  submission archive retain their prior SHA256 hashes.

Final 1.4.2 source archive SHA256:
`a4b96c4806ca26642c455937c8858ed05d4a95fd5959b74f04d22ebf65ef5a7f`.
The previous archive is retained as `before.tar.gz` in the evidence directory.
R itself is not sanitizer-instrumented; leak detection is disabled. The test
runner separately warns that testthat was built under R 4.5.2 while this R is
4.5.0. Other operating systems and CRAN infrastructure were not checked here.
No commit, push or submission. Next release step: review this candidate and
run target-platform checks before an independently authorized publication.
