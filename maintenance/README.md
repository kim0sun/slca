# Maintenance

This is the entry point for maintaining the public `slca` R package. Start with
[the integration validation](2026-10-21-integration-validation.md) for evidence, status, and next
steps. This directory is repository documentation, excluded from source packages
by the scoped `^maintenance$` rule in [.Rbuildignore](../.Rbuildignore).

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
- [CRAN comments](../cran-comments.md): recorded submission-check context, not
  proof of a current submission or acceptance.

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
