# CRAN candidate follow-up review — 2026-10-17

[Preparation record](2026-10-17-cran-candidate.md) · [Workflow](README.md)

Package-source SHA: `17bc194bcd78f03de5e7e17ba7d5fc844d630dfa`.
Version remains 1.4.1. This follow-up changes only the longitudinal examples and
their generated help, plus build-excluded review records. No runtime source or
test assertions changed after the previous candidate.

## Exact remaining diagnostics

`R CMD check --as-cran --run-donttest`: **0 errors, 1 warning, 1 note**.

Installation warning:

```text
/Library/Frameworks/R.framework/Resources/include/R_ext/Boolean.h:62:36: warning: unknown warning group '-Wfixed-enum-extension', ignored [-Wunknown-warning-option]
```

The diagnostic is emitted by the installed R header's pragma. A standalone C++
file including only `R_ext/Boolean.h` reproduced it with Apple clang 21, without
any slca or Rcpp code. This is an R-header/compiler compatibility issue, not a
warning at a package-source line. No suppression flags or header edits were added.

HTML manual note:

```text
Skipping checking HTML validation: 'tidy' doesn't look like recent enough HTML Tidy.
Please obtain a recent version of HTML Tidy by downloading a binary
release or compiling the source code from <https://www.html-tidy.org/>.
Skipping checking math rendering: package 'V8' unavailable
```

Installed Tidy identifies itself as the 2006 Apple build 13462. V8 is unavailable.
HTML validation and math rendering remain unverified; the PDF manual passed.
This note describes unavailable validation tools, not a demonstrated HTML defect.
No new tools or dependencies were installed.

The installation log also contained `ld: warning: -ld_classic is deprecated and
will be removed in a future release`. The option came from a user-level Makevars
setting, not package source or R's system Makeconf. A separate install with
`R_MAKEVARS_USER=/dev/null` eliminated that linker warning, loaded successfully,
and passed the same full test suite. The user configuration was not edited.
The R-header warning persists in that isolated build.

## Example timing resolved without hiding examples

The previous check reported:

```text
Examples with CPU (user + system) or elapsed time > 5s
         user system elapsed
nlsy97 19.329  0.129  19.564
```

Simply reducing latent-class counts on the full data did not reliably eliminate
the long runtime. The final example keeps all three occasions, indicator blocks,
and equality constraints, using a fixed-seed 500-row subset and two-class models
with EM tolerance 1e-6. Both models converged in a timing probe (approximately
0.3 and 0.8 seconds). The text identifies this as a syntax illustration, not
full-data inference or model selection. The former `donttest` wrapper was removed,
so the smaller examples also run in ordinary checks. The first simple model still
uses the full dataset. Final full-check examples took about 15 seconds in total,
with no individual example timing note. Diagnostic bootstrap failure counts were
zero.

## Additional installed-environment checks

- R 4.5.0/macOS arm64: build, full CRAN-mode check, all examples, PDF manual, and
  175 assertions passed (0 test failures/warnings/skips).
- R 3.5.3, 3.6.0 and 4.2.0: all 17 R source files parsed. These are syntax-only
  checks, not package compatibility certification. Full checks were unavailable:
  R 3.5 lacks testthat; R 3.6 has testthat 3.0.4; R 4.2 has testthat 3.1.4 and
  lacks DiagrammeR. The candidate needs testthat >= 3.1.7. Some old version-directory
  launchers redirect to current R; the probes used the version-specific executables
  with their matching R homes. R 4.3/4.4 directories lack executable installations.
- R-header-only compiler reproduction and the isolated default-linker install
  provide separate evidence for the two environment warnings.

## Independent review handoff

The original repository keeps the local branch `prep/cran-1.4.1-20261017`.
Persistent review artifacts are under `cran-review.Rcheck/2026-10-17/` in that
repository, including source/tests-only and separate documentation/metadata diffs,
changed-file inventory, full sanitized check/install/example/test logs, environment
probes, candidate archive, and SHA-256 manifest. The source/test patch is based on
public `40d7d79c9558912f565d9e86b0e6b7a701ad0774`; forward application on that
baseline and reverse application on the candidate were checked without modifying
sources. These artifacts enable an independent review; they do not claim one has
already been performed by a second reviewer.

`cran-review.Rcheck` is excluded by R's standard build rule for directories ending
in `.Rcheck`; a sentinel directory was verified absent from the built archive.
No new build-ignore rule or change to the original working files was needed.
The persistent review bundle excludes private research, personal correspondence,
and unpublished data; its tarball contains only ordinary package-distribution
files, including the already-public package metadata and datasets.

## Minimal remaining actions

1. Review the source/test patch and approve the maintenance scope and 1.4.1 version.
2. In an already-provisioned suitable environment, run `R CMD build` and
   `R CMD check --as-cran --run-donttest` on this exact candidate with a compatible
   R/compiler, modern HTML Tidy, and V8. Check HTML and math rendering rather than
   suppressing the note.
3. Run the existing Windows/Linux/current-R/devel/oldrel matrix on the exact
   candidate; these platforms remain unchecked. Verify the stated dependency
   floor in a suitable isolated library. This task performed no external service
   upload or new installation.
4. Refresh CRAN version availability and obtain submission authorization. No push,
   tag, release publication, or CRAN submission was performed.
