# Independent-review fixes — 2026-10-17

[Previous candidate review](2026-10-17-cran-review.md) · [Maintenance](README.md)

Reviewed candidate: `13af9fa67f24a905674494ccae17d60430c0c02e`.
Corrected package source: `44d4eb5849f402dbdf89d94f49eb836e019e761f`.
Version remains 1.4.1. The following record commit changes only build-excluded
maintenance/submission notes, not the tested package contents.

## Unfinished saturated likelihood

The independent synthetic example has 16 binary indicators, two complete rows
(all 1/all 2), and 10,000 rows with only the first four indicators observed as 1.
The analytic maximum log-likelihood is
`10001 * log(10001/10002) + log(1/10002) = -10.2104904`.
Previously the 20,000-iteration result was returned as `-10.3558969`, without a
warning, understating the deviance by approximately 0.290813.

[src/saturated.cpp](../src/saturated.cpp) now returns `converged` and `iterations`,
checks the finite convergence criterion, and recomputes the likelihood at the
returned probabilities rather than using the preceding iterate. An unfinished
iteration never returns a finite `loglik`. [count_row](../R/data_encode.R)
warns and marks the saturated frequencies, probabilities, and likelihood as NA.
The fitted model frame retains `saturated.converged` and `saturated.iterations`.

The exact reproduction now returns:

```text
computed loglik: NA
analytic loglik: -10.2104904
converged: FALSE
iterations: 20000
Warning: Saturated response table did not converge; absolute fit statistics are unavailable.
```

This is an explicit unavailable result, not a claim that the analytic maximum was
computed. The existing iteration budget is unchanged. Model estimation remains
available; deviance and absolute chi-squared fit statistics are NA, and absolute
bootstrap testing rejects the unavailable saturated likelihood. A regression
checks this downstream behavior using an injected unfinished solver result. Other
tests exercise the actual difficult data, an iteration-limited solver, and a
converged solver's likelihood/probability consistency.

## Covariate row alignment

With the first observation entirely missing, the model retains row IDs 2–100.
Previously full/reversed 100-row covariates were matched, but reversed 99-row
covariates were bound positionally, changing the slope's sign.

[regress](../R/regress.R) now defaults to matching unique, nonempty, nonmissing row
names, independent of row count. Missing fitted IDs and duplicate IDs are errors.
Extra source rows may be supplied, but every fitted observation must match.
Automatic row numbers are usable only while they retain original row identity;
there is no guessed positional fallback when matching fails.

For intentionally unnamed data already in fitted-observation order, callers can
use `align = "position"`, which requires exactly the fitted row count. This new
argument is named-only after `...`, preserving the existing positional forwarding
of additional arguments. The help documents the rule and its compatibility
implication: previously accepted unmatched equal-size frames now require explicit
positional alignment or correct IDs.

The exact original, reversed-full, and reversed-retained reproductions now all
return intercept `0.9128261` and slope `0.002365126`. Regression tests also cover
explicit positional mode, its row-count check, missing IDs, and duplicate IDs.
The corrected reproducer asserts equality of all three coefficient matrices.

## Validation and re-review

[New regressions](../tests/testthat/test-independent-review.R) add four test cases
and 26 assertions. The new tests failed on the preceding candidate and pass with
the correction. The existing 175 assertions remain in the full suite.

`R CMD build` and `R CMD check --as-cran --run-donttest` on the source archive
completed successfully: **0 errors, 1 warning, 1 note**. All 201 test assertions
passed (0 failures/warnings/skips); examples, code/documentation consistency and
PDF manual passed. The two exact reproductions were also rerun against the
installed package produced by this check, with the results above and explicit
assertions on convergence status and coefficient equality.

The remaining warning is the already-reproduced R-header/Apple-clang warning for
`-Wfixed-enum-extension`. The remaining HTML note reports old Tidy and unavailable
V8. There is no example-timing note. These environment limitations and the
unexecuted platform matrix remain as described in the previous review.
The corrected 17 R source files also parsed on installed R 3.5.3, 3.6.0 and 4.2.0;
these remain syntax checks only, not full compatibility checks.

Archive SHA-256:
`caea246b64548bb6563c9cadcb8ad1b2af0f0b7e117c9b030123040e35b658be`.

The original repository keeps the updated local branch
`prep/cran-1.4.1-20261017`. A separate persistent review bundle at
`cran-review.Rcheck/2026-10-17-independent-fixes/` contains the exact before/after
reproductions, patch from the reviewed candidate, complete source/test diff from
public master, check/install/test/example logs, archive, and checksums. The earlier
review bundle is retained unchanged. Neither bundle is included in R source
packages. The original working-source snapshot remains unchanged.

No new dependencies, external builder upload, public push, release, tag, or CRAN
submission was performed. Independent re-review of these corrections and the
previously documented platform/HTML/compiler checks remain submission gates.
