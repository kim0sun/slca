# Internal Response Encoding

`slca` separates user-facing response data from calculation inputs.

User-facing response data, including `simulate()` output, are ordinary factor data frames. They should not carry internal calculation attributes such as `y`, `yu`, `freq`, `theta`, `loglik`, or `df`.

Internal model calculations use encoded response vectors produced by `proc_data()` or `proc_data2()`:

* Missing responses are encoded as `0`.
* Observed response categories are encoded as one-based indices, `1` to `K`, as in version 1.4.0.
* Printed output and public response labels remain on the original factor-label scale.

Public input uses `NA` for missingness; an observed label of `0` is still a
valid category, not a missing-value code. The C++ likelihood, score and EM
routines skip missing responses or handle their expected counts as appropriate,
and subtract one from observed codes when indexing probability arrays.

The native random sampler still returns zero-based draws. These are mapped to
factor labels for public output and shifted by one for likelihood evaluation.
Saved response caches are regenerated from the model frame when continuing
estimation or reordering, so the intervening zero-based caches are not guessed
from their numeric ranges. Summaries likewise encode the stored response table.
