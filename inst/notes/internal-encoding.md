# Internal Response Encoding

`slca` separates user-facing response data from calculation inputs.

User-facing response data, including `simulate()` output, are ordinary factor data frames. They should not carry internal calculation attributes such as `y`, `yu`, `freq`, `theta`, `loglik`, or `df`.

Internal model calculations use encoded response vectors produced by `proc_data()` or `proc_data2()`:

* Missing responses are encoded as `-1`.
* Observed response categories are encoded as zero-based indices, `0` to `K - 1`.
* Printed output and public response labels remain on the original factor-label scale.

This boundary keeps public data objects stable while allowing the C++ routines to use zero-based indexing directly.
