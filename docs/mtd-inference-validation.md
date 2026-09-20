# MTD inference validation

## Statistical contract and references

`MTD.No.Smooth.Test()` computes the unsmoothed product of first differences,
dividing each derivative series by its full-series sample standard deviation.
This is the descriptive MTD statistic of [Shine et al. (2015)](https://doi.org/10.1016/j.neuroimage.2015.07.064).

The default inference method is the truncated time-shift test (TTS) of
[Yuan and Shou (2024)](https://doi.org/10.1371/journal.pbio.3002758).
Its reference implementation is the article's
[S1 Code](https://doi.org/10.1371/journal.pbio.3002758.s009),
`core/test_independence.py` from `aeyuan-tts_repo-551b5a612383`.
The archived source was verified with SHA-256:

- S1 Code ZIP: `9323e192e4c586794cd7f0c795c8cb601e295f616062c237f0761ed94832e7cd`.
- `core/test_independence.py`: `5dd63754c02bf6cc1d9e89a799f3765624ea322543745540d6ecf21467481fc2`.

Users must supply `radius` in advance. There is no automatic radius selection.
With `m` first differences and radius `r`, the first series retains indices
`(r + 1):(m - r)` and the second series is shifted by every integer in
`-r:r`, without wrapping. The statistic is the absolute mean derivative
product. Full-series standardization is common to all shifts, so it cancels
in their ordering. The reference shift order is zero, positive, then negative.

If `B` shift statistics, including zero shift and ties, are at least as large
as the observed statistic, the reported p value is `min(1, B / (r + 1))`.
The unclipped reference bound remains available as `tts_bound`. No random
shuffle or additional plus-one adjustment is used for TTS. The minimum
p value is `1 / (r + 1)`.

TTS tests independence of the first-difference processes under the requirement
that the first-difference process of the second column is strictly stationary.
`coupling_mean` is the
descriptive full-series mean, whereas `test_coupling_mean` identifies the
signed central-window mean actually tested. The explicit `method = "shuffle"`
option requires raw observations in the second column to be exchangeable
under arbitrary row permutations; stationarity alone is insufficient.

## Independent calibration design

Run the standalone base-R script from the package root:

```sh
OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 \
  Rscript tools/validate-mtd-inference.R
```

An optional first argument changes the output directory. The default is
`../output/audit/inference`. The script writes a summary CSV, raw results and
metadata in RDS format, and a metadata text file. It records source hashes,
scenario seeds, and `sessionInfo()`; it requires no optional backend packages.

The calibration uses 1,000 independent data sets per scenario, 160 time
points, and a two-sided nominal level of 0.05. Each null data set contains two
independent stationary Gaussian processes, generated with independent
innovations. AR processes use 2,000 burn-in observations. The AR(2)
coefficients are `c(2 * r * cos(2 * pi / period), -r^2)`.

All choices were fixed before the corrected calibration: TTS radius 39,
159 first differences, 81 retained central differences, 79 shifts, and a
minimum p value of 0.025. This radius is a validation design choice, not a
package default or a universally recommended radius. The comparison method
uses 199 random permutations of the second column's raw observations and the
two-sided plus-one p value.

The row-shuffle statistic was independently coded and verified against a
frozen copy of the original public function using identical RNG state. The
corrected TTS results call the public `MTD.No.Smooth.Test()` directly. The
script retains the original random-draw sequence and checks that deterministic
TTS does not alter it. In this audit, all 6,000 original shuffle p values,
coupling means, and derivative-lag autocorrelations reproduced the saved
baseline exactly. The optional frozen-source and baseline checks run when
those local audit files are available; they are not required for a fresh
checkout to reproduce the calibration.

## Results

Run environment: R 4.5.3, Ubuntu 22.04.5 LTS, x86_64, base/stats only, with
BLAS threads limited to one. MCSE is the binomial Monte Carlo standard error
`sqrt(rate * (1 - rate) / 1000)`. Intervals are exact binomial 95% confidence
intervals. Percentages in the MCSE columns are percentage points.

| Independent null processes | Shuffle rejection | Shuffle MCSE | TTS rejection | TTS MCSE | TTS 95% interval |
|---|---:|---:|---:|---:|---:|
| White noise | 4.9% | 0.68 | 1.7% | 0.41 | 0.99–2.71% |
| AR(1), phi = 0.8 | 4.5% | 0.66 | 2.2% | 0.46 | 1.38–3.31% |
| AR(1), phi = -0.5 | 9.4% | 0.92 | 2.1% | 0.45 | 1.30–3.19% |
| AR(1), phi = -0.8 | 22.9% | 1.33 | 2.3% | 0.47 | 1.46–3.43% |
| AR(2), r = 0.95, period = 12 | 78.1% | 1.31 | 1.9% | 0.43 | 1.15–2.95% |
| AR(2), r = 0.98, period = 24 | 89.3% | 0.98 | 2.2% | 0.46 | 1.38–3.31% |

Null scenario seeds are 991501 through 991506 in table order. The shuffle
results show that testing only white noise and positively correlated AR(1)
processes would miss substantial miscalibration. First differences can remain
autocorrelated, and arbitrary raw-row shuffling does not preserve that null
structure. TTS was conservative in all six tested scenarios. This behavior
is compatible with a bound-based test and is not evidence that its nominal
level should be changed after seeing the results.

Two prespecified dependence checks used independent stationary AR(1)
processes `X` and `E`, each with phi 0.5, and
`Y = rho * X + sqrt(1 - rho^2) * E`, with rho 0.6 or -0.6.
There were 1,000 data sets per sign, using seeds 994001 and 994002.
TTS rejected in 1,000/1,000 cases for both signs, with exact 95% intervals
99.63–100%. The plug-in MCSE is zero at this boundary; the confidence
interval still represents finite simulation uncertainty. Every tested
coupling had the intended sign; mean tested couplings were 0.592 and -0.605.
These are strong-effect functionality checks, not general power guarantees.

The calibration supports this implementation for the tested stationary nulls
and distinguishes it from invalid arbitrary row shuffling. It does not
establish stationarity for a user's data, validity under nonstationarity,
adequate power for weaker effects, an optimal radius, or causal direction.
Radius selection and the distinction between descriptive and tested windows
must remain visible in reporting.

## Reference regression and package checks

`tools/generate-mtd-reference.py` executes the unmodified, hash-verified author
TTS function. Seven checked-in cases cover independent AR series, negative
coupling, shifted/scaled values, exact ties, zero radius, the maximal radius,
and minimum-length input. Every shift, absolute statistic, exceedance count and
bound agrees with the R implementation; numerical statistics use tolerance
`1e-12`. The fixtures also verify that shared sample-SD scaling leaves the
raw-product ranking unchanged. R regression tests need neither Python nor
network access.

On 2026-09-20, the integrated package passed all 2,255 assertions with no
failures, test warnings or skips. `R CMD check --no-manual` completed with
0 errors, 0 warnings and 0 notes, including installed-package examples and
tests. The checked archive matched all 111 source, manual, test, fixture and
validation-tool files in the working tree. PDF-manual compilation was not run.
