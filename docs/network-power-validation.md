# Network power and sample-size validation

Validation date: 2026-09-20. This record covers `NetworkPower()` / `SampleSize()` and the conditional sample-size claims returned by the Monte Carlo and powerly branches.

## Current statistical contract

Parameters remain ordinary function arguments and named arguments in `...`; no new public configuration object or parameter is required.

The Monte Carlo branch conditions on one fixed generating Gaussian graphical network. `generating_network` contains its partial correlations. For `estimator = "EBICglasso"` or `"partial"`, `true_network` is that same recovery target; for `"correlation"`, `true_network` is the population **marginal correlation** matrix implied by the generating precision, with a zero diagonal. These two matrices generally differ: in a three-node chain, the nonadjacent nodes can have zero partial correlation and nonzero marginal correlation.

Network generation records the requested density and edge-strength range, the actual nonzero edge density/sign proportion/range, and the common edge scaling used to obtain a positive-definite precision matrix. Empirical correlation matrices are not silently repaired during estimation; unregularized partial-correlation designs must have more observations than nodes. Backend errors and non-finite network estimates are failed replications and retain their error messages when some replications succeed. An entirely failed run does not yield a recommendation.

Monte Carlo target values must be in their metric's range: sensitivity/specificity in `[0,1]`, MCC/edge-weight correlation in `[-1,1]`, and RMSE nonnegative. A target metric can still be undefined for a particular graph estimate, for example MCC when every estimated edge is absent. Successful fits with undefined target metrics are counted separately from failed fits. Both remain in the target-attainment probability denominator as non-attainment. If no finite target metric is available at a candidate N, that N cannot become a recommendation, even with `target_probability = 0`.

Each candidate's summary gives attainment counts, probability, Monte Carlo standard error, and an exact-binomial pointwise 95% interval. The selected N remains the smallest evaluated candidate whose **point estimate** meets the target and has finite target metrics. Its interval and whether its lower limit supports the target are explicit. These pointwise intervals are not adjusted for choosing N from the grid; independent validation at the chosen N is needed. A grid endpoint remains an endpoint, not evidence that a smaller N was ruled out or that the grid was sufficiently wide.

## Native powerly behavior

The reference is installed **powerly 1.10.0**, using its actual `powerly()` and `validate()` functions. The native procedure and `model_matrix` interface are described in the [CRAN manual](https://cran.r-project.org/web/packages/powerly/powerly.pdf). A supplied `model_matrix` does not require `nodes`, `density`, or other unused generator controls. Two-node matrices are supported, consistent with an actual native two-node run; the existing quickNet Monte Carlo generator remains scoped to at least three nodes.

The source sample-size recommendation comes from the **bootstrap-median curve** (`step_3$ci[, "50%"]`), as implemented in [StepThree.R](https://github.com/cran/powerly/blob/1.10.0/R/StepThree.R). quickNet records that curve's value at the source median N, the separate point-fit value, the source sample-size interval, range boundaries, and the comparison direction. An upper-bound fallback is not treated as attained unless the appropriate source curve actually meets the criterion. For older result structures lacking the bootstrap curve, the point-curve fallback is explicitly identified in `probability_source`.

The source generator produces observations with **five ordinal levels by default**, and its estimator uses its own EBIC gamma (0.5); these settings are recorded rather than assumed equivalent to the continuous Gaussian Monte Carlo branch. Source `StepOne` [replaces missing recovery measures with zero](https://github.com/cran/powerly/blob/1.10.0/R/StepOne.R) before computing attainment. quickNet retains that policy and records it; the original undefined-count cannot be reconstructed from those replaced values. If a target metric is intrinsically undefined for the true network, its native numerical recommendation is retained as `backend_recommended_n` but not presented as established target attainment.

For independent native validation, if the quickNet result is named `plan`, use:

```r
powerly::validate(plan$fit, replications = 300, cores = 1, verbose = FALSE)
```

A source recommendation is a model-based estimate. Agreement with the source algorithm does not establish that a finite estimated attainment probability is at least the requested target.

## Deterministic regression evidence

`tests/testthat/test-power-validation.R` has **63 passing assertions**. Coverage includes:

- Analytical marginal versus partial truth for a three-node chain.
- Actual edge scaling and positive definiteness for a dense generating network.
- Invalid metric targets, invalid sample-size ranges, and singular unregularized designs.
- Exact-binomial bounds, including all-success/all-failure outcomes where plug-in MCSE is zero but interval uncertainty remains.
- Separating backend failures from undefined target metrics, retaining both in the denominator, and preserving backend error messages.
- Bootstrap-median versus point-curve extraction, genuine boundary non-attainment, native decreasing-curve semantics, and two-node native matrix forwarding without generator arguments.

## Native powerly numerical agreement and independent validation

A fixed five-node chain with positive partial correlations 0.3 was supplied through native `model_matrix`. Both direct `powerly()` and `NetworkPower(method = "powerly")` used the same seed **88021**, sample-size range **50–500**, **8** selected sample sizes, **20** replications per selected size, **80** bootstrap draws, one iteration, and one core. The criterion was `P(sensitivity >= 0.6) >= 0.8`.

The true network, every recovery-measure value, sample-wise attainment statistics, and source sample-size recommendation were identical. The native median N was **199**. Its bootstrap-median curve was **0.8016023**; the separate fitted curve was **0.8072891**.

Using the existing `powerly::validate()` with seed **88022** and **300 independent replications** at the native recommendation gave:

| N | Achievements / repetitions | Attainment probability | MCSE | Exact-binomial 95% interval |
|---:|---:|---:|---:|---:|
| 199 | 228 / 300 | 0.7600 | 0.02466 | [0.70757, 0.80721] |

The interval includes 0.8, but its lower limit does not establish attainment of 0.8. This reduced-budget validation is an explicit example of why the source estimate should be validated; it is not a defect in reproducing the powerly algorithm and is not a claim that 199 is a generally sufficient N.

## Independent validation of the Monte Carlo design

Five fixed five-node truths were evaluated with candidate sizes **60, 120, 240, 480, 960**, **100 training replications per candidate**, EBICglasso gamma **0.5**, and criterion `P(MCC >= 0.6) >= 0.8`. Graphs and seeds were fixed before simulation:

- Chain, strength 0.15.
- Chain, strength 0.30.
- Star, strength 0.25.
- Five-node cycle, strength 0.25.
- Six-edge signed graph, absolute strength 0.20, containing both positive and negative edges.

Training uses the package's private Monte Carlo path with explicit fixed truths, so no additional public API is needed. Validation at each selected N uses **300 new data sets**, a separately written base-R Cholesky Gaussian sampler, a direct `qgraph::EBICglasso()` call, and an independently written confusion-count MCC formula. This differs from training's `MASS::mvrnorm()` sampler and does not call the package's recovery-metric helper. Undefined MCC values are counted as non-attainment in both paths.

| Fixed truth | Candidate N | Training attainment | Independent attainment | Independent MCSE | Independent 95% interval | Undefined MCC / 300 |
|---|---:|---:|---:|---:|---:|---:|
| Weak chain | 960 | 0.9900 | 0.9933 | 0.00470 | [0.97613, 0.99919] | 1 |
| Strong chain | 240 | 0.9500 | 0.9200 | 0.01566 | [0.88330, 0.94807] | 0 |
| Star | 240 | 0.9100 | 0.9133 | 0.01624 | [0.87559, 0.94261] | 5 |
| Cycle | 240 | 0.9500 | 0.8967 | 0.01757 | [0.85654, 0.92870] | 1 |
| Signed graph | 480 | 0.9000 | 0.8933 | 0.01782 | [0.85276, 0.92589] | 3 |

Training seeds are **71001–71005**; independent validation seeds are **72001–72005** in table order. All five independent lower limits exceeded 0.8. There were **2,500 training fits and 1,500 independent validation fits**; no independent fit failed, while the separately reported undefined MCC counts remained in the denominator.

These results support the implementation for the evaluated truths and candidate grid. They do not establish sample sizes for all networks, all graph densities, missingness mechanisms, ordinal/binary models, or alternative metrics. The weak-chain choice was the largest evaluated candidate and remains labeled as such. Individual intervals are pointwise, not simultaneous confidence statements across all scenarios. Sample-size conclusions remain conditional on their actual generating networks and estimation settings.

## Reproduction and artifacts

Run from the package root:

```sh
Rscript tools/validate-network-power.R ../output/audit/power
```

The script uses `pkgload::load_all()` and installed quickNet dependencies plus powerly, qgraph and MASS; it does not require devtools. Validation was run under R **4.5.3**, powerly **1.10.0**, qgraph **1.10.1**, with BLAS thread limits set to one. Package/session versions, source hashes, arguments, true matrices and seeds are recorded.

Artifacts include `powerly-native-validation.rds` and CSV, training and independent raw RDS for each topology, individual validation CSVs, `monte-carlo-held-out-summary.csv`, and `network-power-provenance.rds` / `.txt`. Machine-generated artifacts remain under workspace `output/audit/power/`; the script, tests, and this record are tracked in the package repository.
