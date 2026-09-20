# Data alignment, time boundaries and analysis samples

The current implementation preserves each estimator's data construction and stores the sample actually used alongside the input dimensions. Run the reproducible check from the package root:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 Rscript --vanilla tools/validate-time-alignment.R
```

The script saves fitted objects, the generated data, comparisons and `sessionInfo()` under `../output/audit/time-alignment/`. It calls the installed native constructors directly and builds a separate lookup of predecessor observations. No mock estimator is used.

## Input and time rules

`LongitudinalNet()` sorts by the supplied participant, day and occasion columns. A supplied occasion (`beep`) must be a finite integer index; missing indices, duplicate full participant/day/occasion keys, duplicate node names and overlapping node/index columns produce a diagnostic. The fitted object retains the original row mapping in `meta$input_order` and `meta$lag_index`.

When `beep = NULL`, native backends infer consecutive occasions from row order within each participant/day. Repeated participant/day values are therefore valid. When `day = NULL`, the source model treats each participant as one day. An omitted time column is not inferred from its name. `check_input()` uses these same defaults. Without an explicit occasion index, shuffling rows changes the time series; missing scheduled occasions cannot be reconstructed.

For graphicalVAR and mlVAR, supplied occasion gaps remain gaps in the padded series. A predecessor must belong to the same participant and day. mlVAR checks all requested predictor lags (including `compareToLags` when supplied). graphicalVAR currently exposes one lag. Native missing-data handling remains in effect.

MGM uses a different, native contract: lags refer to row offsets, filtered by its consecutiveness indicator. `beepvar`/`dayvar` or `consec` are passed through to `mgm::mvar()`/`tvmvar()`. MGM does not insert missing rows, and the wrapper does not reinterpret its lags as the padded-grid lags of mlVAR. A single MGM input is an ordered series; the caller must supply boundaries. Time-varying estimation requires increasing `timepoints`.

Wide panel data allow one row per unique, nonmissing participant ID; absent IDs are generated from row numbers. CLPN pools adjacent waves in the supplied wave order and removes participants with any missing required wave value. It retains the original included/dropped row numbers and keeps every transition of one participant in the same CV fold. A fixed seed reproduces the result for a fixed row order; changing participant row order changes random fold allocation. Column reordering with the same node/wave specification leaves the result unchanged.

## Meaning of recorded sample counts

`meta$analysis_sample` separates input rows/participants from the analysis design. `counts_source` names the source of the estimate.

- graphicalVAR: `temporal_rows` is native `fixedResults$N`; complete lag pairs and contributing participant counts are also recorded.
- mlVAR: `temporal_rows_by_node` comes from `stats::nobs()` on the fitted temporal models. Participant counts come from their model frames (or the fitted participant models). A common count is reported only when node counts agree. Backends without these R model methods, such as imported Mplus output, retain `NA` counts without preventing reporting; this run does not execute an external Mplus model.
- psychonetrics gvar: `backend_nobs` and `temporal_rows` use `sample@groups$nobs`. Under FIML this can include partially observed lag vectors, including initial observations. `complete_lag_rows` is reported separately and is not substituted for the likelihood's sample count. The unrelated S4 `sample@nobs` slot is not used as a row count.
- CLPN: analyzed subjects, retained/dropped input rows and the number of pooled transitions are recorded.
- MGM: the native `call$data_lagged$included` mask determines `temporal_rows`; `positive_weight_rows` and `weight_sum` are distinct. Time-varying fits record these for every estimation point in `local_samples`. Weight sums are not relabeled as independent observations or effective sample sizes.

Malformed psychonetrics extraction (missing required matrix, multiple matrices where one is required, wrong dimensions or nonfinite values) now stops with the matrix name. It is never silently truncated to the requested node count. Matching named matrix axes are aligned before presentation.

## Executed comparison

The Linux run used R 4.5.3, graphicalVAR 0.4.1, mlVAR 0.7.3, mgm 1.2-15, psychonetrics 0.16.9 and glmnet 5.0. The design seed is 46510, row shuffle seed 46511, panel design seed 46512 and panel CV seed 46513. Tolerance is `1e-8`, or `1e-6` for psychonetrics. The synthetic model contains a known positive first-node-to-second-node lag effect, eight participants, three days, missing occasions and a missing node value. Node names are Chinese. Reported matrix differences are maximum absolute differences.

| Comparison | Input | Native analysis rows | Maximum difference |
| --- | ---: | ---: | ---: |
| graphicalVAR lag 1, native PDC transpose | 1,056 rows | 996 | 0 |
| mlVAR lags 1 and 2, native temporal transpose | 1,056 rows | 942 per node | 0 |
| psychonetrics gvar, native beta | 1,056 rows | 1,056 FIML; 996 complete lag pairs | 0 |
| MGM mvar lags 1 and 2, signed target-by-predictor arrays | 129 rows | 111 | 0 |
| MGM tvmvar, two points × two lags | 129 rows | 111 at both points | 0 |
| graphicalVAR day-only inferred occasion order | 1,056 rows | 1,020 | 0 |
| CLPN adjacent-wave design and native glmnet coefficients | 120 participants | 119 participants; 238 transitions | 0 |

The three intensive longitudinal backends also reproduced exactly after shuffling rows and columns with the explicit time keys preserved. Independent endpoint comparisons matched their actual native design data. CLPN reproduced exactly after column reordering. The positive lag signal had the expected target row/source column in all three checked longitudinal matrices and CLPN. MGM local weight sums were 89.0808 and 89.4706, despite both local fits retaining 111 rows.

The simulation has no designed participant mean effects. Native mlVAR reported zero random-intercept standard deviations, and quickNet correctly omitted the unavailable between-person layer with a warning. Native graphicalVAR reported a dense selected graph. These native estimation warnings do not indicate a pairing or matrix-direction discrepancy; the script does not suppress them. This experiment checks data construction and extraction, not coverage, estimator consistency, or performance for every possible data-generating model.

## Sources

The checks inspect the installed functions `graphicalVAR:::tsData`, `mlGraphicalVAR`, `mlVAR::mlVAR`, `mlVAR:::aveLag`, `mgm:::lagData`, `mgm:::beepday2consec`, `mgm::mvar`, `mgm::tvmvar`, and `psychonetrics:::tsData`/`var1`. Public parameter and return-value contracts are documented in the official [graphicalVAR manual](https://cran.r-project.org/web/packages/graphicalVAR/graphicalVAR.pdf), [mlVAR manual](https://cran.r-project.org/web/packages/mlVAR/mlVAR.pdf), [mgm manual](https://cran.r-project.org/web/packages/mgm/mgm.pdf), and [psychonetrics manual](https://cran.r-project.org/web/packages/psychonetrics/psychonetrics.pdf). Native calls and tolerances are recorded in the validation script rather than inferred from a wrapper-only result.
