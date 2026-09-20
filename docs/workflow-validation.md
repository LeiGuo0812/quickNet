# Executable README workflows

The numbered examples in both READMEs are executable in order from a clean R session. Each language contains 32 R code blocks, including the later centrality, stability, reporting, perturbation, NIRA, comparison, MTD and export examples. The English and Chinese code parses to identical R expressions; translated comments do not change execution.

Run both languages in separate clean processes from the package root:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 Rscript --vanilla tools/validate-workflows.R
```

An individual language and output directory can also be specified:

```sh
Rscript --vanilla tools/validate-workflows.R README.zh-CN.md ../output/audit/workflows/README.zh-CN
```

The runner loads the working package, extracts the actual fenced code blocks, and evaluates each expression in order. It does not substitute mocks, skip slow models, inject missing demonstration data, or override budgets outside the documentation. A missing displayed `$` result is an error. Every retained `quicknet_fit` must contain finite network matrices, produce a nonempty public report, and have no explicitly failed backend diagnostic. The public export example must generate nonempty image, edge CSV and text-report files.

Installation commands and API/input-validation templates before numbered Example 1 are excluded: they require a user's installation location or describe data that has not yet been created. The examples themselves load quickNet and create the datasets that later blocks use.

## Demonstration design

- Binary data come from a specified four-node pairwise Ising model, using `IsingSampler`. Threshold sensitivity and NIRA use the actual `x1`/`x2` node labels.
- Panel data contain participant intercepts and adjacent-wave autoregressive/cross-lagged structure. Intensive longitudinal data contain participant means, multiple days and lagged effects. Day boundaries remain explicit.
- Confirmatory and latent models use a generated two-factor covariance structure, with separate variable names for the CFA syntax. Dynamic MGM examples create a complete ordered mixed series.
- Meta-analysis examples create study correlations from independent samples. The MetaGVAR example generates 20 independent intensive longitudinal studies with varying coefficients and fits two nodes. Its native `lowertri_randomEffects = "diag"` constraint is explicit in the example.
- Example budgets are intentionally small: Monte Carlo planning uses three sample sizes and five repetitions; powerly uses five sample-size points, five repetitions, 20 bootstraps and one iteration; network stability and SymPerturb use five resamples; NIRA uses 100 simulated observations per condition, five moderation resamples, 99 permutations and five stability repetitions, on one core. graphicalVAR uses a five-by-five grid and omits individual networks explicitly. Package defaults are unchanged.

These examples demonstrate APIs and data flow. Their small simulation budgets do not establish significance, stable rankings, confidence-interval coverage or a research sample-size recommendation. Full inference, native-reference and calibration checks are documented separately.

## Artifacts and measurements

Each language produces `extracted-examples.R`, `fitted-models.rds`, per-block logs, `blocks.json`/`blocks.rds`, `summary.json`, `provenance.rds`, `session-info.txt`, a multipage plot PDF and copies of the public export example's files. Records include README/source hashes, exact dependency versions, native warnings, per-block elapsed/user time and R allocation bytes from `Rprofmem`.

On Linux, `process_peak_RSS_kB` reads the process high-water mark from `/proc/self/status`. It is cumulative across blocks, not the additional memory attributable to that block. R allocation totals count repeated allocation and therefore differ from resident memory; native and child-process allocations are not measured by `Rprofmem`. Profiling itself adds overhead. These measurements describe the specified workflow and machine, not an asymptotic scaling benchmark.

## Execution record

Four clean sessions completed on 2026-09-20. Each ran all 32 blocks, retained 22 fitted model objects, passed public report and export checks, and completed NIRA with `assumption_check_passed`. All models exposing the inspected native optimizer status reported success; both lavaan fits also passed its admissibility check. Other backends retain `unknown` status when the inspected object has no documented status field.

| Platform / installed R | Language | Passed blocks | Sum of block evaluation seconds | R allocation total (MiB) | Process peak RSS (MiB) |
| --- | --- | ---: | ---: | ---: | ---: |
| Linux / 4.5.3 | English | 32/32 | 53.57 | 790.31 | 697.64 |
| Linux / 4.5.3 | Chinese | 32/32 | 51.62 | 790.41 | 694.83 |
| Native Windows / 4.5.0 | English | 32/32 | 39.37 | 796.03 | unavailable |
| Native Windows / 4.5.0 | Chinese | 32/32 | 37.13 | 796.16 | unavailable |

Timing excludes package loading, the between-block profiler bookkeeping, and final report/serialization checks. Runs shared the host with other verification processes, so these figures are descriptive rather than controlled platform speed comparisons. Linux artifacts are under `../output/audit/workflows/`; Windows artifacts are under `../output/audit/workflows-windows/`. Windows used its installed older dependency set, including psychonetrics 0.15, qgraph 1.9.8, bootnet 1.8, glmnet 4.1-10 and lavaan 0.6-21. Linux used psychonetrics 0.16.9, qgraph 1.10.1, bootnet 1.9.1 and glmnet 5.0. The complete versions are recorded in each session artifact. macOS execution is pending the remote compatibility workflow.

Each export contains the graph PDF, the exported adjacency matrix CSV, `edges.csv` and `report.txt`; the runner also saves a multipage workflow PDF. The small-budget results are execution fixtures and are not reported as scientific recommendations.

After execution, the README clarified that the 1,552-test SymPerturb audit was historical. `text-only-revision.json` records the executed README hash, current prose hash and both original and LF-normalized extracted-code hashes. The extraction was compared line-for-line, allowing the Windows CRLF convention, and the executable code was unchanged. Future prose-only changes can be recorded without re-estimating models:

```sh
Rscript --vanilla tools/validate-workflows.R --record-prose-revision
```

This command stops if the extracted blocks differ. Future execution records capture the source and README hashes before package loading; the initial four runs recorded source hashes at completion. The final installed-package checks separately cover the final source freeze.

## Expected native diagnostics

The `mtcars` EBIC examples can select dense graphs and retain the native specificity warning. RI-CLPM's native constructor on Linux notes that the latent-variable count exceeds the observed-variable count and uses simple starting values; Windows reports unavailable baseline-model incremental fit indices. Neither message alone is a convergence test. NIRA emits its coherent-construct and meaningful-sum-score assumption warning. The README separately explains why its small simulation budgets are unsuitable for formal use. Native messages are retained in the block records and logs rather than treated as proof that every estimator assumption has been verified.
