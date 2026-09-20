# quickNet 0.0.0.9000

## Independent documentation examples

- Every analysis code block in both READMEs constructs its own input data or
  simulation design and displays its results. Examples can run individually
  from a fresh R session, including input checks and downstream analyses.
- Workflow validation executes each documented block in a separate R process
  and compares the English and Chinese code. Installation commands are excluded
  from analysis execution.

## Result consistency and validation

- Centrality and bridge statistics use the fitted network matrix. Directed
  matrices use rows as outcomes and columns as sources; named matrix axes and
  study sample sizes are aligned explicitly. Meta-GVAR preserves its past and
  current time blocks. Comparisons reject incompatible edge direction meanings.
- Longitudinal inputs distinguish omitted time indices from explicit time keys,
  retain original row mappings, and record the actual observations and subjects
  used in lagged, node-specific and local estimates. Reports separate these
  counts from input rows and backend sample sizes.
- Fit diagnostics retain backend convergence, inadmissibility and parameter
  checks, with unknown status when evidence is unavailable. Native warnings are
  saved in reports. Nonfatal glmnet partial paths retain source-returned solutions.
- Resampling results record failed/undefined repetitions and available failure
  reasons. Whole-subject longitudinal resampling preserves trajectories and
  rejects incomplete network layers. Percentile intervals describe the source
  estimator and are not presented as calibrated edge-significance tests.
- NIRA infers its moderation rule from the fitted Ising backend and distinguishes
  fixed-network Monte Carlo uncertainty from participant uncertainty. SymPerturb
  states its tie convention and complete participant-bootstrap procedure.
- Reproducible validation includes direct source-package comparisons, hand
  calculations, exact Ising states, Gaussian integration, exhaustive small
  sequence searches and finite-simulation coverage/ranking assessments.
- Both READMEs contain executable demonstration workflows with explicit small
  simulation budgets. CI executes both languages. Cairo PDF export supports
  multilingual labels, and undefined centrality values receive a plot caption.
  Workflow and small/medium serial/parallel performance measurements are recorded
  separately from statistical evidence.

## Sample size planning and compatibility

- Monte Carlo recovery uses estimator-specific population truth and records the
  generating graph, actual edge strengths, density and positive-definite scaling.
  Target domains and unregularized sample-size requirements are validated.
  Outputs separate failed fits from undefined target metrics and include Monte
  Carlo uncertainty, boundary flags and the limits of grid-based recommendations.
- Powerly recommendations follow its bootstrap-median curve, expose source
  generation settings and accept `model_matrix` directly. Independent validation
  uses the source package's `validate()` method.
- Historical EBIC settings are recovered from saved estimator defaults and
  arguments for reports and downstream refits. Reports distinguish CLPN data
  preprocessing from glmnet standardization and omit inapplicable correlations.
- Longitudinal psychonetrics controls follow the installed backend interface;
  unsupported explicit within-person centering receives a clear diagnostic.
- Reproducible compatibility checks cover historical serialized fits, complete
  and minimal dependency environments. CI defines Windows, Linux and macOS
  checks with a recorded dependency snapshot and current dependencies.

## Dependence tests

- Paired network comparison follows NetworkComparisonTest's within-pair
  label swaps and records its exchangeability assumptions and permutation
  details. Participant row positions are retained during swaps. Paired binary
  inputs satisfy the same category-count restriction as their conditional
  permutation samples.
- MTD coupling uses the sample-SD normalization in the Shine authors' MATLAB
  implementation. Its default inference is the Yuan--Shou truncated time-shift
  test, with an explicitly supplied `radius`, every allowed shift, and the
  published finite-sample bound. Outputs distinguish the full-series mean
  from the central-window statistic and state the stationarity assumption.
  `method = "shuffle"` explicitly selects raw-observation permutation and
  accepts `nperm`; this option requires exchangeable observations.
- Validation includes official NCT numerical comparisons, independent paired
  swap enumeration, author-generated TTS reference fixtures and reproducible
  null simulations for paired data and autocorrelated time series.

## Model parameters

- Model arguments are supplied directly through function parameters and `...`.
  Backend controls inherit source defaults; invalid or conflicting arguments
  fail explicitly. Fit metadata records backend versions and effective settings.
- MGM and mixed VAR use CV by default, with AND regularization for MGM;
  time-varying VAR uses EBIC. MGM requires variable types and levels; dynamic
  MGM requires lags, and time-varying models require estimation points and bandwidth.
- EBICglasso honors correlation and missing-data controls. Ordinal association
  estimation preserves psych's smoothing and category guard. Correlation models
  do not silently replace undefined correlations or repair the correlation matrix.
- mlVAR inherits its default effect structures, graphicalVAR estimates subject
  networks, and psychonetrics inherits estimator, covariance and missing-data defaults.
  CLPN forwards glmnet controls and records grouped CV folds and actual fold counts.
- LatentNet forwards lavaan estimators and uses its identification defaults.
  Panel SEM accepts native lavaan controls while retaining explicit residual syntax.
- Powerly uses its native simulation counts and accepts arguments directly.
  NetCompare, Bridge and netCor use the source defaults for testing and normalization.
- Stability and comparison preserve fitted model controls; observation-specific
  controls requiring manual resampling alignment are rejected clearly.

## EBIC settings

- `gamma = NULL` selects 0.5 for EBICglasso/graphicalVAR and 0.25 for
  Ising and EBIC-selected MGM/mixed VAR/time-varying mixed VAR. Explicit values in [0,1] are
  validated and retained. Non-EBIC estimation and CV record inactive gamma
  as NULL; Monte Carlo result rows use NA.
- Stability and network comparison preserve fitted estimation settings.
  `NetCompare()` accepts two exploratory cross-sectional `quicknet_fit`
  objects with matching settings and records its effective gamma.
- Reports and NIRA provenance use backend evidence or recorded metadata for
  existing fits. Resampling requires a known original EBIC gamma. Console
  explanations and bilingual documentation describe the model defaults.

## Virtual perturbation

- Continuous `Perturbation()` methods implement the SymPerturb 0.1.0 method
  specification. Gaussian means and ridge-regularized covariance are estimated
  from finite participant data in `fit$data`; topology uses thresholded partial
  correlations. Default outcome bounds are [0,4], with `bounds = NULL` available
  for unbounded outcomes.
- `dosage`, `knockout` and `knockdown` apply target location/scale maps and update
  the complete mean and covariance. `remaining_strength` expresses the retained
  target state fraction. State rankings use `system_benefit`, the weighted
  standardized improvement over non-target outcomes.
- `edge_block` and `node_block` attenuate adjacency edges and report
  `communication_block`, the relative loss of finite-step propagation.
- `combination` evaluates unit-dose target pairs on a common non-target outcome
  set. `incremental_pair_value` is joint benefit minus the better single-target
  benefit, retaining signed values.
- `sequence` uses beam search with discounted marginal benefits and costs,
  retaining candidate sequences and their stepwise paths. It shares its scored
  candidate table with the complete `symperturb` workflow.
- `symperturb` reports seven-utility VPPS, 13 separate robustness scenarios,
  scenario ranks and complete-pipeline bootstrap intervals, rank draws and
  top-k probabilities. Both full analysis and sequence optimization require
  a module mapping covering all symptoms and containing at least two modules.
- Perturbation results support summary, ranking, dose-response, node-change,
  communication-block and sequence plots, plus structured reports. Node-change
  plots display one selected condition. Documentation includes configuration,
  result fields, numerical validation and reproduction commands.

## Ising threshold intervention

- `ising_threshold` provides single-chain Gibbs threshold sensitivity analysis.
- `NIRA()` and `Perturbation(method = "nira")` implement the single-network
  workflow described by Wang et al. (2026): moderation prerequisite assessment,
  baseline and per-node simulations, adjusted plus-one permutation tests,
  and repeated-simulation rank stability.
- NIRA supports cross-platform parallel execution with reproducible
  L'Ecuyer-CMRG task streams and literature-compatible or native simulation
  engines. The `quicknet_nira` object supports print, summary, plot and
  `quicknet_report()` methods.
- Printed results and reports explain Ising EBIC settings, moderation
  statistics and Cohen's d sign conventions, with references at the end.
- Virtual perturbation results represent model-implied projections for
  hypothesis generation and candidate screening, not causal treatment effects.
