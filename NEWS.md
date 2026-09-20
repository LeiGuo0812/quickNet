# quickNet 0.0.0.9000

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
