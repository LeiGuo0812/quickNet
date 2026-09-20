# Backend parameter contract

The public API accepts named function arguments. Additional controls are passed
through `...`; users do not need to construct a parameter object. Backend
estimation defaults are inherited. Model definitions, data-layout conventions,
network extraction and intervention protocols are documented separately.

The audit covers all 32 entries in `model_registry()`, plus stability,
comparison and derived network statistics. It checks constructor defaults,
explicit parameter forwarding, inactive parameters, metadata and refitting.
Plot styling is a quickNet presentation choice, not an estimator setting.

## Source versions

Audit date: 2026-09-20. R 4.5.3; bootnet 1.9.1; qgraph 1.10.1; IsingFit 0.4;
mgm 1.2.15; glmnet 5.0; psych 2.6.5; graphicalVAR 0.4.1; mlVAR 0.7.3;
psychonetrics 0.16.9; lavaan 0.7.2; powerly 1.10.0; IsingSampler 0.5.0;
networktools 1.6.0. Installed function bodies are the reference for these versions.
Online documentation may describe a different version. In particular, this
bootnet version defaults to `corMethod = "cor"`, whereas older versions used
`"cor_auto"`. Effective backend versions are included in fit metadata.

[Source formals](backend-source-formals.csv) list each inspected backend
parameter. MGM supplies many defaults inside its function body; these are also
checked against its returned `$call`. Source expressions are documentation,
not a claim that an unresolved expression is an effective fitted value.

## Model coverage

| Model / family | Constructor and default contract | Method-specific representation |
|---|---|---|
| EBICglasso | bootnet EBICglasso; tuning 0.5, source correlation method, pairwise missingness, source lambda grid/refit/threshold controls | Symmetric edge matrix with zero diagonal |
| correlation | stats::cor; Pearson; incomplete data requires an explicit rule | Zero diagonal; no silent positive-definite repair |
| partial | stats::cor followed by matrix inversion; Pearson | Precision standardized into partial correlations; singular inputs fail |
| ordinal | psych::polychoric; smooth TRUE, correct 0.5, max.cat 8; other native controls forwarded | Polychoric output used directly; optional rank/Pearson association method |
| ising | IsingFit; AND TRUE, gamma 0.25, min_sum -Inf, native lower lambda bound | Binary 0/1 outcomes; plotting/progress suppressed by default |
| mgm | mgm::mgm; CV, AND, k 2, scale TRUE, threshold LW, binarySign FALSE; types/levels required | Pairwise adjacency returned; full higher-order model remains in raw fit |
| clpn | glmnet::cv.glmnet; alpha 1, native standardize TRUE and intercept; lambda.1se; supplied controls forwarded | Gaussian outcomes, adjacent-wave pooling and participant-grouped folds; optional standardize_data FALSE; actual folds recorded |
| ri_clpm | psychonetrics::ri_clpm; covariance innovations, no standardization, backend missing/estimator rules | Wide panel variable matrix; optional explicit stationarity constraints |
| panel_gvar | psychonetrics::panelgvar/panelvar; native within/between structures and missing/estimator controls | Temporal, within and between layers |
| panel_var | psychonetrics::panelvar; native covariance structures and missing/estimator controls | Temporal, within and between layers |
| panel_sem | lavaan::sem; native estimator, fixed.x and listwise default; no preprocessing scaling | Generated adjacent-wave syntax; auto.cov.y FALSE lets explicit residual_cov constraints determine residual edges |
| graphicalVAR | graphicalVAR::mlGraphicalVAR; gamma 0.5, scale/centerWithin TRUE, subjectNetworks TRUE; native lambda grids | Temporal, contemporaneous and between layers; wrapper supports lag 1 |
| mlVAR | mlVAR::mlVAR; estimator/temporal/contemporaneous "default"; native lag/scaling controls | Resolved settings recorded; all estimated edges extracted with nonsig = "show"; multiple positive lags supported |
| psychonetrics_gvar | psychonetrics::gvar/var1; ML, missing auto, standardize none, centerWithin FALSE; native controls | GGM contemporaneous structure fixed by model choice; lag-1 temporal layer |
| confirmatory_ggm | psychonetrics::ggm/varcov; estimator default, missing auto | omega template |
| confirmatory_ising | psychonetrics::Ising; estimator default, listwise, beta parameterization, maxNodes 20 | omega/tau/beta/responses retain source semantics |
| confirmatory_cor | psychonetrics::varcov, type cor; estimator default, missing auto | rho template |
| confirmatory_covariance | psychonetrics::varcov, type cov; estimator default, missing auto | sigma template |
| confirmatory_precision | psychonetrics::varcov, type prec; estimator default, missing auto | kappa template |
| latent_network | lavaan::cfa; native estimator, std.lv FALSE, listwise; robust/ordered estimator controls forwarded | Latent correlation layer; optional supplementary factor-score residual correlation layer |
| lvm | psychonetrics::lvm; estimator default, missing auto, loading identification | User loading matrix; covariance parameterizations configurable |
| lnm | psychonetrics::lnm/lvm; native estimation defaults | Alias fixes latent GGM and residual covariance |
| rnm | psychonetrics::rnm/lvm; native estimation defaults | Alias fixes residual GGM and latent covariance |
| lrnm | psychonetrics::lrnm/lvm; native estimation defaults | Alias fixes both latent and residual GGM |
| meta_ggm | psychonetrics::meta_varcov; randomEffects chol, estimator FIML; native Vmethod/Vestimation and other controls | Population GGM layer |
| meta_cor | psychonetrics::meta_varcov; same meta-analysis defaults | Population correlation layer |
| meta_gvar | psychonetrics::meta_gvar/meta_var1; chol random effects and FIML | Population temporal and contemporaneous layers |
| mixedVAR | mgm::mvar; CV, scale TRUE, threshold LW; explicit lags/types/levels required | All requested lag layers retained |
| time_varying_mvar | mgm::tvmvar; EBIC, lambdaGam 0.25, threshold HW; explicit lags/estpoints/bandwidth | Time-index normalization and local estimation follow mgm; all local/lag layers retained |
| power | powerly native sensitivity/30 points/30 replications/10000 bootstrap samples; required range and network dimensions | Separate quickNet Monte Carlo method has its own recorded planning design; summary uses effective native metric/targets; internal GGM gamma 0.5 is recorded separately as backend_gamma |
| perturbation | SymPerturb 0.1.0 specification and reference fixtures | Location/scale interventions, clipping, conditional attribution, structural edits and optimization follow documented protocol settings |
| nira | IsingSampler settings and NIRA workflow | Fixed fitted Ising parameters; moderation deliberately uses EBIC and binarySign TRUE; protocol settings and effect orientation are explicitly documented |

## Additional workflows and parameter handling

- `EBICglassoNet()` forwards estimation arguments directly. `quickNet()` routes
  recognized model controls to the estimator and plotting controls to qgraph.
  A shared name such as `threshold` controls estimation; plot it separately with
  `plot(fit, threshold = ...)` when desired. Unknown names and duplicate controls
  fail instead of being consumed silently.
- `Stability()` and fitted-input `NetCompare()` reuse original model controls.
  CV fits do not acquire an EBIC gamma during refitting. Missing original EBIC
  provenance still requires refitting the original data. Observation-specific
  controls such as weights/foldid are rejected before resampling because a new
  row order also requires remapping those controls.
- Fits created by earlier quickNet constructors retain their documented MGM
  criterion/rule, CLPN preprocessing and association-matrix repair when refitted.
  Backend evidence takes precedence over stale metadata; the recovered
  association repair policy is recorded as `meta$repair_pd`.
- `LongitudinalStability()` retains constructor controls, participant grouping
  and the selected model's applicable settings. `nfolds` applies only to CLPN.
- `NetCompare()` uses the NCT source defaults: 100 permutations, edge and
  centrality testing FALSE, strength/expected influence if centrality is enabled.
  `add.bridge = TRUE` explicitly enables and adds bridge tests.
- `Bridge()` inherits normalize FALSE. `netCor()` inherits 999 permutations,
  graph FALSE and a two-sided alternative from `ape::mantel.test`.
- Centrality uses the source calculation defaults; plot scaling and additional
  standardized display columns are output conventions. MTD and the Monte Carlo
  planner are quickNet algorithms with their own documented design parameters.
- quiet/progress controls affect presentation. They do not redefine statistical
  estimators. Source warnings are available; MGM does not suppress them by default.

`backend_args` is internal provenance for explicitly forwarded controls;
`backend_settings` records effective settings exposed by the backend or the
actual call; `backend_version` identifies the implementation. These are returned
metadata, not extra objects that callers must construct. Source default
expressions, where supplied as `backend_defaults`, remain separate from fitted
settings. Additional model-specific information remains in the raw backend fit.

## Sources and verification

- [bootnet source](https://github.com/SachaEpskamp/bootnet)
- [MGM documentation](https://search.r-project.org/CRAN/refmans/mgm/html/mgm.html)
- [glmnet reference](https://glmnet.stanford.edu/reference/cv.glmnet.html)
- [mlVAR source](https://github.com/SachaEpskamp/mlVAR/blob/master/R/mlVAR.R)
- [psychonetrics source](https://github.com/SachaEpskamp/psychonetrics)
- [lavaan estimation documentation](https://lavaan.ugent.be/tutorial/est.html)
- [powerly reference](https://search.r-project.org/CRAN/refmans/powerly/html/powerly.html)
- [NCT source](https://github.com/cvborkulo/NetworkComparisonTest/blob/master/R/NCT.R)

Regression tests compare direct backend calls and quickNet fits using identical
data and RNG state, verify non-default arguments and refitting, and check model
metadata. Existing full-package tests cover the remaining model families and
NIRA/SymPerturb fixtures. The package is also checked with R CMD check.

Validation on 2026-09-20:

- `R CMD check --no-manual` completed with 0 errors, 0 warnings and 0 notes.
  Its installed-package test suite passed 1,761 assertions, with 0 failures,
  0 warnings and 0 skips; package examples also passed.
- A separate real Powerly run completed with 4 nodes, density 0.5, sample-size
  range 50–200, 5 sample-size points, 5 replications, 20 bootstrap samples and
  1 iteration. This checks the backend integration and result extraction;
  these reduced counts are not a recommended study-planning design.
- The tested source archive matched all 101 R source, test and manual files
  in the working tree. NCT defaults were checked against its upstream source;
  quickNet's bundled comparison implementation is covered by the package tests.
