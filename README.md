![QuickNet](https://github.com/LeiGuo0812/quickNet/assets/50766698/240de5bc-e4a9-41ef-b04e-97e4e6b9878b)

# quickNet

[Chinese version](README.zh-CN.md)

`quickNet` provides a compact interface for estimating, plotting, summarizing, and comparing psychological networks in R. The current API returns a unified `quicknet_fit` object, keeps the main legacy function names, and supports cross-sectional, cross-lagged panel, and intensive longitudinal network models.

## Installation

### Install from GitHub

```r
if (!require(devtools)) {
  install.packages("devtools")
}

devtools::install_github("LeiGuo0812/quickNet")
```

### Install from a local ZIP file

Click `Code -> Download ZIP`, then set your R working directory to the folder containing the downloaded ZIP file.

```r
if (!require(devtools)) {
  install.packages("devtools")
}

devtools::install_local("quickNet-main.zip")
```

Some models require optional backend packages: `PanelNet(model = "clpn")` requires `glmnet`, `PanelNet(model = "ri_clpm" / "panel_gvar" / "panel_var")` requires `psychonetrics`, `LongitudinalNet(model = "graphicalVAR")` requires `graphicalVAR`, `LongitudinalNet(model = "mlVAR")` requires `mlVAR`, and `LongitudinalNet(model = "psychonetrics_gvar")` requires `psychonetrics`.
Additional optional modules use `powerly`, `psychonetrics`, `lavaan`, and `MASS` for sample size planning, confirmatory networks, latent/residual networks, and SEM-based panel networks.

## Unified Output

Main model-fitting functions return a `quicknet_fit` object. Common fields are:

```r
fit$graph             # Default network matrix
fit$networks          # One or more network layers
fit$edges             # Edge table
fit$nodes             # Node-level table
summary(fit)          # Network-level summary
plot(fit)             # Quick network plot
```

Use `model_registry()` to inspect the package-level model map, including the
model family, backend, analysis type, network layers, reportable quantities,
key references, and known limitations.

```r
model_registry()
model_registry("ConfirmatoryNet")
```

`quicknet_report(fit)` returns academic reporting tables. For
psychonetrics-backed models, the report includes `fit_indices`, `parameters`,
`modification_indices`, and `constraints` in addition to the common sample,
network, edge, and node summaries.

## Available Models

| Data type | Function | Model name | Brief description |
| --- | --- | --- | --- |
| Cross-sectional continuous data | `quickNet()` | `"EBICglasso"` | Sparse Gaussian graphical model estimated with EBICglasso. This is the default option for continuous psychological network data. |
| Cross-sectional continuous data | `quickNet()` | `"correlation"` | Zero-order correlation network. Edges represent pairwise associations without conditioning on other variables. |
| Cross-sectional continuous data | `quickNet()` | `"partial"` | Partial correlation network. Edges represent conditional associations after adjusting for all other nodes. |
| Cross-sectional binary data | `quickNet()` | `"ising"` | Ising model for 0/1 variables. Useful for binary symptoms, endorsements, or event indicators. |
| Cross-sectional ordinal data | `quickNet()` | `"ordinal"` | Ordinal association network based on polychoric-style correlations. Useful for Likert-type items. |
| Cross-sectional mixed data | `quickNet()` | `"mgm"` | Mixed Graphical Model for combinations of Gaussian, categorical, and other variable types. |
| Wide-format panel data | `PanelNet()` | `"clpn"` | Cross-lagged panel network. Edges are directed from previous-wave nodes to next-wave nodes. |
| Wide-format panel data | `PanelNet()` | `"ri_clpm"` | Random-intercept cross-lagged panel model via `psychonetrics`, separating within-person dynamics from stable between-person differences. |
| Wide-format panel data | `PanelNet()` | `"panel_gvar"` | Panel graphical VAR via `psychonetrics::panelgvar()`, returning temporal, within-person, and between-person network layers. |
| Wide-format panel data | `PanelNet()` | `"panel_var"` | Panel VAR via `psychonetrics::panelvar()`, returning temporal and covariance-based within/between layers. |
| Wide-format panel data | `PanelSEMNet()` | `"panel_sem"` | SEM-based cross-lagged panel network with lavaan fit indices. |
| Long-format intensive longitudinal data | `LongitudinalNet()` | `"graphicalVAR"` | Multilevel graphical VAR model returning temporal, contemporaneous, and between-person networks. |
| Long-format intensive longitudinal data | `LongitudinalNet()` | `"mlVAR"` | Multilevel VAR model via `mlVAR`, also returning temporal, contemporaneous, and between-person networks. |
| Long-format intensive longitudinal data | `LongitudinalNet()` | `"psychonetrics_gvar"` | Lag-1 graphical VAR via `psychonetrics::gvar()`, returning temporal and contemporaneous networks. |
| Time-ordered mixed data | `MixedVARNet()` | `"mixedVAR"` | Mixed vector autoregressive network for continuous and categorical time-series variables. |
| Time-ordered mixed data | `TimeVaryingNet()` | `"time_varying_mvar"` | Time-varying mixed VAR networks estimated at user-defined time points. |
| Cross-sectional continuous data | `ConfirmatoryNet()` | `"confirmatory_ggm"` | Confirmatory Gaussian graphical model with user-specified free and fixed edges. |
| CFA/SEM data | `LatentNet()` | `"latent_network"` | Latent-variable correlation network and optional residual item network after CFA. |
| CFA/SEM data | `LatentNet()` | `"lvm"` | Psychonetrics latent variable model with latent and residual covariance layers. |
| CFA/SEM data | `LatentNet()` | `"lnm"` | Psychonetrics latent network model; the latent network is estimated as a GGM. |
| CFA/SEM data | `LatentNet()` | `"rnm"` | Psychonetrics residual network model; the residual item network is estimated as a GGM. |
| CFA/SEM data | `LatentNet()` | `"lrnm"` | Psychonetrics latent-and-residual network model with both GGM layers. |
| Meta-analytic correlation/covariance data | `MetaNet()` | `"meta_ggm"` | Psychonetrics meta-analytic Gaussian graphical model from multiple study matrices or study-level raw data. |
| Meta-analytic correlation/covariance data | `MetaNet()` | `"meta_cor"` | Psychonetrics meta-analytic pooled correlation network. |
| Meta-analytic intensive longitudinal data | `MetaNet()` | `"meta_gvar"` | Psychonetrics single-stage meta-analytic GVAR with temporal and contemporaneous network layers. |
| Confirmatory cross-sectional data | `ConfirmatoryNet()` | `"confirmatory_ising"` | Psychonetrics confirmatory Ising model for binary variables. |
| Confirmatory cross-sectional data | `ConfirmatoryNet()` | `"confirmatory_cor"` | Psychonetrics confirmatory correlation model. |
| Confirmatory cross-sectional data | `ConfirmatoryNet()` | `"confirmatory_covariance"` | Psychonetrics confirmatory covariance model. |
| Confirmatory cross-sectional data | `ConfirmatoryNet()` | `"confirmatory_precision"` | Psychonetrics confirmatory precision-matrix model. |

Sample size planning uses a separate `quicknet_power` object returned by `NetworkPower()` or its alias `SampleSize()`.

`EBICglassoNet()` is retained as a convenience wrapper for the cross-sectional `model = "EBICglasso"` workflow.

For academic use, cite the methods that match the models and analyses you report. The essential references are listed in [References](#references).

## Input Requirements

Each model has explicit input checks. Use `input_requirements()` to inspect expected input format, or `check_input()` to diagnose a dataset before fitting.

```r
input_requirements("ising")

check_input(binary_data, model = "ising")
check_input(panel_data, model = "clpn", nodes = c("x1", "x2", "x3"), waves = 1:3)
check_input(esm_data, model = "graphicalVAR", vars = c("x1", "x2", "x3"))
```

Model-fitting functions call the same validator internally. Clear format errors stop early; risk conditions such as very small samples or imbalanced binary variables are shown as warnings.

## Minimal Examples

### 1. EBICglasso Cross-Sectional Network

```r
library(quickNet)

fit <- quickNet(mtcars[, 1:6], model = "EBICglasso", pie = FALSE)

summary(fit)
fit$edges
plot(fit)
```

Legacy convenience interface:

```r
fit <- EBICglassoNet(mtcars[, 1:6])
```

### 2. Correlation Network

```r
fit <- quickNet(
  mtcars[, 1:6],
  model = "correlation",
  cor_method = "pearson",
  pie = FALSE
)

fit$graph
fit$nodes
```

### 3. Partial Correlation Network

```r
fit <- quickNet(
  mtcars[, 1:6],
  model = "partial",
  cor_method = "pearson",
  pie = FALSE
)

summary(fit)
```

### 4. Ising Network for Binary Data

```r
set.seed(1)
binary_data <- data.frame(
  x1 = rbinom(120, 1, 0.50),
  x2 = rbinom(120, 1, 0.45),
  x3 = rbinom(120, 1, 0.55),
  x4 = rbinom(120, 1, 0.50)
)

fit <- quickNet(binary_data, model = "ising", gamma = 0.25, pie = FALSE)

fit$edges
fit$nodes
```

### 5. Ordinal Network

```r
set.seed(1)
ordinal_data <- data.frame(
  x1 = sample(1:5, 120, replace = TRUE),
  x2 = sample(1:5, 120, replace = TRUE),
  x3 = sample(1:5, 120, replace = TRUE),
  x4 = sample(1:5, 120, replace = TRUE)
)

fit <- quickNet(
  ordinal_data,
  model = "ordinal",
  ordinal_method = "polychoric",
  pie = FALSE
)

summary(fit)
```

### 6. Mixed Graphical Model

```r
set.seed(1)
mixed_data <- data.frame(
  c1 = rnorm(120),
  c2 = rnorm(120),
  d1 = sample(1:2, 120, replace = TRUE),
  d2 = sample(1:2, 120, replace = TRUE)
)

fit <- quickNet(
  mixed_data,
  model = "mgm",
  types = c("g", "g", "c", "c"),
  levels = c(1, 1, 2, 2),
  gamma = 0.25,
  pie = FALSE
)

fit$nodes
```

## Longitudinal Network Examples

### 7. Cross-Lagged Panel Network

`PanelNet()` expects wide-format panel data. By default, column names should follow the pattern `node_twave`, for example `x1_t1` and `x1_t2`.

```r
set.seed(1)
n <- 80
panel_data <- data.frame(id = seq_len(n))

for (wave in 1:3) {
  panel_data[[paste0("x1_t", wave)]] <- rnorm(n)
  panel_data[[paste0("x2_t", wave)]] <- rnorm(n)
  panel_data[[paste0("x3_t", wave)]] <- rnorm(n)
}

panel_fit <- PanelNet(
  panel_data,
  nodes = c("x1", "x2", "x3"),
  waves = 1:3,
  id = "id",
  nfolds = 5
)

panel_fit$networks$default       # Autoregressive and cross-lagged paths
panel_fit$networks$cross_lagged  # Cross-lagged paths only
panel_fit$edges
```

### 8. psychonetrics Panel Models

The same wide panel format can be used for random-intercept CLPM and panel GVAR models.

```r
ri_fit <- PanelNet(
  panel_data,
  nodes = c("x1", "x2", "x3"),
  waves = 1:3,
  model = "ri_clpm"
)

panel_gvar <- PanelNet(
  panel_data,
  nodes = c("x1", "x2", "x3"),
  waves = 1:3,
  model = "panel_gvar"
)

ri_fit$networks$temporal
ri_fit$networks$random_intercept
panel_gvar$networks$within
panel_gvar$networks$between
```

### 9. graphicalVAR Intensive Longitudinal Network

`LongitudinalNet()` expects long-format data with a subject ID, a day/date variable, and a within-day measurement occasion variable.

```r
set.seed(1)
ids <- rep(1:8, each = 12)
time <- rep(1:12, times = 8)

esm_data <- data.frame(
  id = ids,
  day = ceiling(time / 4),
  beep = ((time - 1) %% 4) + 1,
  x1 = rnorm(length(ids)),
  x2 = rnorm(length(ids)),
  x3 = rnorm(length(ids))
)

gvar_fit <- LongitudinalNet(
  esm_data,
  vars = c("x1", "x2", "x3"),
  id = "id",
  day = "day",
  beep = "beep",
  model = "graphicalVAR"
)

gvar_fit$networks$temporal
gvar_fit$networks$contemporaneous
gvar_fit$networks$between
```

### 10. psychonetrics GVAR Intensive Longitudinal Network

```r
psy_gvar <- LongitudinalNet(
  esm_data,
  vars = c("x1", "x2", "x3"),
  id = "id",
  day = "day",
  beep = "beep",
  model = "psychonetrics_gvar"
)

psy_gvar$networks$temporal
psy_gvar$networks$contemporaneous
```

### 11. mlVAR Intensive Longitudinal Network

```r
mlvar_fit <- LongitudinalNet(
  esm_data,
  vars = c("x1", "x2", "x3"),
  id = "id",
  day = "day",
  beep = "beep",
  model = "mlVAR",
  temporal = "fixed",
  contemporaneous = "fixed",
  nCores = 1
)

mlvar_fit$edges
mlvar_fit$nodes
```

### 12. Network Power and Sample Size Planning

```r
power <- NetworkPower(
  nodes = 8,
  density = 0.30,
  replications = 100,
  target_metric = "mcc",
  target_value = 0.60,
  target_probability = 0.80
)

summary(power)
plot(power)
quicknet_report(power)$text
```

If `sample_sizes = NULL`, `NetworkPower()` generates an adaptive candidate
grid from the number of nodes. The default target metric is `mcc`, which is
more balanced than sensitivity alone because it penalizes both false negatives
and false positives. If you explicitly use `target_metric = "sensitivity"`,
interpret the recommendation as optimistic unless specificity or false-positive
control is also reported.

For `powerly`-based GGM planning:

```r
powerly_plan <- NetworkPower(
  method = "powerly",
  nodes = 8,
  density = 0.30,
  target_metric = "sensitivity",
  target_value = 0.60,
  target_probability = 0.80
)
```

### 13. Confirmatory, Latent, and Dynamic Networks

```r
omega <- matrix(1, 6, 6)
diag(omega) <- 0
colnames(omega) <- rownames(omega) <- paste0("x", 1:6)

confirmatory <- ConfirmatoryNet(data, vars = paste0("x", 1:6), omega = omega)

confirmatory_cor <- ConfirmatoryNet(data, vars = paste0("x", 1:6), model = "cor")
confirmatory_precision <- ConfirmatoryNet(data, vars = paste0("x", 1:6), model = "precision")
```

```r
confirmatory_ising <- ConfirmatoryNet(binary_data, model = "ising")
confirmatory_ising$networks$default
```

```r
cfa_model <- "
Depression =~ d1 + d2 + d3
Anxiety    =~ a1 + a2 + a3
"

latent <- LatentNet(data, model = cfa_model)
latent$networks$latent
latent$networks$residual
```

```r
lambda <- matrix(0, 6, 2, dimnames = list(paste0("x", 1:6), c("Depression", "Anxiety")))
lambda[1:3, "Depression"] <- 1
lambda[4:6, "Anxiety"] <- 1

lnm <- LatentNet(data, model = "lnm", vars = paste0("x", 1:6), lambda = lambda)
lrnm <- LatentNet(data, model = "lrnm", vars = paste0("x", 1:6), lambda = lambda)

lnm$networks$latent
lrnm$networks$residual
```

```r
panel_sem <- PanelSEMNet(panel_data, nodes = c("x1", "x2", "x3"), waves = 1:3)

mixed_var <- MixedVARNet(
  time_data,
  types = c("g", "g", "c"),
  levels = c(1, 1, 2)
)

tv_mvar <- TimeVaryingNet(
  time_data,
  types = c("g", "g", "c"),
  levels = c(1, 1, 2),
  estpoints = c(0.25, 0.50, 0.75)
)
```

### 14. Meta-Analytic Networks

`MetaNet()` estimates psychonetrics meta-analytic network models from multiple study correlation/covariance matrices or multi-study raw data.

```r
cors <- list(study1_cor, study2_cor, study3_cor)
nobs <- c(150, 180, 220)

meta_ggm <- MetaNet(
  cors = cors,
  nobs = nobs,
  vars = c("x1", "x2", "x3"),
  model = "meta_ggm"
)

meta_ggm$networks$default
quicknet_report(meta_ggm)$sample
```

For multi-study intensive longitudinal data:

```r
meta_gvar <- MetaNet(
  data = multi_study_esm,
  studyvar = "study",
  vars = c("x1", "x2", "x3"),
  id = "id",
  day = "day",
  beep = "beep",
  model = "meta_gvar"
)

meta_gvar$networks$temporal
meta_gvar$networks$contemporaneous
```

## Common Follow-Up Analyses

### Centrality and Bridge Centrality

```r
fit <- quickNet(mtcars[, 1:6], pie = FALSE)

centrality <- Centrality(fit)
centrality$node_table

bridge <- Bridge(
  fit,
  communities = list(group1 = 1:3, group2 = 4:6)
)
bridge$bridge_data
```

### Stability Analysis

Cross-sectional network:

```r
fit <- quickNet(mtcars[, 1:6], model = "correlation", pie = FALSE)
stability <- Stability(fit, nboot = 100)

stability$edge_bootstrap_stability
stability$case_drop_centrality_stability
```

Longitudinal network:

```r
longitudinal_stability <- LongitudinalStability(panel_fit, nboot = 100)
```

### Academic Reporting Parameters

Use `quicknet_report()` to extract report-ready sample information, estimation settings, network summaries, edge summaries, node-level indices, and model-specific parameters from any `quicknet_fit` object.

```r
fit <- quickNet(mtcars[, 1:6], model = "EBICglasso", pie = FALSE)
report <- quicknet_report(fit)

report$sample          # Sample size and node count
report$estimation      # Estimator and tuning parameters
report$networks        # Density and edge-weight summary by network layer
report$edges           # Positive/negative/nonzero edge counts
report$nodes           # Node-level centrality and predictability
report$model_specific  # Model-specific report fields
report$text            # Short plain-language summary
```

### Virtual Perturbation and Intervention Simulation

Use `Perturbation()` for model-implied in silico perturbation analyses. These results are useful for hypothesis generation and candidate target screening, but they should not be interpreted as causal intervention effects.

```r
fit <- quickNet(mtcars[, 1:6], model = "partial", pie = FALSE)

dosage <- Perturbation(
  fit,
  method = "dosage",
  targets = c("mpg", "cyl"),
  dose = c(0.25, 0.50, 1.00)
)

dosage$metrics
dosage$rankings
quicknet_report(dosage)$text

plot(dosage)
get_perturbation_plot(dosage, type = "rank")
get_perturbation_plot(dosage, type = "dose_response")
get_perturbation_plot(dosage, type = "node_change", perturbation_id = 1)
```

Continuous networks support Gaussian conditioning, virtual knockout, virtual knockdown, precision-edge blocking, combination perturbation, and greedy sequence optimization:

```r
Perturbation(fit, method = "knockout", targets = "mpg")
Perturbation(fit, method = "knockdown", targets = "mpg", remaining_strength = 0.50)
blocked <- Perturbation(fit, method = "edge_block", targets = "mpg")
Perturbation(fit, method = "combination", targets = c("mpg", "cyl", "disp"))
sequence <- Perturbation(fit, method = "sequence", targets = c("mpg", "cyl", "disp"), steps = 2)

get_perturbation_plot(blocked, type = "edge_block")
get_perturbation_plot(sequence, type = "sequence")
```

For Ising models, the legacy `ising_threshold` method remains available as a
lightweight single-chain sensitivity analysis:

```r
ising_fit <- quickNet(binary_data, model = "ising", gamma = 0.25, pie = FALSE)

ising_result <- Perturbation(
  ising_fit,
  method = "ising_threshold",
  targets = c("b1", "b2"),
  threshold_shift = -0.5
)

get_perturbation_plot(ising_result, type = "rank")
get_perturbation_plot(ising_result, type = "node_change", target = "b1")
```

For the formal, single-network NIRA workflow described by Wang et al. (2026),
use `NIRA()` (or `Perturbation(method = "nira")`). The implementation performs
the moderation prerequisite, original and one-node-at-a-time threshold
simulations, multiplicity-adjusted permutation tests, and repeated-simulation
rank stability:

```r
nira_result <- NIRA(
  ising_fit,
  perturbation_type = "alleviating",
  amount_of_SDs_perturbation = 2,
  n_samples = 5000,
  moderation_nboot = 1000,
  n_permutations = 5000,
  stability_reps = 1000,
  parallel = TRUE,
  ncores = 6,
  seed = 2025,
  engine = "literature",
  engine_iterations = 100
)

summary(nira_result)
nira_result$rankings
quicknet_report(nira_result)
plot(nira_result, type = "effect")
plot(nira_result, type = "stability")
```

For development, `moderation_nboot = 100` and `stability_reps = 100` can be
used to shorten runtime; formal analyses should use at least 1000 moderation
resamples and the default 1000 stability repetitions. The default
`engine = "literature"` uses the IsingSampler 0/1 parameterization. The
`native` engine uses independently initialized Gibbs chains with the same
conditional probabilities. Task-specific L'Ecuyer-CMRG substreams make
serial and PSOCK-parallel task results reproducible for a fixed R/RNG version.
The default `engine_iterations = 100` matches the reference workflow but is
not a convergence guarantee. Strongly coupled or multimodal networks should
be checked with larger values as a sensitivity analysis. Automatic parallel
selection uses at most four workers, and each worker is limited to one
BLAS/OpenMP thread to avoid nested thread oversubscription.

The implementation also rejects non-finite, underflowed, or machine-precision
no-op threshold changes and task counts that would overflow R's integer range.
MGM moderation signs are retained when MGM defines them; failed moderation and
stability repetitions keep their original indices and error messages. An
installed-package smoke test confirmed that serial and two-worker PSOCK MGM
results are identical for fixed RNG streams.

Stable moderation blocks NIRA by default because changing a threshold while
holding every edge fixed is then contradicted by the fitted data. Setting
`proceed_on_moderation = TRUE` continues only as an explicitly flagged
fixed-edge-assumption violation. Not detecting stable moderation is not proof
that moderation is absent. Moderation estimates retain MGM's reported sign
when it is defined. If MGM reports a selected categorical-interaction sign as
undefined, that role is summarized on a magnitude-only scale and directional
proportions are reported as unavailable rather than inventing a direction.

NIRA requires cross-sectional, complete, 0/1 data; independent observations;
an interpretable pairwise Ising model; thresholds that can be interpreted as
spontaneous activation tendencies; a node set representing a coherent
construct; and a theoretically meaningful total score. Its normal confidence
intervals, Cohen's d values, and permutation p values describe distributions
simulated from fixed estimated parameters and do not include network-estimation
uncertainty. Simulation-stability ranks are not bootstrap stability and do not
show that rank 1 differs significantly from rank 2.

Perturbation and NIRA plots are intentionally conservative. They display only
computed model-implied simulation summaries and do not imply clinical efficacy
or causal treatment effects. Real clinical use requires external intervention
studies.

Developer compatibility checks use only public reference-package APIs. On the
recorded five-node fixture, quickNet and nodeIdentifyR had the same complete
ranking (`N5 > N4 > N3 > N2 > N1`); all condition means and 30 marginal
activation probabilities were within joint Monte Carlo uncertainty. NIRApost
checks confirmed the `1 / 5001` plus-one permutation grid, Holm adjustment,
identical stability rank frequencies, and matching moderation-selection
semantics. Exact versions, commits, numerical results, and executable scripts
are recorded in
[`inst/validation/NIRA_REFERENCE_VALIDATION.md`](inst/validation/NIRA_REFERENCE_VALIDATION.md).

This interpretation follows the network intervention and simulation literature, while also respecting cautions that centrality or model-implied perturbation rankings should not be treated as direct causal treatment effects without a suitable causal design.

### Network Comparison

```r
net1 <- quickNet(mtcars[, 1:6], pie = FALSE)
net2 <- quickNet((mtcars[, 1:6])^2, pie = FALSE)

comparison <- NetCompare(mtcars[, 1:6], (mtcars[, 1:6])^2, it = 100)
plots <- get_compare_plot(comparison, net1, output = FALSE)
```

### Export Plots and Tables

```r
fit <- quickNet(mtcars[, 1:6], pie = FALSE)

get_network_plot(fit, path = tempdir(), prefix = "example")
get_edges_df(fit)
globalCoeff(fit)
```

## References

If you use `quickNet` in academic work, cite the package and the method references relevant to the models or helper functions used:

- Cross-sectional psychological network estimation and visualization: Epskamp, S., Cramer, A. O. J., Waldorp, L. J., Schmittmann, V. D., & Borsboom, D. (2012). `qgraph`: Network visualizations of relationships in psychometric data. *Journal of Statistical Software, 48*(4), 1-18. https://doi.org/10.18637/jss.v048.i04
- Network estimation, accuracy, and stability: Epskamp, S., Borsboom, D., & Fried, E. I. (2018). Estimating psychological networks and their accuracy: A tutorial paper. *Behavior Research Methods, 50*, 195-212. https://doi.org/10.3758/s13428-017-0862-1
- EBIC model selection for Gaussian graphical models: Foygel, R., & Drton, M. (2010). Extended Bayesian information criteria for Gaussian graphical models. *Advances in Neural Information Processing Systems, 23*. https://proceedings.neurips.cc/paper/2010/hash/072b030ba126b2f4b2374f342be9ed44-Abstract.html
- Regularized generalized linear models used by panel-network backends: Friedman, J., Hastie, T., & Tibshirani, R. (2010). Regularization paths for generalized linear models via coordinate descent. *Journal of Statistical Software, 33*(1), 1-22. https://doi.org/10.18637/jss.v033.i01
- Binary Ising networks: van Borkulo, C. D., Borsboom, D., Epskamp, S., Blanken, T. F., Boschloo, L., Schoevers, R. A., & Waldorp, L. J. (2014). A new method for constructing networks from binary data. *Scientific Reports, 4*, 5918. https://doi.org/10.1038/srep05918
- Mixed Graphical Models: Haslbeck, J. M. B., & Waldorp, L. J. (2020). `mgm`: Estimating time-varying mixed graphical models in high-dimensional data. *Journal of Statistical Software, 93*(8), 1-46. https://doi.org/10.18637/jss.v093.i08
- Cross-sectional and time-series Gaussian graphical models, including graphicalVAR-style models: Epskamp, S., Waldorp, L. J., Mõttus, R., & Borsboom, D. (2018). The Gaussian graphical model in cross-sectional and time-series data. *Multivariate Behavioral Research, 53*(4), 453-480. https://doi.org/10.1080/00273171.2018.1454823
- Longitudinal psychopathology networks and vector autoregression: Bringmann, L. F., Vissers, N., Wichers, M., Geschwind, N., Kuppens, P., Peeters, F., Borsboom, D., & Tuerlinckx, F. (2013). A network approach to psychopathology: New insights into clinical longitudinal data. *PLOS ONE, 8*(4), e60188. https://doi.org/10.1371/journal.pone.0060188
- Network sample size planning: Constantin, M. A., Schuurman, N. K., & Vermunt, J. K. (2021). A general Monte Carlo method for sample size analysis in the context of network models. https://doi.org/10.31234/osf.io/j5v7u
- Generalized network psychometrics and confirmatory network models: Epskamp, S., Rhemtulla, M., & Borsboom, D. (2017). Generalized network psychometrics: Combining network and latent variable models. *Psychometrika, 82*, 904-927. https://doi.org/10.1007/s11336-017-9557-x
- Random-intercept cross-lagged panel models: Hamaker, E. L., Kuiper, R. M., & Grasman, R. P. P. P. (2015). A critique of the cross-lagged panel model. *Psychological Methods, 20*(1), 102-116. https://doi.org/10.1037/a0038889
- Meta-analytic structural equation modeling: Jak, S., & Cheung, M. W.-L. (2020). Meta-analytic structural equation modeling with moderating effects on SEM parameters. *Psychological Methods, 25*(4), 430-455. https://doi.org/10.1037/met0000245
- SEM/CFA backend: Rosseel, Y. (2012). `lavaan`: An R package for structural equation modeling. *Journal of Statistical Software, 48*(2), 1-36. https://doi.org/10.18637/jss.v048.i02
- Predictability in network models: Haslbeck, J. M. B., & Waldorp, L. J. (2018). How well do network models predict observations? On the importance of predictability in network models. *Behavior Research Methods, 50*, 853-861. https://doi.org/10.3758/s13428-017-0910-x
- Bridge centrality: Jones, P. J., Ma, R., & McNally, R. J. (2021). Bridge centrality: A network approach to understanding comorbidity. *Multivariate Behavioral Research, 56*(2), 353-367. https://doi.org/10.1080/00273171.2019.1614898
- Network comparison testing: van Borkulo, C. D., van Bork, R., Boschloo, L., Kossakowski, J. J., Tio, P., Schoevers, R. A., Borsboom, D., & Waldorp, L. J. (2023). Comparing network structures on three aspects: A permutation test. *Psychological Methods, 28*(6), 1273-1285. https://doi.org/10.1037/met0000476
- Network Intervention Analysis: Blanken, T. F., van der Zweerde, T., van Straten, A., van Someren, E. J. W., Borsboom, D., & Lancee, J. (2019). Introducing Network Intervention Analysis to investigate sequential, symptom-specific treatment effects: A demonstration in co-occurring insomnia and depression. *Psychotherapy and Psychosomatics, 88*(1), 52-54. https://doi.org/10.1159/000495045
- Simulation-based intervention target evaluation: Lunansky, G., Naberman, J., van Borkulo, C. D., Chen, C., Wang, L., & Borsboom, D. (2022). Intervening on psychopathology networks: Evaluating intervention targets through simulations. *Methods, 204*, 29-37. https://doi.org/10.1016/j.ymeth.2021.11.006
- Single-network NIRA workflow, moderation prerequisite, permutation testing, and simulation stability: Wang, F., Wu, Y., Wu, Y., & Zhu, T. (2026). Simulation intervention for cross-sectional network models: Based on the R packages NodeIdentifyR and NIRApost. *Advances in Methods and Practices in Psychological Science*. https://doi.org/10.1177/25152459261452944
- Literature-compatible Ising simulation engine: Epskamp, S. (2026). *IsingSampler: Sampling Methods and Distribution Functions for the Ising Model* (R package version 0.5.0). https://doi.org/10.32614/CRAN.package.IsingSampler
- Interpreting centrality cautiously: Bringmann, L. F., Elmer, T., Epskamp, S., Krause, R. W., Schoch, D., Wichers, M., Wigman, J. T. W., & Snippe, E. (2019). What do centrality measures measure in psychological networks? *Journal of Abnormal Psychology, 128*(8), 892-903. https://doi.org/10.1037/abn0000446

## Changelog

### Development version

- Introduced the unified `quicknet_fit` object for cross-sectional and longitudinal network models.
- Extended cross-sectional support to EBICglasso, correlation, partial correlation, Ising, ordinal, and MGM networks through a consistent `quickNet()` interface.
- Added longitudinal modeling interfaces: `PanelNet()` for cross-lagged panel networks and `LongitudinalNet()` for `graphicalVAR` and `mlVAR` models.
- Added model-agnostic edge tables, node tables, network summaries, centrality helpers, and stability summaries.
- Added virtual perturbation and intervention simulation helpers for Gaussian-style networks and Ising threshold perturbation.
- Added a formal `NIRA()` module with moderation gating, literature-compatible
  Ising simulation, adjusted permutation tests, Monte Carlo rank stability,
  S3 summaries and plots, reports, input metadata, and reproducible PSOCK
  parallelism.
- Added signed MGM moderation summaries, explicit failed-repetition records,
  finite-iteration sensitivity controls, numerical-overflow safeguards, and
  executable nodeIdentifyR/NIRApost compatibility validation.
- Added literature-aligned perturbation plotting helpers for ranking, dosage response, node-level change, edge blocking, and greedy sequence summaries.
- Added `NetworkPower()` / `SampleSize()` for simulation-based network sample size planning.
- Added confirmatory, latent, SEM-panel, mixed VAR, and time-varying mixed VAR network wrappers.
- Added psychonetrics-backed panel and longitudinal models: `PanelNet(model = "ri_clpm")`, `PanelNet(model = "panel_gvar")`, `PanelNet(model = "panel_var")`, and `LongitudinalNet(model = "psychonetrics_gvar")`.
- Added psychonetrics latent/residual network models through `LatentNet(model = "lvm")`, `"lnm"`, `"rnm"`, and `"lrnm"`.
- Added psychonetrics meta-analytic network models through `MetaNet(model = "meta_ggm")`, `"meta_cor"`, and `"meta_gvar"`.
- Extended `ConfirmatoryNet()` to psychonetrics confirmatory Ising, correlation, covariance, and precision-matrix models.
- Added model-specific input requirement helpers and automatic input validation for main fitting functions.
- Added essential method references for supported network models, stability, bridge centrality, network comparison, and perturbation analyses.
- Updated legacy helper functions so they work with the new `quicknet_fit` object.
- Added testthat coverage for cross-sectional and longitudinal workflows.
- Cleaned package documentation, examples, spelling, and R CMD check issues.
