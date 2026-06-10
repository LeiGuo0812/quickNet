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

Some models require optional backend packages: `PanelNet()` requires `glmnet`, `LongitudinalNet(model = "graphicalVAR")` requires `graphicalVAR`, and `LongitudinalNet(model = "mlVAR")` requires `mlVAR`.

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
| Long-format intensive longitudinal data | `LongitudinalNet()` | `"graphicalVAR"` | Multilevel graphical VAR model returning temporal, contemporaneous, and between-person networks. |
| Long-format intensive longitudinal data | `LongitudinalNet()` | `"mlVAR"` | Multilevel VAR model via `mlVAR`, also returning temporal, contemporaneous, and between-person networks. |

`EBICglassoNet()` is retained as a convenience wrapper for the cross-sectional `model = "EBICglasso"` workflow.

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

### 8. graphicalVAR Intensive Longitudinal Network

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

### 9. mlVAR Intensive Longitudinal Network

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

## Changelog

### Development version

- Introduced the unified `quicknet_fit` object for cross-sectional and longitudinal network models.
- Extended cross-sectional support to EBICglasso, correlation, partial correlation, Ising, ordinal, and MGM networks through a consistent `quickNet()` interface.
- Added longitudinal modeling interfaces: `PanelNet()` for cross-lagged panel networks and `LongitudinalNet()` for `graphicalVAR` and `mlVAR` models.
- Added model-agnostic edge tables, node tables, network summaries, centrality helpers, and stability summaries.
- Updated legacy helper functions so they work with the new `quicknet_fit` object.
- Added testthat coverage for cross-sectional and longitudinal workflows.
- Cleaned package documentation, examples, spelling, and R CMD check issues.
