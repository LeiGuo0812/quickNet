![QuickNet](https://github.com/LeiGuo0812/quickNet/assets/50766698/240de5bc-e4a9-41ef-b04e-97e4e6b9878b)

# quickNet

[English version](README.md)

`quickNet` 提供一组快速估计、绘制、汇总和比较心理网络的 R 接口。当前版本统一返回 `quicknet_fit` 对象，旧接口名称仍可使用，同时支持横断面网络、横滞后面板网络和密集纵向网络。

## 安装

### 在线安装

```r
if (!require(devtools)) {
  install.packages("devtools")
}

devtools::install_github("LeiGuo0812/quickNet")
```

### 本地安装

点击 `Code -> Download ZIP` 下载源码压缩包，并将 R 工作目录切换到压缩包所在位置。

```r
if (!require(devtools)) {
  install.packages("devtools")
}

devtools::install_local("quickNet-main.zip")
```

部分模型依赖可选后端包：`PanelNet()` 需要 `glmnet`，`LongitudinalNet(model = "graphicalVAR")` 需要 `graphicalVAR`，`LongitudinalNet(model = "mlVAR")` 需要 `mlVAR`。

## 统一输出对象

主要建模函数返回 `quicknet_fit` 对象。常用字段包括：

```r
fit$graph             # 默认网络矩阵
fit$networks          # 一个或多个网络层
fit$edges             # 边表
fit$nodes             # 节点指标表
summary(fit)          # 网络层面的摘要
plot(fit)             # 快速绘图
```

## 可用模型

| 数据类型 | 函数 | 模型名称 | 说明 |
| --- | --- | --- | --- |
| 横断面连续数据 | `quickNet()` | `"EBICglasso"` | 基于 EBICglasso 的高斯图模型，默认模型 |
| 横断面连续数据 | `quickNet()` | `"correlation"` | 相关网络 |
| 横断面连续数据 | `quickNet()` | `"partial"` | 偏相关网络 |
| 横断面二分类数据 | `quickNet()` | `"ising"` | Ising 网络 |
| 横断面有序分类数据 | `quickNet()` | `"ordinal"` | 多分相关/有序变量网络 |
| 横断面混合数据 | `quickNet()` | `"mgm"` | Mixed Graphical Model |
| 宽格式面板数据 | `PanelNet()` | `"clpn"` | 横滞后面板网络，返回有向网络 |
| 长格式密集纵向数据 | `LongitudinalNet()` | `"graphicalVAR"` | 基于 `graphicalVAR::mlGraphicalVAR()` 的 temporal、contemporaneous、between 网络 |
| 长格式密集纵向数据 | `LongitudinalNet()` | `"mlVAR"` | 基于 `mlVAR::mlVAR()` 的 temporal、contemporaneous、between 网络 |

`EBICglassoNet()` 是保留的便捷接口，等价于估计 `model = "EBICglasso"` 的横断面网络。

## 最小使用示例

### 1. EBICglasso 横断面网络

```r
library(quickNet)

fit <- quickNet(mtcars[, 1:6], model = "EBICglasso", pie = FALSE)

summary(fit)
fit$edges
plot(fit)
```

也可以使用旧接口名称：

```r
fit <- EBICglassoNet(mtcars[, 1:6])
```

### 2. 相关网络

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

### 3. 偏相关网络

```r
fit <- quickNet(
  mtcars[, 1:6],
  model = "partial",
  cor_method = "pearson",
  pie = FALSE
)

summary(fit)
```

### 4. Ising 二分类网络

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

### 5. 有序分类网络

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

### 6. 混合图模型 MGM

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

## 纵向网络示例

### 7. 横滞后面板网络 CLPN

`PanelNet()` 使用宽格式数据。列名默认格式为 `节点名_t波次`，例如 `x1_t1`、`x1_t2`。

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

panel_fit$networks$default       # 包含自回归和横滞后路径
panel_fit$networks$cross_lagged  # 仅横滞后路径
panel_fit$edges
```

### 8. graphicalVAR 密集纵向网络

`LongitudinalNet()` 使用长格式数据，需要个体 ID、天数/日期变量和测量时点变量。

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

### 9. mlVAR 密集纵向网络

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

## 常用后续分析

### 中心性和桥接中心性

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

### 稳定性分析

横断面网络：

```r
fit <- quickNet(mtcars[, 1:6], model = "correlation", pie = FALSE)
stability <- Stability(fit, nboot = 100)

stability$edge_bootstrap_stability
stability$case_drop_centrality_stability
```

纵向网络：

```r
longitudinal_stability <- LongitudinalStability(panel_fit, nboot = 100)
```

### 网络比较

```r
net1 <- quickNet(mtcars[, 1:6], pie = FALSE)
net2 <- quickNet((mtcars[, 1:6])^2, pie = FALSE)

comparison <- NetCompare(mtcars[, 1:6], (mtcars[, 1:6])^2, it = 100)
plots <- get_compare_plot(comparison, net1, output = FALSE)
```

### 导出图和表

```r
fit <- quickNet(mtcars[, 1:6], pie = FALSE)

get_network_plot(fit, path = tempdir(), prefix = "example")
get_edges_df(fit)
globalCoeff(fit)
```

## 版本更新

### 开发版本

- 新增统一的 `quicknet_fit` 对象，用于横断面和纵向网络模型。
- 扩展横断面模型支持，通过一致的 `quickNet()` 接口支持 EBICglasso、相关网络、偏相关网络、Ising、有序分类网络和 MGM。
- 新增纵向网络接口：`PanelNet()` 用于横滞后面板网络，`LongitudinalNet()` 用于 `graphicalVAR` 和 `mlVAR` 模型。
- 新增模型通用的边表、节点表、网络摘要、中心性辅助结果和稳定性汇总。
- 更新旧辅助函数，使其兼容新的 `quicknet_fit` 对象。
- 添加横断面和纵向网络工作流的 testthat 测试。
- 清理包文档、示例、拼写问题和 R CMD check 问题。
