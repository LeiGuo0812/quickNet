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

部分模型依赖可选后端包：`PanelNet(model = "clpn")` 需要 `glmnet`，`PanelNet(model = "ri_clpm" / "panel_gvar" / "panel_var")` 需要 `psychonetrics`，`LongitudinalNet(model = "graphicalVAR")` 需要 `graphicalVAR`，`LongitudinalNet(model = "mlVAR")` 需要 `mlVAR`，`LongitudinalNet(model = "psychonetrics_gvar")` 需要 `psychonetrics`。
其他扩展模块会使用可选后端包：`powerly`、`psychonetrics`、`lavaan` 和 `MASS` 分别用于样本量规划、验证性网络、潜变量/残差网络和 SEM 面板网络。

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

使用 `model_registry()` 可以查看包级模型注册表，包括模型家族、后端、
分析类型、网络层、可报告结果、关键参考文献和已知限制。

```r
model_registry()
model_registry("ConfirmatoryNet")
```

`quicknet_report(fit)` 返回面向学术汇报的结果表。对于基于
psychonetrics 的模型，报告会在常规样本、网络、边和节点摘要之外，
额外包含 `fit_indices`、`parameters`、`modification_indices` 和
`constraints`。

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
| 宽格式面板数据 | `PanelNet()` | `"ri_clpm"` | 基于 `psychonetrics` 的随机截距横滞后面板模型，用于区分个体内动态和稳定个体间差异 |
| 宽格式面板数据 | `PanelNet()` | `"panel_gvar"` | 基于 `psychonetrics::panelgvar()` 的 panel graphical VAR，返回 temporal、within 和 between 网络层 |
| 宽格式面板数据 | `PanelNet()` | `"panel_var"` | 基于 `psychonetrics::panelvar()` 的 panel VAR，返回 temporal 和协方差型 within/between 网络层 |
| 宽格式面板数据 | `PanelSEMNet()` | `"panel_sem"` | 基于 lavaan 的 SEM 横滞后面板网络，并返回模型拟合指标 |
| 长格式密集纵向数据 | `LongitudinalNet()` | `"graphicalVAR"` | 基于 `graphicalVAR::mlGraphicalVAR()` 的 temporal、contemporaneous、between 网络 |
| 长格式密集纵向数据 | `LongitudinalNet()` | `"mlVAR"` | 基于 `mlVAR::mlVAR()` 的 temporal、contemporaneous、between 网络 |
| 长格式密集纵向数据 | `LongitudinalNet()` | `"psychonetrics_gvar"` | 基于 `psychonetrics::gvar()` 的 lag-1 graphical VAR，返回 temporal 和 contemporaneous 网络 |
| 时间序列混合数据 | `MixedVARNet()` | `"mixedVAR"` | 适用于连续和分类变量混合时间序列的 mixed VAR 网络 |
| 时间序列混合数据 | `TimeVaryingNet()` | `"time_varying_mvar"` | 在用户指定时间点估计 time-varying mixed VAR 网络 |
| 横断面连续数据 | `ConfirmatoryNet()` | `"confirmatory_ggm"` | 用户指定自由边/固定零边的验证性高斯图模型 |
| CFA/SEM 数据 | `LatentNet()` | `"latent_network"` | CFA 后的潜变量相关网络和可选项目残差网络 |
| CFA/SEM 数据 | `LatentNet()` | `"lvm"` | psychonetrics 潜变量模型，返回潜变量和残差协方差层 |
| CFA/SEM 数据 | `LatentNet()` | `"lnm"` | psychonetrics latent network model，潜变量网络以 GGM 估计 |
| CFA/SEM 数据 | `LatentNet()` | `"rnm"` | psychonetrics residual network model，项目残差网络以 GGM 估计 |
| CFA/SEM 数据 | `LatentNet()` | `"lrnm"` | psychonetrics latent-and-residual network model，同时返回潜变量和残差 GGM 层 |
| 元分析相关/协方差数据 | `MetaNet()` | `"meta_ggm"` | 基于 psychonetrics 的元分析高斯图模型，可使用多个研究的相关/协方差矩阵或研究层原始数据 |
| 元分析相关/协方差数据 | `MetaNet()` | `"meta_cor"` | 基于 psychonetrics 的元分析合并相关网络 |
| 元分析密集纵向数据 | `MetaNet()` | `"meta_gvar"` | 基于 psychonetrics 的单阶段元分析 GVAR，返回 temporal 和 contemporaneous 网络层 |
| 验证性横断面数据 | `ConfirmatoryNet()` | `"confirmatory_ising"` | 基于 psychonetrics 的验证性 Ising 模型，适用于二分类变量 |
| 验证性横断面数据 | `ConfirmatoryNet()` | `"confirmatory_cor"` | 基于 psychonetrics 的验证性相关模型 |
| 验证性横断面数据 | `ConfirmatoryNet()` | `"confirmatory_covariance"` | 基于 psychonetrics 的验证性协方差模型 |
| 验证性横断面数据 | `ConfirmatoryNet()` | `"confirmatory_precision"` | 基于 psychonetrics 的验证性精度矩阵模型 |

样本量规划使用单独的 `quicknet_power` 对象，由 `NetworkPower()` 或别名 `SampleSize()` 返回。

`EBICglassoNet()` 是保留的便捷接口，等价于估计 `model = "EBICglasso"` 的横断面网络。

在论文或学术汇报中使用本包时，应根据实际使用的模型和分析类型引用相应方法文献。必要参考文献见 [参考文献](#参考文献)。

## 输入格式要求

每个模型都有明确的输入检查。可以使用 `input_requirements()` 查看某个模型的输入格式，也可以在拟合前用 `check_input()` 诊断数据。

```r
input_requirements("ising")

check_input(binary_data, model = "ising")
check_input(panel_data, model = "clpn", nodes = c("x1", "x2", "x3"), waves = 1:3)
check_input(esm_data, model = "graphicalVAR", vars = c("x1", "x2", "x3"))
```

主要建模函数内部也会调用同一套校验器。明确的格式错误会提前停止；样本量过小、二分类变量极度不平衡等风险情况会以 warning 提醒。

## 模型参数与源软件默认值

参数直接写在函数调用中，不需要构建参数对象：

```r
fit <- quickNet(
  mixed_data, model = "mgm", types = c("g", "g", "c", "c"),
  levels = c(1, 1, 2, 2), lambdaSel = "EBIC", ruleReg = "OR", gamma = 0.25,
  pie = FALSE
)
fit <- EBICglassoNet(data, nlambda = 50, missing = "pairwise")
```

额外的建模参数通过 `...` 传给所选后端，未指定的参数继承后端默认值。
`quickNet()` 中同时属于估计器和绘图的参数（如 `threshold`）优先用于估计；
绘图设置可在 `plot(fit, threshold = ...)` 中指定。后端不支持的参数会报错。

- EBICglasso 继承 bootnet 的相关估计和缺失处理；显式的 `cor_method` 会传到估计器。
  相关网络默认使用 Pearson 相关；缺失处理需明确指定。有序网络保留 psych 的平滑与类别数限制。
- MGM 必须指定 `types` 和 `levels`。Mixed VAR、时变 VAR 必须指定 `lags`；
  时变 VAR 还必须指定 `estpoints` 和 `bandwidth`，这些参数没有源软件默认值。
- mlVAR 继承 `default` 估计器与效应结构；graphicalVAR 默认估计个体网络。
  `day`、`beep` 可省略。psychonetrics 默认不标准化数据，RI-CLPM 默认使用协方差结构。
- CLPN 的 `standardize` 控制 glmnet 内部标准化；`standardize_data = FALSE`
  控制可选的逐波预处理。按受试者分组的 CV 和相邻波次合并属于该面板方法的规则。
- 验证性与潜变量模型继承对应后端的估计器、缺失处理和识别设置。
  `LatentNet(..., estimator = "MLR")` 可直接配置 lavaan；其 `std.lv` 默认为 FALSE。
- Powerly 默认使用 sensitivity、30 个样本量点、30 次重复和 10000 次 bootstrap；
  必须指定 `nodes`、`density`、`range_lower`、`range_upper`。Monte Carlo 分支有单独记录的模拟设计。
- `NetCompare()` 默认 100 次置换，逐边和中心性检验默认关闭；
  `Bridge()` 默认不归一化，`netCor()` 默认 999 次置换且不绘图。

实际设置、后端版本和方法预设分别记录在 `fit$meta$backend_settings`、
`backend_version`、`method_presets` 中。稳定性和网络比较保留原有估计参数；
无法自动对应到重抽样行的权重或折分参数会明确报错。
NIRA 和 SymPerturb 使用各自方法要求的预设，详见对应章节。
完整核查范围和来源见 [参数核查记录](docs/backend-parameter-audit.md)。

## EBIC 参数设置

`gamma = NULL` 按模型解析 EBIC 超参数。显式指定 [0,1] 内的数值时优先使用
该值，其中 0 表示 BIC。

| 模型／函数 | 实际默认 gamma |
|---|---:|
| `quickNet(model = "EBICglasso")`、`EBICglassoNet()` | 0.5 |
| `quickNet(model = "ising")`；采用 `lambdaSel = "EBIC"` 的 MGM | 0.25 |
| `LongitudinalNet(model = "graphicalVAR")` | 0.5 |
| `MixedVARNet()`／`TimeVaryingNet()`，且 `lambdaSel = "EBIC"` | 0.25 |
| `NetworkPower(method = "monte_carlo", estimator = "EBICglasso")` | 0.5 |
| correlation、partial、ordinal、mlVAR，或 `lambdaSel = "CV"` 的 mixed VAR | 不适用 |

MGM 和 Mixed VAR 继承源软件的 CV 默认值；MGM 使用 AND 规则。时变 VAR
默认使用 EBIC。使用 `lambdaSel = "EBIC"` 时，MGM 家族的默认 gamma 为 0.25。
采用 CV 或不使用 EBIC 选模时，gamma 不参与估计，`fit$meta$gamma` 为 `NULL`，
报告不展示该参数；Monte Carlo 功效结果行中的无效 gamma 记为 `NA`。
Powerly 的参数直接写入函数调用，例如 `samples = 30, boots = 10000`。
其内部 GGM 估计器的 gamma 默认值为 0.5，单独记录为 `backend_gamma`；
顶层 `gamma` 参数不改变该内部设置。当前 ordinal 接口估计关联网络，
不进行 EBIC 选模。验证性模型、潜变量模型和元分析接口也不使用此 EBIC 参数。

`Stability(raw_data, model = ...)` 与 `quickNet()` 使用相同的模型默认值。
`Stability(fit)` 保留拟合时的实际设置。旧对象缺少元数据时，可从后端拟合结果
恢复 gamma；若无法确定原始值，需先重新拟合原始数据，再进行重抽样。

`NetCompare(data1, data2, binary.data = TRUE)` 默认使用 0.25；原始高斯数据
比较默认使用 0.5。`NetCompare(fit1, fit2)` 支持两个探索性横断面
`quicknet_fit` 对象，保留两者一致的估计设置，拒绝冲突的 gamma 覆盖值。
Bootnet 对象沿用其估计参数；自定义估计器通过 `estimatorArgs` 配置。
比较结果的实际 gamma 存储于 `result$info$call$gamma`；自定义估计器中不适用
或无法确定的 gamma 记为 `NULL`。

NIRA 分别记录 Ising 建网 gamma 和调节分析的 `moderation_lambda = 0.25`，
后者传给 MGM 的 EBIC `lambdaGam`。SymPerturb 的 `propagation_gamma = 0.45`
控制传播衰减，是另一个参数。

## 最小使用示例

以下代码块从示例 1 开始按顺序运行；后面的示例会复用前面生成的数据和拟合对象。安装好所有示例所需依赖后，即可从干净 R 会话执行。随机模拟使用固定种子。为便于核对完整流程，示例显式使用较小的重复次数和调参网格；这些演示预算不能支持正式的显著性、稳定性或功效结论，也不修改包的默认值。正式研究应独立确定并验证所需预算。执行记录见 [工作流核验](docs/workflow-validation.md)。

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
ising_graph <- matrix(0, 4, 4)
ising_graph[cbind(1:3, 2:4)] <- 0.6
ising_graph <- ising_graph + t(ising_graph)
binary_data <- as.data.frame(IsingSampler::IsingSampler(
  n = 300, graph = ising_graph, thresholds = c(-0.8, -0.4, -0.2, -0.6)
))
names(binary_data) <- paste0("x", 1:4)

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
  lambdaSel = "EBIC",
  gamma = 0.25,
  pie = FALSE
)

fit$nodes
```

## 纵向网络示例

### 7. 横滞后面板网络 CLPN

`PanelNet()` 使用宽格式数据。列名默认格式为 `节点名_t波次`，例如 `x1_t1`、`x1_t2`。

```r
set.seed(12)
n <- 300
panel_data <- data.frame(id = seq_len(n))
intercepts <- matrix(rnorm(n * 3, sd = 0.7), n, 3)
state <- matrix(rnorm(n * 3), n, 3)
for (wave in 1:3) {
  if (wave > 1) state <- cbind(0.3 * state[, 1],
    0.4 * state[, 1] + 0.2 * state[, 2], 0.3 * state[, 3]) + matrix(rnorm(n * 3), n, 3)
  for (node in 1:3) panel_data[[paste0("x", node, "_t", wave)]] <- intercepts[, node] + state[, node]
}

panel_fit <- PanelNet(
  panel_data,
  nodes = c("x1", "x2", "x3"),
  waves = 1:3,
  id = "id",
  nfolds = 5, seed = 12
)

panel_fit$networks$default       # 包含自回归和横滞后路径
panel_fit$networks$cross_lagged  # 仅横滞后路径
panel_fit$edges
```

### 8. psychonetrics 面板模型

相同的宽格式面板数据也可以用于随机截距 CLPM 和 panel GVAR。

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

### 9. graphicalVAR 密集纵向网络

`LongitudinalNet()` 使用长格式数据，需要个体 ID。`day` 和 `beep` 可选，提供时用于确定日期边界和测量顺序。各后端的处理方式见[数据与时间核验](docs/data-time-validation.md)。

```r
set.seed(13)
simulate_esm <- function(cross_lag = 0.4, autoregressive = 0.3) do.call(rbind, lapply(1:8, function(person) {
  person_mean <- rnorm(3, sd = 0.7)
  do.call(rbind, lapply(1:3, function(day) {
    state <- matrix(0, 70, 3)
    for (i in 2:70) state[i, ] <- c(autoregressive * state[i - 1, 1],
      cross_lag * state[i - 1, 1] + 0.2 * state[i - 1, 2],
      autoregressive * state[i - 1, 3]) + rnorm(3)
    values <- sweep(state[41:70, ], 2, person_mean, "+")
    data.frame(id = person, day = day, beep = 1:30,
      x1 = values[, 1], x2 = values[, 2], x3 = values[, 3])
  }))
}))
esm_data <- simulate_esm()

gvar_fit <- LongitudinalNet(
  esm_data,
  vars = c("x1", "x2", "x3"),
  id = "id",
  day = "day",
  beep = "beep",
  model = "graphicalVAR", nLambda = 5, subjectNetworks = FALSE
)

gvar_fit$networks$temporal
gvar_fit$networks$contemporaneous
gvar_fit$networks$between
```

### 10. psychonetrics GVAR 密集纵向网络

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

### 11. mlVAR 密集纵向网络

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

### 12. 网络统计功效和样本量规划

```r
power <- NetworkPower(
  nodes = 8,
  density = 0.30,
  sample_sizes = c(100, 200, 400), replications = 5, seed = 14,
  target_metric = "mcc",
  target_value = 0.60,
  target_probability = 0.80
)

summary(power)
plot(power)
quicknet_report(power)$text
```

如果 `sample_sizes = NULL`，`NetworkPower()` 会根据节点数自动生成候选样本量网格。
Monte Carlo 分支的默认目标指标为 `mcc`，同时考虑假阴性和假阳性。

Monte Carlo 达标概率以一次调用中固定的生成网络为条件。
`estimator = "correlation"` 的 `true_network` 为总体边际相关矩阵，其他估计器的
真值为偏相关网络；`generating_network` 保留生成数据所用的偏相关网络。
设置记录实际密度、边强度及保证精度矩阵正定所用的缩放系数。汇总表提供 Monte Carlo
标准误与逐点精确二项 95% 区间；拟合失败和目标指标无定义分别计数，均计入未达标次数。

推荐 N 是点估计达标的最小候选样本量；没有候选值达标时，`recommended_n` 为 `NA`。
结果同时提供边界标记和 `lower_bound_supports_target`。独立验证及区间解释见
[功效核验记录](docs/network-power-validation.md)。

也可以使用 `powerly` 后端进行 GGM 样本量规划：

```r
powerly_plan <- NetworkPower(
  method = "powerly",
  nodes = 8,
  density = 0.30,
  range_lower = 100,
  range_upper = 500,
  samples = 5, replications = 5, boots = 20, iterations = 1,
  cores = 1, verbose = FALSE, seed = 15,
  target_metric = "sensitivity",
  target_value = 0.60,
  target_probability = 0.80
)
```

Powerly 推荐值按其 bootstrap 中位曲线解释，保留源软件的数据生成设置，默认生成
五级序数数据。已有偏相关真值矩阵时，可直接传入 `model_matrix = ...`，无需另给
`nodes` 和 `density`。使用 `powerly::validate(powerly_plan$fit)` 进行源软件提供的
独立验证。已测试设计及适用范围见[功效核验记录](docs/network-power-validation.md)。

保存对象与运行环境的核验分别见[历史对象核验](docs/legacy-object-validation.md)和
[平台兼容性记录](docs/platform-compatibility-validation.md)。报告依据保存的后端证据
恢复历史设置；重拟合需要足够证据来保留原始估计方法。

### 13. 验证性、潜变量和动态网络

```r
set.seed(16)
factors <- matrix(rnorm(800), 400, 2) %*% chol(matrix(c(1, 0.4, 0.4, 1), 2))
continuous_data <- as.data.frame(sapply(1:6, function(j)
  0.8 * factors[, if (j <= 3) 1 else 2] + rnorm(400, sd = 0.6)))
names(continuous_data) <- paste0("x", 1:6)

omega <- matrix(1, 6, 6)
diag(omega) <- 0
colnames(omega) <- rownames(omega) <- paste0("x", 1:6)

confirmatory <- ConfirmatoryNet(continuous_data, vars = paste0("x", 1:6), omega = omega)

confirmatory_cor <- ConfirmatoryNet(continuous_data, vars = paste0("x", 1:6), model = "cor")
confirmatory_precision <- ConfirmatoryNet(continuous_data, vars = paste0("x", 1:6), model = "precision")
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

latent_data <- setNames(continuous_data, c("d1", "d2", "d3", "a1", "a2", "a3"))
latent <- LatentNet(latent_data, model = cfa_model)
latent$networks$latent
latent$networks$residual
```

```r
lambda <- matrix(0, 6, 2, dimnames = list(paste0("x", 1:6), c("Depression", "Anxiety")))
lambda[1:3, "Depression"] <- 1
lambda[4:6, "Anxiety"] <- 1

lnm <- LatentNet(continuous_data, model = "lnm", vars = paste0("x", 1:6), lambda = lambda)
lrnm <- LatentNet(continuous_data, model = "lrnm", vars = paste0("x", 1:6), lambda = lambda)

lnm$networks$latent
lrnm$networks$residual
```

```r
panel_sem <- PanelSEMNet(panel_data, nodes = c("x1", "x2", "x3"), waves = 1:3)

set.seed(17)
time_data <- data.frame(x1 = as.numeric(arima.sim(list(ar = 0.3), n = 250)),
  x2 = as.numeric(arima.sim(list(ar = -0.2), n = 250)),
  x3 = sample(1:2, 250, replace = TRUE))

mixed_var <- MixedVARNet(
  time_data,
  types = c("g", "g", "c"),
  levels = c(1, 1, 2),
  lags = 1
)

tv_mvar <- TimeVaryingNet(
  time_data,
  types = c("g", "g", "c"),
  levels = c(1, 1, 2),
  lags = 1,
  estpoints = c(0.25, 0.50, 0.75),
  bandwidth = 0.20
)
```

### 14. 元分析网络

`MetaNet()` 可基于多个研究的相关/协方差矩阵或多研究原始数据估计 psychonetrics 元分析网络模型。

```r
set.seed(18)
nobs <- c(150, 180, 220, 160, 190, 210)
population_cor <- matrix(c(1, 0.3, 0.1, 0.3, 1, 0.2, 0.1, 0.2, 1), 3)
cors <- lapply(nobs, function(n) {
  values <- matrix(rnorm(n * 3), n, 3) %*% chol(population_cor)
  colnames(values) <- c("x1", "x2", "x3")
  cor(values)
})

meta_ggm <- MetaNet(
  cors = cors,
  nobs = nobs,
  vars = c("x1", "x2", "x3"),
  model = "meta_ggm"
)

meta_ggm$networks$default
quicknet_report(meta_ggm)$sample
```

对于多研究密集纵向数据： 此处用源软件参数 `lowertri_randomEffects = "diag"` 将随机效应 Cholesky 非对角元素固定为零，作为这个小型演示的模型设定。

```r
set.seed(19)
multi_study_esm <- do.call(rbind, lapply(1:20, function(study) {
  values <- simulate_esm(cross_lag = runif(1, 0.15, 0.55),
                         autoregressive = runif(1, 0.15, 0.45))
  values$study <- study
  values
}))
meta_gvar <- MetaNet(
  data = multi_study_esm,
  studyvar = "study",
  vars = c("x1", "x2"),
  id = "id",
  day = "day",
  beep = "beep",
  model = "meta_gvar", lowertri_randomEffects = "diag"
)

meta_gvar$networks$temporal
meta_gvar$networks$contemporaneous
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
set.seed(20)
stability <- Stability(fit, nboot = 5)

stability$edge_bootstrap_stability
stability$case_drop_centrality_stability
```

纵向网络：

```r
longitudinal_stability <- LongitudinalStability(panel_fit, nboot = 5, seed = 20)
```

### 学术汇报参数

可以使用 `quicknet_report()` 从任意 `quicknet_fit` 对象中提取适合论文或学术汇报的样本信息、估计设置、网络摘要、边摘要、节点指标和模型特异参数。

```r
fit <- quickNet(mtcars[, 1:6], model = "EBICglasso", pie = FALSE)
report <- quicknet_report(fit)

report$sample          # 样本量和节点数
report$estimation      # 估计器和调参信息
report$networks        # 各网络层的密度和边权摘要
report$edges           # 正边、负边和非零边数量
report$nodes           # 节点中心性和预测性
report$model_specific  # 模型特异汇报字段
report$text            # 简短文字摘要
```

### 虚拟扰动与干预模拟

可以使用 `Perturbation()` 进行模型内的 in silico 虚拟扰动分析。这类结果适合用于假设生成和候选靶点筛选，但不应解释为因果干预效应。

```r
fit <- quickNet(mtcars[, 1:6], model = "partial", pie = FALSE)

dosage <- Perturbation(
  fit,
  method = "dosage",
  targets = c("mpg", "cyl"),
  dose = c(0.25, 0.50, 1.00),
  config = list(bounds = NULL)
)

dosage$metrics
dosage$rankings
quicknet_report(dosage)$text

plot(dosage)
get_perturbation_plot(dosage, type = "rank")
get_perturbation_plot(dosage, type = "dose_response")
get_perturbation_plot(dosage, type = "node_change", perturbation_id = 1)
```

连续干预按 SymPerturb 0.1.0 修订规范实现。算法从 `fit$data` 估计原始均值和加岭正则的协方差。拓扑边阈值与状态协方差分别处理。量表边界默认为 `[0,4]`；本例 `mtcars` 不使用该量表边界，因此设置 `bounds = NULL`。

输入须保留至少 3 名参与者、3 个数值型症状变量，且不含缺失值或无穷值。`symperturb` 和 `sequence` 都需要覆盖全部症状、至少包含两个模块的命名向量 `modules`，以使用相同的评分和候选表。原拟合对象中的字符型／因子型 `groups` 向量也可提供模块映射。

```r
cfg <- list(bounds = NULL)
Perturbation(fit, "knockout", targets = "mpg", config = cfg)
Perturbation(fit, "knockdown", targets = "mpg", dose = 0.50, config = cfg)
blocked <- Perturbation(fit, "edge_block", targets = "mpg", config = cfg)
Perturbation(fit, "node_block", targets = "mpg", config = cfg)
Perturbation(fit, "combination", targets = c("mpg", "cyl", "disp"), config = cfg)
modules <- c(mpg = "performance", cyl = "engine", disp = "engine",
             hp = "engine", drat = "performance", wt = "performance")
sequence <- Perturbation(fit, "sequence", targets = c("mpg", "cyl", "disp"),
                         steps = 2, modules = modules, config = cfg)
get_perturbation_plot(blocked, "edge_block")
get_perturbation_plot(sequence, "sequence")

result <- Perturbation(fit, "symperturb", modules = modules, seed = 20,
  config = list(bounds = NULL, sequence_length = 2,
                bootstrap_replicates = 5, bootstrap_top_k = 2))
result$target_scores    # 七维原始效用、标准化效用、VPPS 和排名
result$pair_scores     # 相对于较优单靶点的有符号增量
result$scenario_ranks  # 13 个敏感性场景；稳健性不计入 VPPS
result$bootstrap       # 全流程区间和进入前 k 名的概率
result$sequence        # 束搜索保留的最终序列及其目标函数值
quicknet_report(result)
```

状态干预同时更新均值与协方差。敲降参数 `remaining_strength` 表示目标位置／尺度的保留比例，`knockout` 执行单位剂量的状态干预。结果按 `system_benefit`（非目标加权标准化改善）排序；`burden_reduction` 描述观测量纲上的总变化。

组合指标 `incremental_pair_value` 在共同非目标集合上，计算单位剂量联合改善减去较优单靶点改善。边／节点阻断以 `communication_block` 报告有限步传播量的相对损失。序列采用束搜索，优化带折扣的边际获益减去成本。完整参数及返回字段见 `?Perturbation`。

七维 VPPS 包括 efficacy、dose efficiency、breadth、cross-module、communication block、combination value、responsiveness；稳健性单独报告。每次 bootstrap 都重新估计网络、计算效用并在候选集合内标准化和排名。

R 实现不依赖 Python 运行环境。数值回归数据由本地 Python 参考包生成。bootstrap 使用 R 的随机数发生器；跨语言逐值比较时应传入相同的 `bootstrap_indices`（从 1 开始），不能假定相同整数种子产生相同样本。

验证结果：七组参考配置及 Python 包提供的 12 节点示例均在数值容差内一致。示例的七张结果表最大绝对差约为 `6.7e-13`，其中包含 25 次共享索引的 bootstrap。在此前 SymPerturb 专项核验对应的版本中，全包 1,552 项测试断言通过，`R CMD check --no-manual` 为 0 errors、0 warnings、0 notes；这些数字是该次历史核验的记录，并非当前全包测试总数。检验范围、接口说明及复现命令见[算法与验证记录](docs/symperturb-validation.md)。

参考文献：Zhu, Z., Yu, J., Hu, T., Yang, Z., & Wang, J. (2026). *SymPerturb converts symptom-network structure into testable intervention priorities*. arXiv:2607.28673v1；采用修订方法规范及 SymPerturb 0.1.0。

对于 Ising 模型，`ising_threshold` 提供轻量的单链阈值敏感性分析：

```r
ising_fit <- quickNet(binary_data, model = "ising", gamma = 0.25, pie = FALSE)

ising_result <- Perturbation(
  ising_fit,
  method = "ising_threshold",
  targets = c("x1", "x2"),
  threshold_shift = -0.5
)

get_perturbation_plot(ising_result, type = "rank")
get_perturbation_plot(ising_result, type = "node_change", target = "x1")
```

若要运行 Wang 等（2026）描述的正式单网络 NIRA 工作流，请使用
`NIRA()`（也可用 `Perturbation(method = "nira")`）。该实现依次完成
moderation prerequisite、原始条件与逐节点阈值干预模拟、多重校正的置换
检验，以及重复模拟的排名稳定性分析：

```r
nira_result <- NIRA(
  ising_fit,
  perturbation_type = "alleviating",
  amount_of_SDs_perturbation = 2,
  n_samples = 100,
  moderation_nboot = 5,
  n_permutations = 99,
  stability_reps = 5,
  parallel = FALSE,
  ncores = 1,
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

打印结果时，会在普通说明句中交代实际 EBIC `gamma`、quickNet 的 Ising 默认值
（0.25）和 Wang 等（2026）的示例值（0.25）。NIRA 结果及
`quicknet_report(nira_result)` 还会在结果文字中说明调节效应统计方式与
Cohen's d 的符号约定，并在末尾列出完整参考文献题录。说明文字和题录
也可通过 `summary(nira_result)$text` 和 `$references` 获取。若需与 Wang 等（2026）表 4
的效应量符号一致，可使用 `-nira_result$interventions$raw_cohen_d`。

正式研究建议至少使用 1000 次 moderation 重抽样和 1000 次稳定性重复。
默认的 literature 引擎使用 IsingSampler；可增大 `engine_iterations`
进行敏感性分析。

若检测到稳定 moderation，默认会阻断 NIRA，因为此时“只改变阈值、所有
边保持固定”的假设缺少支持。设置 `proceed_on_moderation = TRUE` 可在
明确警告下继续。

NIRA 要求横断面、完整的 0/1 数据和有意义的总分，结果为给定拟合网络下的模型模拟。
方法适用范围和源文献信息见 [虚拟干预核验](docs/intervention-reliability-validation.md)。

### 网络比较

```r
net1 <- quickNet(mtcars[, 1:6], pie = FALSE)
net2 <- quickNet((mtcars[, 1:6])^2, pie = FALSE)

set.seed(21)
comparison <- NetCompare(mtcars[, 1:6], (mtcars[, 1:6])^2, it = 9, test.edges = TRUE)
plots <- get_compare_plot(comparison, net1, output = FALSE)
```

`paired = TRUE` 沿用 `NetworkComparisonTest::NCT`；两组第 i 行必须为同一受试者。
方法适用条件和置换来源保存在 `comparison$info$permutation`，
详细来源及验证见 [配对 NCT 核验](docs/paired-nct-validation.md)。

### MTD 时间序列耦合检验

`MTD.No.Smooth.Test()` 的耦合指标使用 Shine 等作者 MATLAB 程序的导数标准化规则，
不做时间平滑。显著性检验默认采用 Yuan 与 Shou（2024）的截断时间位移检验（TTS），
按作者实现要求直接指定截断半径 `radius`：

```r
set.seed(1)
series <- cbind(as.numeric(arima.sim(list(ar = 0.5), n = 160)),
                as.numeric(arima.sim(list(ar = 0.5), n = 160)))
mtd <- MTD.No.Smooth.Test(series, radius = 39)
mtd$p.value
mtd$test_coupling_mean
```

输入须为等间隔、按时间排序的一对完整序列，并事先指定 `radius`。
`coupling_mean` 描述完整序列，`test_coupling_mean` 对应截断区间；
TTS 使用全部位移，不接收 `nperm`。`method = "shuffle", nperm = 999` 选择观测置换。
源方法的平稳性／可交换性要求与验证记录见 [MTD 核验](docs/mtd-inference-validation.md)。

### 导出图和表

```r
fit <- quickNet(mtcars[, 1:6], pie = FALSE)

export_dir <- file.path(tempdir(), "quicknet-example")
dir.create(export_dir, showWarnings = FALSE)
get_network_plot(fit, path = export_dir, prefix = "example")
utils::write.csv(get_edges_df(fit), file.path(export_dir, "edges.csv"), row.names = FALSE)
writeLines(quicknet_report(fit)$text, file.path(export_dir, "report.txt"))
globalCoeff(fit)
list.files(export_dir)
```

## 参考文献

如果在学术工作中使用 `quickNet`，请引用本包，并根据实际使用的模型或辅助函数引用对应方法文献：

- 横断面心理网络估计和可视化：Epskamp, S., Cramer, A. O. J., Waldorp, L. J., Schmittmann, V. D., & Borsboom, D. (2012). `qgraph`: Network visualizations of relationships in psychometric data. *Journal of Statistical Software, 48*(4), 1-18. https://doi.org/10.18637/jss.v048.i04
- 网络估计、准确性和稳定性：Epskamp, S., Borsboom, D., & Fried, E. I. (2018). Estimating psychological networks and their accuracy: A tutorial paper. *Behavior Research Methods, 50*, 195-212. https://doi.org/10.3758/s13428-017-0862-1
- 高斯图模型的 EBIC 模型选择：Foygel, R., & Drton, M. (2010). Extended Bayesian information criteria for Gaussian graphical models. *Advances in Neural Information Processing Systems, 23*. https://proceedings.neurips.cc/paper/2010/hash/072b030ba126b2f4b2374f342be9ed44-Abstract.html
- 面板网络后端使用的正则化广义线性模型：Friedman, J., Hastie, T., & Tibshirani, R. (2010). Regularization paths for generalized linear models via coordinate descent. *Journal of Statistical Software, 33*(1), 1-22. https://doi.org/10.18637/jss.v033.i01
- 二分类 Ising 网络：van Borkulo, C. D., Borsboom, D., Epskamp, S., Blanken, T. F., Boschloo, L., Schoevers, R. A., & Waldorp, L. J. (2014). A new method for constructing networks from binary data. *Scientific Reports, 4*, 5918. https://doi.org/10.1038/srep05918
- 混合图模型：Haslbeck, J. M. B., & Waldorp, L. J. (2020). `mgm`: Estimating time-varying mixed graphical models in high-dimensional data. *Journal of Statistical Software, 93*(8), 1-46. https://doi.org/10.18637/jss.v093.i08
- 横断面和时间序列高斯图模型，包括 graphicalVAR 类模型：Epskamp, S., Waldorp, L. J., Mõttus, R., & Borsboom, D. (2018). The Gaussian graphical model in cross-sectional and time-series data. *Multivariate Behavioral Research, 53*(4), 453-480. https://doi.org/10.1080/00273171.2018.1454823
- 纵向心理病理网络和向量自回归：Bringmann, L. F., Vissers, N., Wichers, M., Geschwind, N., Kuppens, P., Peeters, F., Borsboom, D., & Tuerlinckx, F. (2013). A network approach to psychopathology: New insights into clinical longitudinal data. *PLOS ONE, 8*(4), e60188. https://doi.org/10.1371/journal.pone.0060188
- 网络样本量规划：Constantin, M. A., Schuurman, N. K., & Vermunt, J. K. (2021). A general Monte Carlo method for sample size analysis in the context of network models. https://doi.org/10.31234/osf.io/j5v7u
- 广义网络心理计量和验证性网络模型：Epskamp, S., Rhemtulla, M., & Borsboom, D. (2017). Generalized network psychometrics: Combining network and latent variable models. *Psychometrika, 82*, 904-927. https://doi.org/10.1007/s11336-017-9557-x
- 随机截距横滞后面板模型：Hamaker, E. L., Kuiper, R. M., & Grasman, R. P. P. P. (2015). A critique of the cross-lagged panel model. *Psychological Methods, 20*(1), 102-116. https://doi.org/10.1037/a0038889
- 元分析结构方程模型：Jak, S., & Cheung, M. W.-L. (2020). Meta-analytic structural equation modeling with moderating effects on SEM parameters. *Psychological Methods, 25*(4), 430-455. https://doi.org/10.1037/met0000245
- SEM/CFA 后端：Rosseel, Y. (2012). `lavaan`: An R package for structural equation modeling. *Journal of Statistical Software, 48*(2), 1-36. https://doi.org/10.18637/jss.v048.i02
- 网络模型预测性：Haslbeck, J. M. B., & Waldorp, L. J. (2018). How well do network models predict observations? On the importance of predictability in network models. *Behavior Research Methods, 50*, 853-861. https://doi.org/10.3758/s13428-017-0910-x
- 桥接中心性：Jones, P. J., Ma, R., & McNally, R. J. (2021). Bridge centrality: A network approach to understanding comorbidity. *Multivariate Behavioral Research, 56*(2), 353-367. https://doi.org/10.1080/00273171.2019.1614898
- 网络比较检验：van Borkulo, C. D., van Bork, R., Boschloo, L., Kossakowski, J. J., Tio, P., Schoevers, R. A., Borsboom, D., & Waldorp, L. J. (2023). Comparing network structures on three aspects: A permutation test. *Psychological Methods, 28*(6), 1273-1285. https://doi.org/10.1037/met0000476
- Network Intervention Analysis：Blanken, T. F., van der Zweerde, T., van Straten, A., van Someren, E. J. W., Borsboom, D., & Lancee, J. (2019). Introducing Network Intervention Analysis to investigate sequential, symptom-specific treatment effects: A demonstration in co-occurring insomnia and depression. *Psychotherapy and Psychosomatics, 88*(1), 52-54. https://doi.org/10.1159/000495045
- 基于模拟的干预靶点评估：Lunansky, G., Naberman, J., van Borkulo, C. D., Chen, C., Wang, L., & Borsboom, D. (2022). Intervening on psychopathology networks: Evaluating intervention targets through simulations. *Methods, 204*, 29-37. https://doi.org/10.1016/j.ymeth.2021.11.006
- 单网络 NIRA、moderation prerequisite、置换检验和模拟稳定性：Wang, F., Wu, Y., Wu, Y., & Zhu, T. (2026). Simulation intervention for cross-sectional network models: Based on the R packages NodeIdentifyR and NIRApost. *Advances in Methods and Practices in Psychological Science*. https://doi.org/10.1177/25152459261452944
- 文献兼容 Ising 模拟引擎：Epskamp, S. (2026). *IsingSampler: Sampling Methods and Distribution Functions for the Ising Model*（R package version 0.5.0）。https://doi.org/10.32614/CRAN.package.IsingSampler
- 中心性解释的谨慎边界：Bringmann, L. F., Elmer, T., Epskamp, S., Krause, R. W., Schoch, D., Wichers, M., Wigman, J. T. W., & Snippe, E. (2019). What do centrality measures measure in psychological networks? *Journal of Abnormal Psychology, 128*(8), 892-903. https://doi.org/10.1037/abn0000446

- MTD 耦合指标：Shine, J. M., Koyejo, O., Bell, P. T., Gorgolewski, K. J., Gilat, M., & Poldrack, R. A. (2015). Estimation of dynamic functional connectivity using Multiplication of Temporal Derivatives. *NeuroImage, 122*, 399–407. https://doi.org/10.1016/j.neuroimage.2015.07.064
- 时间序列独立性检验：Yuan, A. E., & Shou, W. (2024). A rigorous and versatile statistical test for correlations between stationary time series. *PLOS Biology, 22*(8), e3002758. https://doi.org/10.1371/journal.pbio.3002758

## 版本更新

### 开发版本

- 新增统一的 `quicknet_fit` 对象，用于横断面和纵向网络模型。
- 扩展横断面模型支持，通过一致的 `quickNet()` 接口支持 EBICglasso、相关网络、偏相关网络、Ising、有序分类网络和 MGM。
- 新增纵向网络接口：`PanelNet()` 用于横滞后面板网络，`LongitudinalNet()` 用于 `graphicalVAR` 和 `mlVAR` 模型。
- 新增模型通用的边表、节点表、网络摘要、中心性辅助结果和稳定性汇总。
- `Perturbation()` 实现 SymPerturb 状态干预、邻接边阻断、有符号组合增量、束搜索序列和七维 VPPS，并单独报告稳健性及 bootstrap 结果。
- `NIRA()` 支持 moderation gate、Ising 阈值模拟、多重校正置换
  检验、Monte Carlo 排名稳定性、绘图和报告。
- 扰动绘图支持排名、剂量响应、节点变化、通信阻断和序列路径。
- 新增 `NetworkPower()` / `SampleSize()`，用于基于模拟的网络样本量规划。
- 新增验证性网络、潜变量网络、SEM 面板网络、mixed VAR 和 time-varying mixed VAR 封装接口。
- 新增 psychonetrics 后端的面板和纵向模型：`PanelNet(model = "ri_clpm")`、`PanelNet(model = "panel_gvar")`、`PanelNet(model = "panel_var")` 和 `LongitudinalNet(model = "psychonetrics_gvar")`。
- 通过 `LatentNet(model = "lvm")`、`"lnm"`、`"rnm"` 和 `"lrnm"` 新增 psychonetrics 潜变量/残差网络模型。
- 通过 `MetaNet(model = "meta_ggm")`、`"meta_cor"` 和 `"meta_gvar"` 新增 psychonetrics 元分析网络模型。
- 扩展 `ConfirmatoryNet()`，新增 psychonetrics 验证性 Ising、相关、协方差和精度矩阵模型。
- 新增模型特异输入格式提醒和主要建模函数的自动输入校验。
- 新增支持模型、稳定性、桥接中心性、网络比较和扰动分析的必要方法参考文献。
- 更新旧辅助函数，使其兼容新的 `quicknet_fit` 对象。
- 添加横断面和纵向网络工作流的 testthat 测试。
- 清理包文档、示例、拼写问题和 R CMD check 问题。
