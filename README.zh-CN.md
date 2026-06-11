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
其他扩展模块会使用可选后端包：`powerly`、`psychonetrics`、`lavaan` 和 `MASS` 分别用于样本量规划、验证性网络、潜变量网络和 SEM 面板网络。

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
| 宽格式面板数据 | `PanelSEMNet()` | `"panel_sem"` | 基于 lavaan 的 SEM 横滞后面板网络，并返回模型拟合指标 |
| 长格式密集纵向数据 | `LongitudinalNet()` | `"graphicalVAR"` | 基于 `graphicalVAR::mlGraphicalVAR()` 的 temporal、contemporaneous、between 网络 |
| 长格式密集纵向数据 | `LongitudinalNet()` | `"mlVAR"` | 基于 `mlVAR::mlVAR()` 的 temporal、contemporaneous、between 网络 |
| 时间序列混合数据 | `MixedVARNet()` | `"mixedVAR"` | 适用于连续和分类变量混合时间序列的 mixed VAR 网络 |
| 时间序列混合数据 | `TimeVaryingNet()` | `"time_varying_mvar"` | 在用户指定时间点估计 time-varying mixed VAR 网络 |
| 横断面连续数据 | `ConfirmatoryNet()` | `"confirmatory_ggm"` | 用户指定自由边/固定零边的验证性高斯图模型 |
| CFA/SEM 数据 | `LatentNet()` | `"latent_network"` | CFA 后的潜变量相关网络和可选项目残差网络 |

样本量规划使用单独的 `quicknet_power` 对象，由 `NetworkPower()` 或别名 `SampleSize()` 返回。

`EBICglassoNet()` 是保留的便捷接口，等价于估计 `model = "EBICglasso"` 的横断面网络。

在论文或学术汇报中使用本包时，应根据实际使用的模型和分析类型引用相应方法文献。必要参考文献见 [参考文献](#参考文献)。

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

### 10. 网络统计功效和样本量规划

```r
power <- NetworkPower(
  nodes = 8,
  density = 0.30,
  sample_sizes = c(100, 200, 400),
  replications = 100,
  target_metric = "sensitivity",
  target_value = 0.60,
  target_probability = 0.80
)

summary(power)
plot(power)
quicknet_report(power)$text
```

也可以使用 `powerly` 后端进行 GGM 样本量规划：

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

### 11. 验证性、潜变量和动态网络

```r
omega <- matrix(1, 6, 6)
diag(omega) <- 0
colnames(omega) <- rownames(omega) <- paste0("x", 1:6)

confirmatory <- ConfirmatoryNet(data, vars = paste0("x", 1:6), omega = omega)
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

连续网络支持 Gaussian conditioning、虚拟敲除、虚拟敲降、precision-edge blocking、组合扰动和贪婪顺序优化：

```r
Perturbation(fit, method = "knockout", targets = "mpg")
Perturbation(fit, method = "knockdown", targets = "mpg", remaining_strength = 0.50)
blocked <- Perturbation(fit, method = "edge_block", targets = "mpg")
Perturbation(fit, method = "combination", targets = c("mpg", "cyl", "disp"))
sequence <- Perturbation(fit, method = "sequence", targets = c("mpg", "cyl", "disp"), steps = 2)

get_perturbation_plot(blocked, type = "edge_block")
get_perturbation_plot(sequence, type = "sequence")
```

对于 Ising 模型，可以使用 NIRA-style threshold perturbation：

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

扰动绘图会保持保守解释：只展示当前对象已经计算出的模拟摘要，包括靶点排序、剂量-响应、节点状态或激活变化、边阻断后的 spillover 摘要，以及贪婪序列步骤；这些图不表示临床疗效或因果干预效应。

这种解释边界遵循网络干预和模拟研究的文献，同时也遵守当前对中心性和模型内扰动排序的谨慎解释：如果没有合适的因果设计，不应把这些结果直接解释为真实治疗效应。

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
- SEM/CFA 后端：Rosseel, Y. (2012). `lavaan`: An R package for structural equation modeling. *Journal of Statistical Software, 48*(2), 1-36. https://doi.org/10.18637/jss.v048.i02
- 网络模型预测性：Haslbeck, J. M. B., & Waldorp, L. J. (2018). How well do network models predict observations? On the importance of predictability in network models. *Behavior Research Methods, 50*, 853-861. https://doi.org/10.3758/s13428-017-0910-x
- 桥接中心性：Jones, P. J., Ma, R., & McNally, R. J. (2021). Bridge centrality: A network approach to understanding comorbidity. *Multivariate Behavioral Research, 56*(2), 353-367. https://doi.org/10.1080/00273171.2019.1614898
- 网络比较检验：van Borkulo, C. D., van Bork, R., Boschloo, L., Kossakowski, J. J., Tio, P., Schoevers, R. A., Borsboom, D., & Waldorp, L. J. (2023). Comparing network structures on three aspects: A permutation test. *Psychological Methods, 28*(6), 1273-1285. https://doi.org/10.1037/met0000476
- Network Intervention Analysis：Blanken, T. F., van der Zweerde, T., van Straten, A., van Someren, E. J. W., Borsboom, D., & Lancee, J. (2019). Introducing Network Intervention Analysis to investigate sequential, symptom-specific treatment effects: A demonstration in co-occurring insomnia and depression. *Psychotherapy and Psychosomatics, 88*(1), 52-54. https://doi.org/10.1159/000495045
- 基于模拟的干预靶点评估：Lunansky, G., van Borkulo, C. D., & Borsboom, D. (2021). Intervening on psychopathology networks: Evaluating intervention targets through simulations. *PsyArXiv*. https://doi.org/10.31234/osf.io/sqhje
- 中心性解释的谨慎边界：Bringmann, L. F., Elmer, T., Epskamp, S., Krause, R. W., Schoch, D., Wichers, M., Wigman, J. T. W., & Snippe, E. (2019). What do centrality measures measure in psychological networks? *Journal of Abnormal Psychology, 128*(8), 892-903. https://doi.org/10.1037/abn0000446

## 版本更新

### 开发版本

- 新增统一的 `quicknet_fit` 对象，用于横断面和纵向网络模型。
- 扩展横断面模型支持，通过一致的 `quickNet()` 接口支持 EBICglasso、相关网络、偏相关网络、Ising、有序分类网络和 MGM。
- 新增纵向网络接口：`PanelNet()` 用于横滞后面板网络，`LongitudinalNet()` 用于 `graphicalVAR` 和 `mlVAR` 模型。
- 新增模型通用的边表、节点表、网络摘要、中心性辅助结果和稳定性汇总。
- 新增虚拟扰动与干预模拟辅助函数，支持 Gaussian-style 网络扰动和 Ising threshold perturbation。
- 新增符合保守解释边界的扰动绘图辅助函数，支持排序、剂量-响应、节点变化、边阻断和贪婪序列摘要。
- 新增 `NetworkPower()` / `SampleSize()`，用于基于模拟的网络样本量规划。
- 新增验证性网络、潜变量网络、SEM 面板网络、mixed VAR 和 time-varying mixed VAR 封装接口。
- 新增支持模型、稳定性、桥接中心性、网络比较和扰动分析的必要方法参考文献。
- 更新旧辅助函数，使其兼容新的 `quicknet_fit` 对象。
- 添加横断面和纵向网络工作流的 testthat 测试。
- 清理包文档、示例、拼写问题和 R CMD check 问题。
