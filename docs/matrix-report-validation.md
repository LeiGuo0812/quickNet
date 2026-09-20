# 矩阵、指标、报告与拟合状态核验

日期：2026-09-20。源码基线为 `adabdfc4aff65c5030f194343c52bb6fc5818705`
加本次工作区修改。本文记录当前行为和实际对照；源包估计方法保持其原有定义。

## 当前规则

- quickNet 的有向矩阵采用**行是结果节点、列是来源节点**。传给 qgraph 时转置。
  时间自回归项保留在矩阵、纵向边表及节点表的 autoregressive 列中；
  strength、expected influence、网络密度和 global strength 排除自环。
- `Centrality()` 和 `Bridge()` 根据拟合矩阵计算。缓存图形中的显示阈值不改变
  指标的输入。绘图仍可保留显示阈值，CSV 导出为完整拟合矩阵。
- 裸矩阵的具名行按列名对齐；不对称裸矩阵解释为有向矩阵。
  两网络边集合比较要求相同的有向/无向定义。对共同边，`get_edges_df()`
  的权重来自第一个网络；union 中仅第二个网络具有的边保留第二个网络权重。
- MetaNet 独立匹配研究矩阵的行、列，以及具名 nobs 的研究名称。
  无名称输入按位置解释。Meta-GVAR 保留“过去、当前”两个时间块，并在各块内
  对齐变量；带明确滞后后缀但时间块顺序不符时拒绝输入。
- `quicknet_report()` 的 `analysis_sample` 记录输入行、完整行和后端有效样本。
  有纵向数据时另有实际滞后观测、参与者及逐节点/局部估计计数。
  psychonetrics 读取 `sample@groups$nobs`；`sample@nobs` 是模型统计量数，不能当样本量。
  lavaan 读取 `lavInspect(nobs)`。例如 FIML 中 200 行含一整行缺失时报告分析 199 人、输入 200 行。
- `diagnostics` 保留源优化器状态和参数有限性。lavaan 另检查 `converged` 和
  `post.check`。psychonetrics 的 computed 只表示运行过，不能单独说明收敛。
  源软件没有保存状态时标为 unknown；不会宣称已验证收敛。
- glmnet 的负 jerr 按源定义保留可用的较大 lambda 解，并标为 partial；
  正 jerr 或非有限参数视为失败。已确认失败的拟合不参与自有重抽样摘要。
  psychonetrics/lavaan 的构建或估计警告同时保留在 `backend_warnings`，
  源优化器返回成功码也不抹掉梯度、边界等警告。

## 独立核验

`tools/validate-matrix-report.R` 对直接调用的源软件和 quickNet 分别估计。
12 个模型有 48 个矩阵/样本量/边表数值对照，最大绝对差约 `7.83e-16`：

| 模型 | 对照内容 |
|---|---|
| Confirmatory GGM、相关、协方差、精度、Ising | 原始后端对应参数矩阵的非对角线、实际 nobs |
| lavaan CFA | 潜变量相关；独立 lm 残差相关及 Matrix::nearPD 校正 |
| psychonetrics LNM、LRNM | omega_zeta 与适用的 omega_epsilon |
| RI-CLPM | 独立逐波块提取并取均值的 beta、创新协方差、随机截距协方差 |
| panel VAR、panel GVAR | beta 及相应协方差/偏相关层 |
| PanelSEM | 独立构造 lavaan 模型后，逐对平均相邻波次标准化路径 |

这些矩阵的单位不同：协方差、精度、偏相关、潜变量相关和标准化路径不可互换。
CFA 残差图是因子得分回归的残差相关，并非 psychonetrics RNM 的残差偏相关。
LRNM 对照使用预先指定的稀疏残差模板，以免把不可识别的任意满模型作为正确性证据。

`test-matrix-report-consistency.R` 另核对直接源 MetaNet 的相关/GGM 拟合、
两轴重排/研究权重重排，以及手算带正负边、自环、孤立节点和显示阈值的图。
`test-convergence-diagnostics.R` 包含真实 lavaan 迭代不足和真实 glmnet 短路径，
以及 S4 历史对象没有新诊断字段时的读取。纵向后端独立索引和数值证据见
[数据与时间核验](data-time-validation.md)；重抽样失败与覆盖率见
[重抽样核验](resampling-validation.md)。

## 图形与导出

`tools/validate-graphics.R` 实际生成 24 张 Cairo PDF/SVG（四种图：正负边、全零、
并列、有向；各包含网络、中心性、桥图），并读取 CSV 复核原矩阵。
中文、英文长标签通过 Cairo PDF 的文本提取与图像查看确认；负影响值、自环和
箭头方向可见。不可定义中心性保留 NA，并在图中说明省略原因。

网络图采用数字节点标签加完整名称图例。qgraph 默认可能自动缩写名字；需要原名时
显式提供 labels 或 nodeNames/legend。`device = "cairo_pdf"` 使用已安装字体生成
多语言 PDF；默认 pdf 不变。底层 networktools 对完全不可达的图可能给出 min/Inf
警告，原始警告保存在审计文件中；这类统计量没有被改写为有效的零值。

复现（从包目录执行）：

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 Rscript --vanilla tools/validate-matrix-report.R
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 Rscript --vanilla tools/validate-graphics.R
```

产物在工作区 `output/audit/matrix-report/` 和 `output/audit/graphics/`，包括源对照、
真实拟合对象、优化诊断、警告、版本及图形。正式包检查和实际 README 工作流另行记录。

方法依据：[qgraph centrality](https://search.r-project.org/CRAN/refmans/qgraph/html/centrality.html)、
[networktools bridge](https://search.r-project.org/CRAN/refmans/networktools/html/bridge.html)、
[psychonetrics runmodel](https://search.r-project.org/CRAN/refmans/psychonetrics/html/runmodel.html)、
[lavaan lavInspect](https://search.r-project.org/CRAN/refmans/lavaan/html/lavInspect.html)、
[glmnet 源 jerr 定义](https://github.com/cran/glmnet/blob/master/R/jerr.elnet.R)。
