# 原生网络模拟与样本量规划

更新日期：2026-10-07。实现依据为 Epskamp 与 Fried 的网络教程 §7.1，以及 Constantin、Schuurman 与 Vermunt 的 Monte Carlo 样本量方法；对照运行版本为 R 4.5.3、bootnet 1.9.1、powerly 1.10.0。

## 两种方法解决的问题

| 内容 | netSimulator | powerly |
|---|---|---|
| 研究问题 | 给定若干 N，网络和中心性恢复如何？ | 达到预设恢复质量的概率需要多大的 N？ |
| 计算方式 | 每个样本量、估计设置组合重复生成数据并估计网络 | Monte Carlo、单调样条拟合、分层 bootstrap，迭代收窄候选区间 |
| 指标 | 原生边恢复、强度等中心性恢复；可用 moreOutput 扩展 | 原生 sen、spe、mcc、rho |
| 多条件 | 估计参数向量形成条件组合；原始条件列保留 | 单次搜索使用一组真实网络与性能目标 |
| 输出 | 按条件的恢复分布、原生摘要和图 | 原生样本量中位估计、区间、各步骤结果及收敛状态 |
| 自动选择 N | 原生方法不做；研究者结合研究目标判断 | 按性能阈值与目标达标概率搜索 |
| 独立验证 | 再次运行指定条件的恢复模拟 | 原生 powerly::validate 在选定 N 生成新的数据 |

Epskamp 与 Fried 把预期加权网络作为模拟假设，并建议从参考数据取得网络时，对正则化选出的结构进行 `refit = TRUE`，减轻边权收缩偏差。教程中的示例不是普遍适用的最低样本量。实现直接保留作者程序中的恢复定义。[教程全文 §7.1](https://arxiv.org/html/1607.01367v9#S7.SS1)

powerly 区分性能阈值与概率阈值。例如 `measure_value = .6`、`statistic_value = .8` 表示寻找满足 `P(sensitivity >= .6) >= .8` 的 N。它在每个候选 N 内重抽恢复指标、拟合 bootstrap 曲线，并用曲线与概率目标的交点取得样本量区间；区间宽度达到 tolerance 才表示搜索收敛。验证另用新模拟。[论文全文](https://www.researchgate.net/publication/372249149_A_general_Monte_Carlo_method_for_sample_size_analysis_in_the_context_of_network_models)、[作者方法说明](https://powerly.dev/tutorial/method)

## 本包的封装边界

`NetworkPower()` 默认调用 `bootnet::netSimulator()`；`SampleSize()` 保持别名。必须给出明确假设的 `model_matrix` 或原生 `input`，也允许原生网络生成函数。此要求使模拟假设可见；原生函数自身还允许不指定 input 时生成默认网络。包不会改变传入网络的边权。

| quickNet 参数 | 原生参数／行为 |
|---|---|
| model_matrix | netSimulator 的 input；powerly 的 model_matrix |
| sample_sizes | netSimulator 的 nCases |
| replications | netSimulator 的 nReps；powerly 的 replications |
| gamma | netSimulator 的单值 tuning 别名；多个条件直接用 tuning |
| default、dataGenerator、nCores、moreArgs、moreOutput | 原样交给 netSimulator |
| target_metric／target_value／target_probability | powerly 的 measure／measure_value／statistic_value |
| range_lower、range_upper、samples、boots、iterations、tolerance | 原样交给 powerly |

未传入的 netSimulator 默认参数由原函数处理；因此连续或有序数据生成必须按研究需要明确配置。参数向量在 `...` 中代表多条件；需要固定传入向量或函数的估计参数使用原生 `moreArgs`。`moreOutput` 的自定义指标完整保留在原始结果中；原生摘要并不自动汇总所有扩展指标。[bootnet 官方手册](https://cran.r-project.org/web/packages/bootnet/bootnet.pdf)

netSimulator 的 `$fit`、`$results`、`$raw` 保留未修改的原生对象，包括 `ExpectedInfluence`、偏差、条件和错误行。`summary(plan)` 与 `plot(plan, ...)` 调用原生方法。`$recommendation$status` 为 `not_applicable`，没有自动推荐 N。若全部模拟失败，仍保留原始结果，`$summary_error` 说明原生摘要失败原因；正式调用 `summary(plan)` 保留原生报错。

powerly 保留原生 `$fit`，其三步骤、样本量区间和中位曲线不被重写。`plot(plan, step = 1/2/3)` 使用原生图。`$recommendation` 分别记录曲线是否达标、`algorithm_converged`、实际迭代数和推荐区间宽度；原生范围上界回退不自动视为达标。未收敛结果标注为暂定估计。

powerly 1.10.0 的公共接口仅支持横断面 GGM 与上述四种指标。其生成器默认五级有序数据，估计器使用 gamma = .5；公共接口不能配置这两个设置。quickNet 拒绝非空 gamma 和伪 levels 参数。源程序将未定义恢复指标替换为零；本包记录并保留这一政策。[GgmModel 源码](https://github.com/cran/powerly/blob/1.10.0/R/GgmModel.R)、[StepOne 源码](https://github.com/cran/powerly/blob/1.10.0/R/StepOne.R)

## 原生独立验证

`ValidateNetworkPower(plan, replications = ..., sample = ..., seed = ..., cores = ..., verbose = ...)` 直接调用 `powerly::validate(plan$fit, ...)`。默认重复次数继承安装版本的原生值，当前为 3000。省略 sample 时验证已经达到搜索曲线目标的中位候选；未达标时需明确指定要验证的 N。

结果 `$fit` 保留原生 Validation 对象。`summary(validation)` 和 `quicknet_report(validation)` 额外整理达标次数、原生达标概率、MCSE、精确二项 95% 区间以及条件性判断；`plot(validation)` 委派原生绘图。区间跨越目标时标为 uncertain，不把搜索阶段的曲线达标当作验证成功。区间不包含假设网络的不确定性。[Validation 源码](https://github.com/cran/powerly/blob/1.10.0/R/Validation.R)

训练和验证应使用不同随机数据。指定 seed 是单核下的复现控制；并行运行的随机流语义遵循原生后端，不承诺并行与单核逐项相同。

常规样本量搜索使用 `increasing = TRUE`。powerly 1.10.0 在 `increasing = FALSE` 时按概率小于等于目标寻找曲线交点，但原生 validation 仍按大于等于目标评价达标。本包保持这两个原生行为，分别记录 `search_probability_comparison` 与 `validation_probability_comparison`；遇到方向不同时报告明确说明，避免混用准则。

## 方法范围

样本量规划仅保留原生 `netSimulator` 与 `powerly` 两条路径，`SampleSize` 直接使用同一函数接口。netSimulator 的原生模型能力还包括 Ising 列表输入，其交互矩阵不被误当作 GGM 偏相关矩阵检查。powerly 的 `nodes`、`density`、`positive`、`edge_strength` 只在未提供总体矩阵、需要生成假设网络时使用；不再维护另一套网络生成、恢复指标或网格推荐算法。

这次更新不把横断面恢复规划扩展为组间检验或纵向样本量计算。规划中的中心性恢复相关也不是 CS 系数；收集后仍需使用适合模型的精度与稳定性分析。

推文中的 250、400、500 例，以及每个节点 10 或 20 例，没有写入自动计算规则。它们来自特定模拟条件，不能替代本研究假设网络下的模拟。若主要目标是桥中心性恢复，powerly 当前四种原生指标无法直接对应；可在 netSimulator 的 moreOutput 中另行定义并检验恢复指标，比较候选 N，但不能将这种扩展称为原生 powerly 桥中心性功效计算。

## 验证证据

- 同种子下，将原生 netSimulator 与公共入口逐项比较：连续多条件、有序五级数据、Ising graph/intercepts、自定义输出及错误保留。
- 将原生 powerly 的每一步指标、拟合曲线、bootstrap 曲线、推荐 N 和区间与封装逐项比较；另核查未收敛及边界回退。
- 使用新的种子对照原生 powerly::validate，并以独立 binom.test 核对附加区间。
- 双语 README 提供独立、小预算示例。预算用于接口验证，不构成研究样本量建议。

本轮移除重复算法后，README 的三个原生示例已分别在独立 Rscript 会话运行成功，双语可执行代码相同。示例仍使用其公开的小规模预算，绘图警告、未收敛及验证不确定状态如实保留。上一轮断言数量和打包检查结果不作为本轮通过证据。

原生验证脚本也已重新执行，退出状态为 0。连续多条件的 8 行结果、五级有序数据的 4 行结果均无失败；`NetworkPower()` 与 `SampleSize()` 的原始结果以及封装摘要均与相同种子的原生结果一致。powerly 的三阶段、曲线、中位推荐和区间逐项一致；新的 300 次独立验证与直接调用原生 `validate()` 的结果一致，附加区间与独立 `binom.test()` 一致。完整日志及生成物位于 `/tmp/quicknet-native-cleanup-validation.log` 和 `/tmp/quicknet-native-cleanup-validation/`。

### 保留的原生 powerly 数值证据

先前核验以五节点、相邻边偏相关为 0.3 的链式网络为假设，使用种子 88021、样本量范围 50–500、8 个样本量点、每点 20 次重复、80 次 bootstrap、1 次迭代和单核。直接运行 `powerly::powerly()` 与封装的真实网络、每个恢复指标、逐样本量达标统计和推荐值完全一致。中位推荐 N=199，对应 bootstrap 中位曲线 0.8016023，单独的点拟合曲线 0.8072891。该搜索未收敛，推荐值仅为暂定估计。

随后用种子 88022，在 N=199 进行 300 次原生独立验证：228/300 达标，达标概率为 0.7600，MCSE 为 0.02466，精确二项 95% 区间为 [0.70757, 0.80721]。区间跨越目标 0.8，不能据此确立达标。本轮脚本重新运行复现了这些数值；它们描述已运行的固定设计，不是通用样本量建议。

### 当前复现入口

从包根目录执行：

```sh
Rscript tools/validate-network-power.R ../output/audit/power-native
```

脚本只调用公共原生接口，核对连续多条件与五级有序 netSimulator、powerly 三阶段及区间的一致性，并使用新的种子对照 `ValidateNetworkPower()` 和 `powerly::validate()`。精确二项区间另用 `stats::binom.test()` 核对。生成物包括原生结果和封装的 RDS、netSimulator 逐条件 CSV、powerly 与验证汇总 CSV、源文件哈希、种子与 sessionInfo；新输出目录与之前审计目录分开。仓库外已有历史生成物保留。

### 本轮最终整包检查

2026-10-07，在 Linux/WSL、R 4.5.3 和当前已安装依赖下，`R CMD build --no-build-vignettes --no-manual` 成功；随后执行完整 `R CMD check --no-manual`，全部 Suggests 可用，Rd examples 通过，最终 `Status: OK`。完整 testthat 套件为 `[FAIL 0 | WARN 0 | SKIP 0 | PASS 3013]`。

英文 README 的 37 个分析块分别在新的 R 会话中运行，37/37 通过；两语对应的 37 个可执行代码块解析一致，两个安装块不执行。第 19 个工作流块中的 `quicknet_power_validation` 已纳入报告检查并保存，状态 `uncertain` 保留。图形脚本退出 0，验证 24 份非空 PDF/SVG 及导出矩阵、指标表；README 工作流另行检查报告与文件导出。真实 covariance 与 lvm 后端补充运行通过。

[机读验证摘要](validation/native-network-power-2026-10-07.json)随仓库保留检查状态、核心数值和 R 源码 SHA-256。完整检查日志、测试输出、README 逐块记录及原生验证 CSV 已归档到本地 `../output/audit/native-network-power-2026-10-07/`；构建和临时缓存可以清理。删除内容、实际修复与功能覆盖见 [本轮整包功能核验](package-functional-check.md)。这些结果支持实际运行的接口与数据条件，不表示每个模型在所有后端版本、平台和数据情形下均已充分验证。

## 文献

- Epskamp, S., & Fried, E. I. (2018). A tutorial on regularized partial correlation networks. Psychological Methods, 23(4), 617–634. [正式 DOI](https://doi.org/10.1037/met0000167)。
- Constantin, M. A., Schuurman, N. K., & Vermunt, J. K. (2026; 2023 年在线发表). A general Monte Carlo method for sample size analysis in the context of network models. Psychological Methods, 31(3), 385–405. [正式 DOI](https://doi.org/10.1037/met0000555)、[PubMed 书目](https://pubmed.ncbi.nlm.nih.gov/37428726/)。
