# 本轮整包功能核验

日期：2026-10-07。本轮功能核验已完成：Linux/WSL、R 4.5.3 环境下构建成功，完整 `R CMD check --no-manual` 为 `Status: OK`，3013 个测试断言通过，英文 README 的 37 个分析块全部独立运行成功。保留原生 netSimulator 与 powerly 样本量规划并删除重叠算法；本记录对应本轮源码，之前的检查数量不计作本轮通过证据。

## 保留与删除的范围

`NetworkPower()` 默认调用 `bootnet::netSimulator()`，另可选择 `powerly`；`SampleSize()` 直接转发到同一规划入口。原生恢复结果、条件列、摘要、绘图、powerly 三阶段和独立验证保留。powerly 未提供 `model_matrix` 时，仍可用 `nodes`、`density`、`positive`、`edge_strength` 配置其原生假设网络生成。

已删除旧 `method = "monte_carlo"` 算法、专属网络生成与恢复指标 helper、网格推荐逻辑以及相关公开参数和旧验证脚本依赖。旧 `estimator` 字符串选择、`powerly_args` 包装列表和自定义恢复指标的数值 `threshold` 不再属于本包规划接口。netSimulator 原生 `threshold` 参数仍可经 `...` 原样传递，语义由原生程序决定；不把该原生参数误当成已删除的数值阈值功能。

重复的旧样本量文档已删除，其中有价值的原生 powerly 对照与独立验证证据已整合到 [原生规划说明](native-network-power.md)。当前 README 和核验工具只使用保留的原生接口。仓库外已有历史审计生成物保留。

## 本轮发现并修复的功能问题

- **powerly 事前校验误拦原生参数。** 通用校验器自动加入的 `missing`、`require_complete` 不是 powerly 规划参数，现从原生参数校验中排除，防止有效调用被错误拒绝。
- **R 参数列表的部分匹配。** `statistic`、`measure` 的读取都改为 `[[ ]]` 精确字段访问，避免仅提供 `statistic_value` 或 `measure_value` 时被 R 的 `$` 部分匹配误认为提供了相应指标字段。
- **样本量概率来源混淆。** powerly 的 bootstrap 中位曲线缺失时，明确记录其不可用，不再用点拟合曲线替代；原生数值候选仍可保留，但不因此认定目标达标。
- **稳定性接口范围检查过晚。** `Stability()` 在重抽样前明确只接受 EBICglasso、correlation、partial、ordinal、Ising、MGM 六种探索性横断面模型。支持的面板和密集纵向模型使用 `LongitudinalStability()`；其他模型不会先执行不适用的重抽样。
- **稳定性导出静默缺项。** `get_stability_plot()` 现在导出可用的自定义边 bootstrap 与 case-drop 表；没有可导出的图或所请求表时明确报错。自定义 case-drop 相关表不被称为 bootnet CS 系数。
- **独立验证对象未纳入工作流检查。** README 核验工具将 `quicknet_power_validation` 纳入报告检查与结果保存。

## 检查清单与证据

[机读验证摘要](validation/native-network-power-2026-10-07.json)随仓库保存检查结果、核心数值和 R 源码 SHA-256。原始日志、逐块运行记录和 CSV 已归档到本地 `../output/audit/native-network-power-2026-10-07/`，无需保留构建缓存。

以下均为本轮实际完成的检查。测试套件无失败、警告或跳过；原生示例中的警告和统计状态保留在执行日志中。

| 检查项 | 实际覆盖范围 | 最终结果 | 证据 |
|---|---|---|---|
| 删除与文档一致性 | 当前规划方法、参数、helper、示例、文档引用及原生参数保留 | 通过 | 源码、README、docs 与工具脚本搜索；`git diff --check` |
| 公开入口与帮助页 | 41 个 export 的 Rd 覆盖；模型注册与输入规格各 32 项的一致性 | 通过 | 当前 NAMESPACE、man、model_registry 与 input_specs 静态核对 |
| 全部自动测试 | 当前完整 testthat 套件；新稳定性范围与导出回归 | PASS 3013；FAIL/WARN/SKIP 均为 0 | [验证摘要](validation/native-network-power-2026-10-07.json)；新稳定性接口专项 25 个断言通过 |
| 原生规划与独立验证 | 连续多条件、有序五级 netSimulator；两公开入口；powerly 三阶段、推荐、区间和 300 次新模拟验证 | 脚本退出 0；原生一致性核对通过 | [验证摘要](validation/native-network-power-2026-10-07.json)；原生验证脚本与本地归档 |
| README 独立运行 | 英文 37 个分析块逐块使用新 R 会话；中文可执行代码与英文逐块一致；README 的报告与文件导出检查 | 37/37 通过；两语 37 对代码一致 | [验证摘要](validation/native-network-power-2026-10-07.json)；独立工作流记录 |
| 帮助页示例与安装包检查 | `R CMD build --no-build-vignettes --no-manual` 后运行 `R CMD check --no-manual`，包含 Rd examples，全部 Suggests 可用 | 构建成功；Status: OK；examples OK | [验证摘要](validation/native-network-power-2026-10-07.json)；完整包检查日志 |
| 图形与矩阵/指标导出 | 24 份非空 PDF/SVG 图形，以及导出的矩阵与指标表；报告导出由 README 工作流另行检查 | 脚本退出 0；24 份图形与表格检查通过 | 本轮图形验证日志与生成物 |
| 补充真实后端运行 | `ConfirmatoryNet(model = "covariance")` 和 `LatentNet(model = "lvm")` 的有限网络、诊断与报告；不适用稳定性入口的明确拒绝 | 通过 | [验证摘要](validation/native-network-power-2026-10-07.json)；补充模型运行日志；`confirmatory_covariance` 是前者拟合结果中的模型名称 |

README 的两个安装代码块不执行，不计入 37 个分析块。中文本轮采用与英文逐块解析一致性的检查，没有再重复执行全部中文例子。工作流第 19 个分析块中的 `quicknet_power_validation` 已检查报告并保存结果，状态 `uncertain` 如实保留。

原生脚本已分别将 `NetworkPower()`、`SampleSize()` 的结果与同种子的原生 netSimulator 比较。连续多条件保留 8 行、有序设计保留 4 行，均无失败。powerly 真实对照复现 N=199、未收敛、bootstrap 中位曲线 0.8016023 和点曲线 0.8072891；独立 300 次验证为 228/300 达标，达标概率 0.76，精确二项 95% 区间 [0.70757, 0.80721]，状态为 `uncertain`。这些是固定设计的实现对照，不是通用最低样本量或验证成功声明。

## 支持边界与结论解释

netSimulator 描述给定假设、估计方法和候选 N 下的恢复分布，不自动推荐样本量；其原生模型能力还可接受 Ising 的图与截距输入。powerly 当前公共接口仅支持横断面 GGM 与其四种恢复指标，内部默认 gamma = 0.5、五级有序生成不能通过其公共接口修改。恢复概率不等于边假设检验功效，也不等于数据收集后的 CS 系数。桥中心性、组间比较与纵向设计需要相应的模拟目标和数据生成机制。

41 个 export 有帮助页、32 项模型注册与输入规格一致，属于接口和文档覆盖证据；不能据此声称每一种模型、后端版本及数据情形都经过充分数值或统计验证。README、真实后端运行与图形检查支持其实际使用的演示条件。原生警告、低预算不收敛及验证区间跨越目标等结果均如实保留；不通过隐藏这些状态使演示表现为达标。

本轮结果适用于当前 Linux/WSL、R 4.5.3 和已安装依赖版本。整包测试、帮助页示例、37 个独立 README 分析块、原生一致性脚本、24 份图形及矩阵/指标导出全部完成。其他平台、不同后端版本以及未运行的数据情形不由本轮记录建立通过证据。
