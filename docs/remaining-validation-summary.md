# 剩余核验与修复汇总

本轮覆盖核验计划中的 A–E、H 项。quickNet 聚合已有算法：有源软件的部分以其实际输出为参照，自有计算用手算、独立公式或穷举结果核对。参数仍通过函数参数传入。

## 已修复与核验

| 范围 | 当前行为与证据 |
|---|---|
| 矩阵、边表、指标和报告 | 统计指标使用拟合矩阵；显示阈值不改变统计网络。统一核对有向矩阵方向、自环、符号、名称和 Meta 研究/矩阵轴匹配。12 个实际模型、48 项源对照通过，最大绝对差异为 `7.83e-16`。见[矩阵和报告](matrix-report-validation.md)。 |
| 数据与时间 | 保留输入行映射、时间配对和有效分析样本数；处理省略时间索引、跨日、缺测及逐节点样本差异。7 项真实后端或独立设计对照通过。见[数据与时间](data-time-validation.md)。 |
| 重抽样、失败与收敛 | 记录请求、成功、失败和未定义统计量；保留失败原因、缺失网络层及原生警告。后端状态区分成功、部分结果、失败和未知。11 组源软件对照通过，另完成 61,119 次 bootstrap 和 200 个 NCT 独立数据集核验。见[重抽样](resampling-validation.md)。 |
| 虚拟干预 | NIRA 调节建网规则优先读取实际 Ising 后端设置，显式函数参数可覆盖。核验小网络精确枚举、独立高斯公式、积分、序列穷举、近奇异情形和并列排名；输出区分固定网络模拟与参与者重抽样。见[干预可靠性](intervention-reliability-validation.md)。 |
| 工作流、文档和导出 | 修复中英文示例的数据、标签及调用顺序。Linux 和原生 Windows 各运行两种语言，每次完整执行 32 个代码块并检查报告及导出；另验证 24 份 PDF/SVG 图形。见[工作流](workflow-validation.md)和[图形核验](matrix-report-validation.md)。 |
| CI 与性能 | 配置三平台的基线、最新和最小依赖检查；完整依赖任务执行两种语言的 README。完成六种性能配置、每种三次独立测量。见[平台兼容性](platform-compatibility-validation.md)及[性能基线](performance-validation.md)。 |

方法的适用条件沿用源实现。主说明和屏幕输出简要提示，具体来源、版本、模拟设计及结果可从上述记录追溯。

## 最终整包检查

2026-09-20，最终代码、帮助文件和测试使用同一源码归档完成以下检查：

| 环境 | 检查 | 结果 |
|---|---|---|
| Linux，R 4.5.3，完整依赖 | `R CMD check --no-manual` | 0 错误、0 警告、0 NOTE；2,836 项断言通过，无测试警告或跳过 |
| 原生 Windows，R 4.5.0，另一组已安装依赖 | `R CMD check --no-manual` | 0 错误、0 警告、0 NOTE；2,824 项断言通过，无测试警告或跳过；兼容性 smoke 检查通过 |
| Linux，隔离最小依赖 | `R CMD check --no-manual --no-tests` | 0 错误、0 警告、0 NOTE；本环境有意不安装 testthat、不运行测试套件 |

两个完整环境相差的 12 项断言是仅在后端版本匹配时运行的历史对象数值比较；跨版本对象另有[独立验证](platform-compatibility-validation.md)。最终检查日志、依赖版本和 SHA-256 清单位于工作区 `output/audit/remaining-checks/`，其中 `final-package-manifest.json` 记录归档与当前源码的一致性。代码、帮助文件和测试逐文件核对一致；最终 README 文字修订另有记录，已确认执行代码未变。`git diff --check` 通过。

## 先前两项核验

功效与样本量规划见[功效核验](network-power-validation.md)；历史对象、跨会话及依赖兼容见[旧对象核验](legacy-object-validation.md)和[平台兼容性](platform-compatibility-validation.md)。这些记录及其原始检查清单保留，不与本轮结果混写。

本记录汇总本地核验结果；macOS 和远端 CI 的状态以对应提交的 GitHub Actions 运行记录为准，配置文件本身不作为通过证据。
