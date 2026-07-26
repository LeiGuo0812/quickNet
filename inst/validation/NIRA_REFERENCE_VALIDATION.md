# NIRA reference compatibility validation

This validation records behavioral comparisons without making either reference
package a quickNet dependency and without copying reference source code into
quickNet.

## Reference versions

- `nodeIdentifyR` 1.0.0, MIT, commit
  `22ceb4c9c19d6c95a4030ea4ce13d8545cbbfdb3`.
- `NIRApost` 1.1.0 (2026-03-21), GPL >= 3, commit
  `d8055a8806ba0c52bf46c36098a25c84f27f648e`.
- R 4.5.3 and IsingSampler 0.5.0 (GPL-2).
- Seed 2025, 5,000 observations per condition, alleviating direction, and a
  two-standard-deviation threshold change.

The executable fixed-network check is in
`tools/nira_reference_validation.R`. It defines all network parameters,
recorded output, joint-Monte-Carlo-SE tolerances, and assertions.
The executable NIRApost check is in
`tools/nira_post_reference_validation.R`; it uses the same five-node fixture,
seed, perturbation, and recorded reference commits for its stability check.
Set `NIRAPOST_SOURCE_DIR` to that clean NIRApost checkout when the package is
not installed. The script then installs the checkout into a temporary library
and calls only its public exports; all declared NIRApost Imports must already
be available.

IsingSampler is a runtime dependency for the default literature engine; no
IsingSampler or NIRApost source is copied into quickNet, and quickNet's MIT
license is unchanged.

## Fixed five-node fixture

Thresholds are `N1=-2`, `N2=-1.2`, `N3=-0.3`, `N4=0.2`, and
`N5=3.15`.
The symmetric weight matrix has a zero diagonal and chain edges
`N1--N2=.01`, `N2--N3=.02`, `N3--N4=.03`, and `N4--N5=.04`; all other
edges are zero. The exact threshold delta is `2 * sd(thresholds) =
3.9354796`.

Using the public `nodeIdentifyR::simulateResponses()` and
`nodeIdentifyR::calculateSumScores()` functions produced:

| condition | n | mean | SD | SE |
|---|---:|---:|---:|---:|
| original | 5000 | 2.2922 | 0.9045346 | 0.0127921 |
| N1 | 5000 | 2.1802 | 0.8547948 | 0.0120886 |
| N2 | 5000 | 2.0576 | 0.7995061 | 0.0113067 |
| N3 | 5000 | 1.9092 | 0.7614928 | 0.0107691 |
| N4 | 5000 | 1.7524 | 0.7740892 | 0.0109473 |
| N5 | 5000 | 1.6546 | 1.0025467 | 0.0141782 |

The reference alleviating effects were `.1120, .2346, .3830, .5398,
.6376`. Their complete rank was `N5 > N4 > N3 > N2 > N1`; the smallest
adjacent-effect gap was `.0978`, or 5.46 joint Monte Carlo SE. Thus the
fixture has a genuinely separated rank rather than relying on the random order
of near ties.

The quickNet literature engine produced condition means
`2.2956, 2.1824, 2.0868, 1.8978, 1.7568, 1.6690` and effects
`.1132, .2088, .3978, .5388, .6266`. Every condition-mean difference was
below `max(.02, 5 * combined MCSE)`, every effect had the same direction,
and the complete rank was identical.

Node marginal activation probabilities were also compared for all 30
condition-by-node cells. The recorded reference matrix is:

| condition | N1 | N2 | N3 | N4 | N5 |
|---|---:|---:|---:|---:|---:|
| original | .1170 | .2294 | .4300 | .5564 | .9594 |
| N1 | .0026 | .2360 | .4194 | .5604 | .9618 |
| N2 | .1152 | .0056 | .4256 | .5540 | .9572 |
| N3 | .1292 | .2414 | .0160 | .5584 | .9642 |
| N4 | .1264 | .2252 | .4174 | .0272 | .9562 |
| N5 | .1174 | .2284 | .4330 | .5596 | .3162 |

The largest quickNet/reference marginal difference was `.0132`, and every
cell was below `max(.01, 5 * combined binomial MCSE)`. Both implementations
had six conditions, and every public nodeIdentifyR score vector equaled
`rowSums()` of its binary sample matrix. The threshold delta and all changed
thresholds agreed at machine precision.

The reference public API fixes the condition sample size internally and does
not expose it as an argument. quickNet deliberately makes `n_samples`
configurable. A fixed-length behavior in
`prepareDFforPlottingAndANOVA()` was also not reproduced.

## NIRApost output compatibility

The exported NIRApost functions were exercised from a temporary clean checkout
of the recorded commit. No GPL code or derived implementation was added to
quickNet.

- `permutationNIRAtest()` returned `stat` and `plot_data`. On a saved
  three-condition score fixture, `stat` had the documented mean, SD, SE,
  normal CI, Cohen's d, raw p, and adjusted-p columns. Raw p values were
  `1/5001 = 0.00019996`, confirming the plus-one grid; Holm values matched
  `stats::p.adjust()` exactly.
- `stabilityNIRAtest()` with two development repetitions returned two
  independently simulated `mean` and `sd` rows for the original plus five
  intervention conditions. `findMaxN(n=5)` gave each node exactly one rank per
  repetition and the same complete rank
  `N5 > N4 > N3 > N2 > N1`. quickNet preserves these aggregate semantics but
  intentionally does not retain every repetition's 30,000-row long sample.
- `runMgmmAnalysis()` and quickNet's independent moderation implementation
  were exercised on the same 250-case binary fixture generated with
  `plogis(2 - 4 * A * B)` and two development resamples. NIRApost selected all
  three moderator roles, with magnitude means `1.42`, `1.68`, and `1.74`,
  intervals `[1.18, 1.65]`, `[1.18, 2.17]`, and `[1.57, 1.90]`, and selection
  proportions of one. Its public output does not define direction for this
  fit. quickNet selected the same three roles while retaining MGM's public
  signs: each role had full-sample estimate `-.8987895`, resampling mean
  `-1.213713`, interval `[-1.459733, -.9676941]`, unit negative/same-sign
  proportions, two direction-defined repetitions, and zero failures.
  The executable check asserts these structural, stability, and direction
  semantics. This is a smoke check only; two resamples are not suitable for
  inference.

A structural sign fixture also confirms that if MGM reports a selected sign as
undefined, quickNet labels that role `estimate_scale = "magnitude"` and leaves
directional proportions unavailable rather than inventing a direction.

An installed-package PSOCK smoke test (three binary nodes, two resamples,
two workers) returned an object exactly identical to serial execution.
PSOCK workers inherit one BLAS/OpenMP thread each and the parent process
environment is restored after cluster creation; this prevents nested-thread
oversubscription observed with MGM on many-core hosts.

NIRApost's stability output retains full long-format simulations, while
quickNet retains only condition means and rank aggregates. NIRApost's
moderation resampling and quickNet's paired case-resampling use different RNG
schedules, so exact bootstrap estimates are neither expected nor required.

## Interpretation boundary

The checks establish parameterization, condition construction, total-score
definition, direction, adjusted-p structure, and rank-frequency semantics.
They do not establish causal validity, clinical efficacy, or uncertainty in
the originally estimated network. The reference-compatible default uses 100
finite MH sweeps and is not a convergence guarantee; strongly coupled or
multimodal networks require sensitivity checks with larger
`engine_iterations`. NIRApost remains an optional validation reference and is
not required to install, test, or run quickNet.
