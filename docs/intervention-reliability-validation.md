# Virtual intervention accuracy and ranking reliability

Validation date: 2026-09-20.

This audit separates numerical agreement, Monte Carlo error under a fixed
network, participant resampling, and causal interpretation. None of these
calculations establish a causal treatment effect.

## Fixed sources and design

The reference implementations are NodeIdentifyR commit
`22ceb4c9c19d6c95a4030ea4ce13d8545cbbfdb3`, NIRApost commit
`6231832736df7c693b630820729f73f64c30ec92`, IsingSampler 0.5.0, mgm 1.2.15,
and SymPerturb 0.1.0 commit `76dd4178b285b80beb69f14a642e84ed1cabc7a0`.
The R audit uses R 4.5.3 and records package versions, seeds, source hashes,
raw results and available stage timings in
`../output/audit/intervention-reliability/`.

The simulation design was fixed before interpreting results:

| Component | Independent units and internal budget | Target |
| --- | --- | --- |
| NIRA numerical accuracy | Four 4-node Ising networks; two engines; 12 seeds per budget; `(n_samples, iterations)` = `(500,10)`, `(3000,100)`, `(3000,500)` | All 16 states enumerated; node probabilities, total means, directional effects, pooled-SD effect sizes and ranks. |
| NIRA rank stability | 120 repeated fixed-network simulations; 500 chains/condition; 100 Gibbs sweeps | Absolute-effect ranking, including three exchangeable candidates; reported MCSE and random ordering of exact ties. |
| Simulated-distribution permutation | 300 independent simulated datasets per null/weak-effect and sample-size combination; 199 permutations; 100 or 1000 simulated observations per condition | Conditional two-sided rejection rates and exact binomial intervals. |
| Moderation screening | Null, positive and negative three-way interaction; 12 independent datasets per condition; 300 participants; 19 bootstrap refits, AND rule, EBIC gamma 0.25 | Family detection, failed runs, retained bootstrap counts, and binomial MCSE/intervals. |
| SymPerturb participant bootstrap | Two population designs; 20 independent datasets/design; 120 participants; 39 bootstrap resamples/dataset | VPPS/rank percentile coverage and first-place frequencies conditional on a fixed candidate set. |
| Sequence search | 20 independently generated networks; 5 candidates; 3 steps; all 60 ordered sequences | Complete width-60 beam versus independent exhaustive objective; width-1 approximation loss. |

The small moderation and participant-bootstrap experiments are finite
screening audits. They do not establish nominal 95% coverage or calibrated
familywise error across network types. MCSE is `sqrt(p*(1-p)/R)` for a
proportion; exact binomial intervals are also retained so that zero observed
errors are not presented as certainty. Twelve or twenty outer datasets leave
substantial uncertainty. Internal bootstrap count and outer dataset count are
different quantities.

## NIRA numerical and inference checks

The exact reference assigns each binary state probability proportional to
`exp(beta * (sum(threshold*x) + sum_{i<j} W[i,j]*x[i]*x[j])))`.
Threshold changes are `2 * sd(thresholds)`. The designs include strong positive
coupling near two competing modes, mixed signs, nearly deterministic nodes,
and nearly tied targets. `sampling-summary.csv` reports bias, RMSE, empirical
bias MCSE, sampling MCSE, effect-size error, undefined effect-size counts, rank
error and top-1 frequencies. Increasing the number of chains reduces sampling
noise but cannot by itself remove finite-iteration initialization bias.

The strong-positive design showed this limitation directly. The following
effect RMSEs pool the four targets and 12 seeds within each configuration:

| Engine | 500 chains, 10 iterations | 3000 chains, 100 iterations | 3000 chains, 500 iterations |
| --- | --- | --- | --- |
| IsingSampler MH | 0.1684 | 0.0870 | 0.0417 |
| Native Gibbs | 0.1453 | 0.1102 | 0.0553 |

At 100 iterations the mean directional-effect bias was -0.0747 for MH and
-0.0989 for Gibbs; at 500 iterations it was -0.0135 and -0.0242. These are
descriptive averages across correlated target estimates; per-target bias MCSE
uses the 12 independent seeds and is stored separately. IsingSampler's API
calls its engine `MH`; the inspected C++ implementation updates each node from
its full conditional probability, starting a fresh random state for every
sample. The native Bernoulli implementation uses the same transition law, with
different RNG consumption. Differences between these empirical RMSEs are not
evidence that one engine has a superior mixing algorithm.
The audit does not treat 100 or 500 iterations as a universal convergence
criterion, and it does not replace either source sampling rule.

For the extreme-threshold design, the small budget yielded a zero empirical
pooled standard deviation in 17/48 target estimates with the author engine and
15/48 with the native engine. Cohen's d was retained as unavailable (`NA`),
not zero; both larger budgets had no undefined d values. A finite d or a narrow
conditional mean interval still does not establish adequate mixing or precise
estimation of rare-state probabilities.

The unmodified author `simulateResponses()` was also run directly at its
5000-sample default on the mixed-sign case. Both author and quickNet condition
means were within two exact Monte Carlo standard errors of the enumerated
means. Different RNG streams and condition order need not produce identical
sample matrices. For an independently enumerated 20-allocation permutation
example, exact `p=0.2`; both unmodified NIRApost and quickNet returned `0.19995`
with 19,999 permutations. Both returned the same raw Cohen's d,
`2.0412414523`. The main quickNet `cohen_d` remains directional; `raw_cohen_d`
uses intervention minus original.

The fixed-network null rejection rates were 2.67% at 100 simulated observations
and 3.67% at 1000, with MCSE 0.93 and 1.09 percentage points. Their exact 95%
binomial intervals were 1.16–5.19% and 1.84–6.47%. These results concern equal
simulated score distributions, independent chains and the chosen 199-permutation
test using `p < .05`.

For the *same* weak model-implied effect, increasing the simulated sample size
from 100 to 1000 increased the significant proportion from 3.67% to 28.67%
(MCSE 1.09 and 2.61 percentage points). This is computational precision under a
fixed network. It is not increased evidence from additional study participants.
Condition-mean intervals, simulated-distribution permutation p-values and
Monte Carlo rank stability omit uncertainty from estimating the original
network.

In the separate 120-repeat near-tie stability run, individual top-1 frequencies
were 20.8–28.3%, with Monte Carlo standard errors of 3.7–4.1 percentage points.
All 120 repetitions succeeded. The native condition sampler and stability
worker also produced identical condition means when supplied the same task
streams, thresholds, inverse temperature and iteration budget.

Moderation uses participant resampling and refits the moderated MGM for every
candidate role. Its percentile exclusion-of-zero rule is a regularized-model
screening criterion, not a general familywise 5% hypothesis test. The finite
experiment produced the following family detection frequencies:

| Generating interaction | Detected datasets | MCSE, percentage points | Exact 95% binomial interval |
| --- | --- | --- | --- |
| Pairwise null | 0/12 | 0 | 0–26.46% |
| Positive three-way | 11/12 | 7.98 | 61.52–99.79% |
| Negative three-way | 12/12 | 0 | 73.54–100% |

All 36 original datasets and their 684 bootstrap repetitions completed.
The wide intervals, particularly the upper bound with zero null detections,
prevent a claim of nominal familywise error control. A negative screen does
not establish edge invariance.

The sparse-category diagnostics distinguish failure before screening from
failure during resampling. With only one event among 40 participants, the
original MGM fails its requirement for at least two events per category.
The moderation driver correctly stops at this full-sample fit. A separate
40-worker diagnostic on those data recorded 27 failures, but cannot be treated
as a valid completed moderation analysis. With two events among 40 participants,
the full-sample fit succeeds; 9/19 bootstrap repetitions then fail because a
resample has too few events or zero variance. The driver correctly stops under
its existing greater-than-20% failure policy and reports both causes. Failed
fits are never converted to zero moderation. When failures remain within the
allowed proportion, intervals use successful repetitions and the returned
counts identify that denominator. Exact worker errors and both driver
diagnostics are retained in the audit artifacts.

## Independent Gaussian and bounded-observation checks

The state oracle starts from a constructed regression model
`X_K = mu_K + B (X_S-mu_S) + epsilon`, with a known loading matrix `B`, target
covariance and independent residual covariance. This avoids reusing the
implementation's covariance-block inverse as the numerical oracle. Twelve
linked, location-only, scale-only and independent-power cases, including zero
dose and complete target suppression, had zero mean error and maximum
covariance error `2.22e-16`; post-intervention covariances were positive
semidefinite within floating-point tolerance.

For bounded observations the independent identity
`E[clip(Y,l,u)] = l + integral_l^u P(Y > t) dt` was integrated numerically.
Thirty-five combinations covered means inside/outside the bounds, zero
variance, narrow distributions and broad tails. Maximum error relative to
the implementation's normal-density/CDF expression was `4.44e-16`.
This is a winsorised-normal expectation, not a truncated-normal mean.

Constant data, all-zero data and near-singular data were checked at ridge 0
and 0.02. Zero dose restored baseline moments, zero topology produced zero
communication-block utility, and constant participant resamples remained
finite. Column permutations and non-ASCII/long names preserved aligned
moments and scores with the full partner candidate pool.

There is a numerical boundary to cross-language identity. With ridge 0 and a
covariance condition number approximately `4.39e16`, R's `solve()` detects a
computationally singular matrix and the implementation uses its pseudoinverse.
NumPy's ordinary inverse in the reference returned negative precision diagonal
entries; the reference's clipped denominator then produced partial correlations
as large as `5.44e32`. At ridge 0.02 the condition number was approximately 110,
the precision difference was `9.24e-14`, and the intervention covariance
difference was `6.11e-16`. The stable pseudoinverse behavior was retained;
quickNet does not reproduce an unstable ordinary inverse merely to force
equality. The existing seven Python-reference fixtures still pass.

## Rankings, bootstrap and sequences

SymPerturb assigns the minimum rank to tied VPPS values. Consequently, top-k
includes every candidate whose minimum rank is at most k, which may be more
than k candidates. In the constant-data check all five candidates had VPPS 50,
rank 1 and top-1 probability 1. This indicates a complete tie. It does not
identify five uniquely optimal targets. Candidate-set normalization also means
VPPS values cannot be assumed invariant when candidates are added or removed.

In the separated Gaussian design, population target A ranked first in all
20 independent datasets, and its mean bootstrap top-1 probability was 0.9295.
In the near-tie design, each candidate ranked first in only 15–25% of datasets.
VPPS percentile coverage ranged from 80–100% in the separated design and
70–95% in the near-tie design; these coarse estimates and their wide binomial
intervals are retained in `bootstrap-summary.csv`. In particular, the audit
does not certify bootstrap percentile coverage for rank statistics or
candidate-set min-max scores near ties. Tied ranks can change discontinuously
under arbitrarily small estimation perturbations.

The sequence oracle computes the unit-dose Gaussian conditional mean using
the precision matrix and directly evaluates every ordered path's discounted
marginal benefit minus cost. All 60 paths in each of 20 networks agreed with
the complete beam, maximum absolute objective difference `1.89e-15`.
The width-1 beam missed the optimum in 16/20 networks; the largest absolute
objective loss was 1.36658. This is an expected limitation of finite beam
search. The algorithm and objective were not changed to eliminate that loss.

## Implementation changes and reproduction

NIRA now prefers the actual fitted Ising backend's AND/OR rule when inferring
the moderation rule, rather than stale wrapper metadata; an explicit
`moderation_rule` still takes precedence. Method-scope labels distinguish
fixed-network simulation from participant resampling; detailed numerical and
ranking qualifications are retained in this technical audit.

Run the independent stages from the package root, setting thread limits before
R starts so an already-initialized BLAS does not oversubscribe small matrix
operations:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1 \
  Rscript --vanilla tools/validate-intervention-reliability.R all
```

Any stage in the design table can be run separately by replacing `all` with
`sampling`, `stability`, `permutation`, `moderation`, `failure_guard`,
`symperturb`, `bootstrap` or `reference`. The optional author-source stage
needs the fixed checkouts;
set `QUICKNET_NODEIDENTIFYR_SOURCE` and `QUICKNET_NIRAPOST_SOURCE` if they are
not in the workspace's `../tmp/` reference directories.

The near-singular Python diagnostic runs the original source without vendoring
it into quickNet:

```sh
OPENBLAS_NUM_THREADS=1 python tools/generate-intervention-reference.py \
  --source ../reference/SymPerturb/src
```

Run that command after the `reference` stage generates its input CSV. Python
and NumPy are needed only for this diagnostic. Ordinary R tests retain the
existing stored independent Python fixtures. The new deterministic tests are
in `tests/testthat/test-intervention-reliability.R`.
Its eight groups and 61 assertions passed without failures, warnings or skips;
the NIRA and existing SymPerturb reference/interface regressions also passed.

### Saved-result revalidation and operational provenance

The long sampling and moderation runs completed and saved all numerical
results. Adding later stage definitions to the active Rscript file caused a
trailing-code read error after those saves; both original error logs are
retained as `intervention-sampling.log` and
`intervention-moderation.log` in the audit directory. Their missing final
timing records are not treated as performance benchmarks. The unchanged
simulations were not rerun solely to remove this operational error.

A separate clean process parsed the frozen script, verified all eight stage
dispatches, reloaded every saved case and regenerated both summaries from the
raw results. It confirmed 24 sampling configurations, 12 seeds per cell,
1,152 target records, 96 sampling summary rows, 36 moderation datasets and
684 successful moderation bootstrap repetitions. The enumerated baseline
probabilities also matched `IsingSampler::IsingStateProb()` within `3.33e-16`.
The two-event bootstrap-failure diagnostic was independently rerun and again
stopped with 9/19 failures. This revalidation exited successfully; its script
hashes, versions and checks are stored in `clean-revalidation.json` and `.rds`.

To revalidate those saved artifacts without repeating the long simulations:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1 \
  Rscript --vanilla tools/revalidate-intervention-results.R
```

Source references: [NodeIdentifyR simulation](https://github.com/JasperNaberman/nodeIdentifyR/blob/22ceb4c9c19d6c95a4030ea4ce13d8545cbbfdb3/R/simulateResponses.R),
[NIRApost permutation test](https://github.com/kingfly51/NIRA_post/blob/6231832736df7c693b630820729f73f64c30ec92/R/permutationTest.R),
[NIRApost moderation screen](https://github.com/kingfly51/NIRA_post/blob/6231832736df7c693b630820729f73f64c30ec92/R/runMgmmAnalysis.R).
The independently initialized, sequential conditional updates were checked in
the [IsingSampler C++ source](https://github.com/SachaEpskamp/IsingSampler/blob/25947d1c3e6eb2eb2cd0a65fd3b936eea72efc88/src/IsingCpp_CFTP.cpp).
The supplied SymPerturb source specification and earlier independent Python
comparisons are documented in [symperturb-validation.md](symperturb-validation.md).
