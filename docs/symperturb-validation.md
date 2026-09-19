# SymPerturb algorithms, interface migration and validation

quickNet's continuous `Perturbation()` methods implement the revised method
specification of the locally supplied SymPerturb 0.1.0 reference, commit
`76dd4178b285b80beb69f14a642e84ed1cabc7a0`. The R implementation is independent
of a Python runtime. Ising and NIRA methods retain their existing algorithms.

## Algorithm coverage

| Component | Implemented behavior |
|---|---|
| Network estimation | Original-data means and sample covariance plus `ridge * diag(S)`, inverse/pseudoinverse, partial correlations; edge thresholding applies only to topology. |
| State intervention | Linked, location-only, scale-only and independent location/scale maps, anchors, complete post-intervention mean/covariance, and explicit zero-variance knockout endpoints. |
| Observation model | Winsorised-normal expectation for bounded scales, or unbounded means; improvement standardized by baseline SD. |
| Downstream benefit | Weighted mean on non-target outcomes; direct benefit, beneficial spillover and adverse spillover remain separate. |
| Communication blocking | Edge/node adjacency attenuation; finite-step propagation; raw, row or spectral normalization; signed/unsigned propagation and spectral-radius diagnostic. |
| Combination | Unit-dose joint benefit minus the better single-target benefit on the same non-target set; signed values by default; efficacy-based partner selection in full analysis. |
| Sequence | Efficacy candidate pool from the full scored table, beam search with discount and costs, all retained final sequences, and their stepwise paths. |
| VPPS | Seven utilities, candidate-set min-max normalization, neutral score 50 for constant dimensions, weighted aggregate and minimum ranks for ties. |
| Robustness | Thirteen sensitivity scenarios, population rank SD, separate robustness diagnostic and retained scenario ranks. |
| Bootstrap | Participant resampling followed by network refitting, perturbation, utility calculation, candidate-set normalization and ranking in every replicate; intervals, rank draws and top-k probabilities. |

The seven utilities are efficacy, dose efficiency, breadth, cross-module reach,
communication block, combination value and responsiveness. Robustness is not
included in VPPS. These are model-derived priorities, not identified causal
treatment effects. Sequence order is a decision-objective result, not an
identified biological time ordering.

## Input and interface migration

- Original finite numeric participant data must be available in `fit$data`,
  with at least three participants and three symptoms. The continuous
  intervention model is re-estimated independently of the original fitted graph.
- Defaults follow the reference: ridge `.02`, topology threshold `.03`, bounds
  `[0,4]`, and dose grid `c(0,.10,.25,.50,.75,1)`. Set
  `config = list(bounds = NULL)` for unbounded outcomes.
- `symperturb` and `sequence` require a named module mapping covering every
  symptom and containing at least two modules. A character/factor `groups`
  vector saved with the fit can provide the mapping.
- `remaining_strength` is a state-retention alias for `1 - dose` in knockdown;
  it no longer attenuates precision entries. Supply either argument, not both.
- `knockout` returns the state intervention. Use `node_block` for the separate
  topology operation; the extra structural-knockout row has been removed.
- State rankings use `system_benefit`, the weighted standardized non-target
  improvement. Raw `burden_reduction` remains a descriptive quantity.
- Combinations use `incremental_pair_value`, replacing additive `synergy`.
  The reference comparison requires two targets and unit dose.
- Blocking uses `communication_block`, the relative finite-step propagation
  loss. Non-NULL `pulse_values` or `spillover_nodes` raises a migration error.
- Sequence primary tables contain the final beam candidates. Their individual
  steps are available in `sequence_paths`.
- `summary()` returns the primary metrics. `quicknet_report()` retains the
  additional analysis tables. Plot labels use the revised metrics; node-change
  plots show one condition, selected explicitly with `perturbation_id` when
  needed.
- Config names match the reference. Explicit R arguments `dose`, `threshold`,
  `seed` and `steps` override their corresponding config entries as documented
  in `?Perturbation`.

For identical cross-language bootstrap calculations, provide the same
`bootstrap_indices`: a matrix with one row per replicate and one column per
participant, containing one-based participant indices. R and NumPy use different
random-number generators, so equal integer seeds alone do not produce equal
resamples. Ordinary R bootstrap runs remain reproducible with their configured
seed and preserve the caller's random state.

## Numerical evidence

The checked-in [fixtures](../tests/testthat/fixtures/README.md) were generated
by running the Python reference, independently of the R implementation. Seven
configurations cover bounded linked defaults, unbounded linked, location-only,
scale-only, independent powers, a restricted candidate set, and tied zero state
effects. Additional cases cover singular target covariance blocks and constant
or nearly constant normalization dimensions.

Regression comparisons use tolerance `1e-10` for moments/topology and `1e-8`
for complete result tables. Shared bootstrap resamples test both per-replicate
outputs and summaries. Interface tests cover summary, plots, reports,
configuration precedence, migration errors and invalid input.

The Python package's supplied 12-node example was also run end to end, including
13 sensitivity scenarios, 25 shared bootstrap resamples, sequence length 3 and
beam width 20. All seven result tables agreed:

| Result table | Rows | Maximum absolute numeric difference |
|---|---:|---:|
| target_scores | 12 | 6.6791e-13 |
| dose_response | 72 | 2.28983e-16 |
| pair_scores | 45 | 3.88578e-16 |
| robustness | 12 | 3.33067e-16 |
| bootstrap | 12 | 1.2168e-13 |
| sequence | 20 | 4.44089e-16 |
| network_edges | 51 | 4.71845e-16 |

The reference's seven existing pytest tests and example CLI passed. Four public
R plot types were rendered and inspected. The final R package check passed all
1,552 assertions with zero failures, warnings or skips; `R CMD check --no-manual`
reported **0 errors, 0 warnings, 0 notes**. This includes installation, loading,
namespace/dependency checks, S3 consistency, static checks, Rd documentation,
code/documentation parameter agreement, examples and the complete test suite.
Only PDF-manual compilation was omitted because LaTeX was unavailable.

These checks establish numerical agreement with the specified local reference
on the tested cases. They do not constitute a repeat of every manuscript
simulation or a validation of causal treatment effects.

## Reproducing the checks

From the quickNet repository root, with R test dependencies installed:

```sh
Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_local(".", reporter="summary")'
R CMD build .
R CMD check --no-manual quickNet_0.0.0.9000.tar.gz
```

To regenerate numerical oracles, obtain the same reference version and install
its Python dependencies, then run:

```sh
PYTHONPATH=/path/to/SymPerturb/src python tools/generate-symperturb-reference.py
```

The fixture generation environment used Python 3.10, NumPy 2.2.6, pandas 2.3.3
and SciPy 1.15.3. Python is not needed for ordinary package use or for executing
the stored R regression tests.

Reference: Zhu, Z., Yu, J., Hu, T., Yang, Z., & Wang, J. (2026). *SymPerturb
converts symptom-network structure into testable intervention priorities*.
arXiv:2607.28673v1; revised method specification and SymPerturb 0.1.0.
