# quickNet 0.0.0.9000

## New features

- Added the exported `NIRA()` single-network Ising simulation-intervention
  workflow described by Wang et al. (2026).
- Added moderation prerequisite gating, adjusted plus-one permutation tests,
  repeated-simulation rank stability, reproducible L'Ecuyer-CMRG task
  substreams, and literature-compatible and native simulation engines.
- Added the `quicknet_nira` S3 object with print, summary, plot, and
  `quicknet_report()` support.
- Added `Perturbation(method = "nira")` without changing the existing default
  or behavior of `method = "ising_threshold"`.
- Documented that all NIRA outputs are fixed-parameter model projections, not
  causal or clinical treatment effects.
