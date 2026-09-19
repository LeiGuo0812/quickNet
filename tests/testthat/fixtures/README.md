Numeric test oracles for the continuous SymPerturb implementation.

- Reference: locally supplied SymPerturb 0.1.0, commit
  `76dd4178b285b80beb69f14a642e84ed1cabc7a0`.
- Generator: `tools/generate-symperturb-reference.py`, executed with the reference
  source directory on `PYTHONPATH`. The fixtures contain synthetic participant
  data and numerical outputs, not the reference package's source code.
- Generation environment: Python 3.10, NumPy 2.2.6, pandas 2.3.3, SciPy 1.15.3.
- Cases: bounded linked defaults, unbounded linked, location-only, scale-only,
  independent powers, a restricted candidate set, and tied zero state effects.
- Coverage: Gaussian network estimation; single, joint, zero-dose and exact
  endpoint moments; bounded observed expectations; all seven utilities and VPPS;
  signed/positive pair aggregation; sequence costs, discount and beam search;
  13 robustness scenarios; full-pipeline bootstrap with identical resampled
  participant indices; signed/unsigned raw, row and spectral propagation;
  singular target blocks; constant and near-constant min-max normalization.
- R tests compare moments/topology to tolerance 1e-10 and complete result tables
  to 1e-8. No Python installation is required to execute the R tests.
- Bootstrap indices are saved as one-based indices for R. Identical integer
  seeds alone do not synchronize NumPy's PCG64 generator and R's RNG.
