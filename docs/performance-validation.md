# Workflow performance baseline

Date: 2026-09-20. Linux R 4.5.3 on WSL2; BLAS/OpenMP limited to one thread per
process. Each configuration uses three fresh R processes and fixed seeds.
This is a measured baseline for the specified workload; it is not a complexity
or capacity guarantee. Bilingual full-workflow measurements are recorded in
[workflow validation](workflow-validation.md).

| Method | Size | Workers | Analysis seconds, median | Peak process-session RSS, median MiB |
|---|---|---:|---:|---:|
| NIRA | small | 1 | 8.622 | 369.3 |
| NIRA | small | 2 | 5.333 | 608.2 |
| NIRA | medium | 1 | 34.686 | 370.6 |
| NIRA | medium | 2 | 17.676 | 609.2 |
| SymPerturb | small | 1 | 0.724 | 364.8 |
| SymPerturb | medium | 1 | 0.978 | 365.6 |

Small has 100 participants and six nodes; medium has 300 participants and eight
nodes. The NIRA benchmark uses the literature engine with 100 sweeps, 99
permutations, and respectively 300/500 simulated observations and 10/15 rank
stability repetitions. Moderation is disabled solely to time the fixed-network
simulation stages; the complete moderation workflow is tested in the README and
intervention audit. SymPerturb runs complete scoring, scenarios, sequence length
two and respectively five/ten participant bootstraps. These settings are explicit
benchmark arguments; package defaults are unchanged.

Analysis time excludes package loading and initial network fitting; the CSV also
records complete-process elapsed time. Memory includes R startup, package loading
and retained data/results. The Linux harness samples RSS every 0.1 seconds across
the isolated process session, including PSOCK workers reparented after launch.
Shared pages are counted per process, so this is summed RSS rather than unique
physical RAM. Three repeats provide a descriptive baseline; machine load and
filesystem caching remain possible influences.

Reproduce from the package root:

```sh
python3 tools/benchmark-workflows.py
```

The harness imposes a 180-second per-case deadline; incomplete attempts are not
entered as measurements. `--method nira|symperturb --resume` continues missing
cases while retaining completed rows. Records are in
`../output/audit/performance/measurements.csv`, alongside machine information,
per-case output and logs. The initial runtime pilot is retained separately in
`../output/audit/performance-pilot/`.

One medium PSOCK attempt did not finish and was excluded. Its process/socket
snapshot and the bounded native IsingSampler/complete-NIRA diagnostic are in
`../output/audit/performance/psock-diagnostic/`. Both diagnostic calls and all three
subsequent formal repeats completed. The original interruption's cause remains
unconfirmed; no package algorithm was changed in response.
