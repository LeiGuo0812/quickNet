# Independent README examples

Every analysis R code block in the English and Chinese READMEs is self-contained:
load quickNet, construct its input data, perform the analysis, and inspect a
returned table, summary, report or plot. Each complete block can be copied into a
fresh R session after installing its dependencies. Earlier examples do not supply
objects or helper functions to later ones.

Sample-size planning begins with an explicit data-generating design and candidate
sample sizes; the planning function generates the simulated observations. The
powerly example constructs a known population network directly.

## Running the documented examples

From the package root, run both languages:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 Rscript --vanilla tools/validate-workflows.R
```

To run one language and choose its artifact directory:

```sh
Rscript --vanilla tools/validate-workflows.R README.zh-CN.md ../output/audit/workflows-independent/README.zh-CN
```

`--check-only` checks extraction and bilingual code parity without executing
analyses. `--blocks=4,6` selects analysis blocks for diagnosis; the resulting
summary explicitly identifies a partial run.

The validator extracts the actual R fences throughout the README, including the
output overview, model registry, input checks and parameter examples. Only the
two installation blocks are excluded; they install the package and are not
analysis examples.

Each block runs in its own new `Rscript --vanilla` process. The worker loads the
current package source before evaluating the unmodified example; it does not
create demonstration data, reuse earlier fits or override documented budgets.
English and Chinese blocks are compared as parsed R expressions. A displayed
missing result field fails validation. Retained fitted networks must contain
finite matrices and produce a nonempty report; explicitly failed backend
statuses fail the check. The export example must create nonempty plot, CSV and
report files, and its own code reads the table and text report back.

## Demonstration data and budgets

Continuous examples generate numeric data with specified shared factors. Binary
examples generate a four-node pairwise Ising model. Panel examples construct
participant intercepts and adjacent-wave effects; intensive longitudinal examples
construct participant means, multiple days and explicit measurement indices.
Confirmatory and latent examples create their own factor data and model syntax.
Meta-analysis examples generate independent study matrices or long-form records.
Every helper function needed for generation is defined inside the same block.

Small simulation counts and tuning grids are explicit demonstration arguments.
They leave package defaults unchanged. For research use, choose appropriate
analysis budgets and consult the method references linked in the README.

## Records and interpretation

The validator retains extracted example code, per-block execution logs, native
warnings, fitted-object checks, plots, exported files, source/README hashes and
R/dependency versions. Timing and R allocation are recorded per block. Linux
process peak RSS includes startup and package loading; R allocation totals count
repeated allocation rather than resident memory. Each block uses a separate
process, so memory is not cumulative across the manual. Windows RSS is unavailable
in this harness.

Independent-run artifacts use `../output/audit/workflows-independent/` on Linux
and `../output/audit/workflows-independent-windows/` on Windows. They are separate
from the earlier sequential-run artifacts. The compatibility CI invokes the same
validator for both languages in each complete dependency job.

## Independent execution record

On 2026-09-20, each final README contained 36 analysis blocks and two excluded
installation blocks. The following final records passed:

| Environment | Language | Independently executed blocks |
|---|---|---:|
| Linux, R 4.5.3 | English | 36/36 |
| Linux, R 4.5.3 | Chinese | 36/36 |
| Native Windows, R 4.5.0 | English | 36/36 |
| Native Windows, R 4.5.0 | Chinese | 36/36 |

These are 144 successful independent example executions. Every block's saved
code was compared with the final README, allowing only platform newline
differences. README, runner and package-source hashes were also checked. Each
language/platform run produced the documented export files and passed its
fitted-result checks. Dependency versions and native messages remain in the
per-block records. The consolidated manifest is in workspace
`output/audit/workflows-independent/final-validation-manifest.json`.

Initial English runs identified two calls passing an unsupported plotting
argument to `EBICglassoNet()`. Those examples now use its estimation interface;
both complete English runs were repeated. Initial diagnostics remain separately
under `../output/audit/workflows-independent-initial/`. The final Chinese runs
used the corrected code. macOS execution is left to the compatibility CI.

## Earlier sequential validation

The previous manual was tested by running 32 blocks in sequence in each language
on Linux and native Windows. Those four runs allowed later examples to use
previously generated objects; they do not establish independent execution.
Their unchanged logs remain under `../output/audit/workflows/` and
`../output/audit/workflows-windows/`. The original record is available at
[commit 4ea2f0b](https://github.com/LeiGuo0812/quickNet/blob/4ea2f0b42b2a3b60413b6467cc21295bb1f9acac/docs/workflow-validation.md).
