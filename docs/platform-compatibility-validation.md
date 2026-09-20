# Platform and dependency compatibility

This audit separates an observed dependency baseline, an automatically updated
dependency track, and a minimal installation. A version recorded here is an
environment that can be tested; it is not a claim that every combination of
versions above it is supported. No untested minimum dependency requirements
have been added to `DESCRIPTION`.

## Recorded environments

The baseline is R 4.5.3 on Linux. The complete observed dependency closure is
recorded in [`tools/compatibility-baseline.csv`](../tools/compatibility-baseline.csv):
209 package versions, including hard transitive dependencies, the declared
Suggests, package-check tooling, and standard R packages. The CSV is a fixed
version snapshot, not a frozen compiler, operating-system, or system-library
image. Base-package versions follow the fixed R version. Other versions are
requested explicitly in the baseline CI track and checked after installation.

An independently available Windows installation supplies a real alternate
environment with older R and backend versions. It is not labeled "latest".
The following representative versions identify the environments; the checker
also exports all visible package versions and library paths.

| Component | Linux observed baseline | Windows alternate environment |
|---|---|---|
| R | 4.5.3 | 4.5.0 |
| bootnet | 1.9.1 | 1.8 |
| qgraph | 1.10.1 | 1.9.8 |
| glmnet | 5.0 | 4.1-10 |
| lavaan | 0.7-2 | 0.6-21 |
| psychonetrics | 0.16.9 | 0.15 |
| graphicalVAR | 0.4.1 | 0.3.4 |
| mlVAR | 0.7.3 | 0.7.3 |
| powerly | 1.10.0 | 1.10.0 |

The Windows check exposed a concrete difference: psychonetrics 0.15's GVAR
constructor does not accept `centerWithin`. The adapter now obtains this
default from the actual backend when available, omits the inactive FALSE
setting when the older backend does not support it, and rejects a requested
TRUE setting with an explicit diagnostic. This compatibility case has a
dedicated regression test.

## Minimal installation

The installed Linux baseline has 165 packages in the recursive hard dependency
closure of quickNet's Imports, including standard R packages. Although
`jsonlite`, `glmnet`, `lavaan`, and `MASS` also appear under quickNet's Suggests,
they are required by the installed core dependency chain. Removing them would
create an invalid dependency environment.

`graphicalVAR`, `mlVAR`, `powerly`, `psychonetrics`, and `testthat` are outside
that hard closure in this environment. The minimal audit made those packages
unavailable and exposed only the hard dependency closure and standard R
library. Linux dependencies were linked into an isolated library from the
already installed packages; quickNet itself was freshly installed there.
This checks package isolation and optional dependencies, not a fresh download
or source rebuild of every dependency.

The minimal smoke check runs without testthat. It verifies core fitting,
reports and a saved network plot, then checks that calls requiring unavailable
optional backends identify the required package. A separate minimal
`R CMD check --no-manual --no-tests` examines installation, loading, code,
documentation and runnable examples with `_R_CHECK_FORCE_SUGGESTS_=false`.
Its report identifies the intentionally unavailable Suggests (INFO in the
tested R 4.5.3 run; other R versions may report a note). The normal testthat suite belongs to
the complete dependency profile and is not reported as having run in the
minimal profile.

## Reproducible compatibility checker

After installing quickNet, run from the source-package root:

```sh
Rscript tools/check-compatibility.R complete baseline path/to/results
Rscript tools/check-compatibility.R complete current path/to/results
Rscript tools/check-compatibility.R minimal current path/to/results
```

`baseline` requires R 4.5.3 and the applicable exact CSV versions. `current`
means that installed versions are inspected without enforcing that snapshot;
it does not mean that they are the newest available versions. `minimal`
requires a pre-isolated library and removes site-library paths before checking
availability. It rejects optional packages outside the hard closure when
those packages remain visible.

The checker verifies behavior that is particularly sensitive to dependency
structure:

- qgraph estimation/plot argument routing, including plot controls stored in
  its internal whitelist, plus an actual saved plot.
- bootnet's effective missing-data default and a real EBICglasso fit.
- lavaan's robust estimator label and the S4 options used for extraction.
- psychonetrics' fitted S4 slots, effective estimator metadata and network
  extraction in the complete profile.
- Missing-backend diagnostics for each independently optional model package
  in the minimal profile.

Outputs include `compatibility-report.txt`, `dependency-versions.csv`,
`installed-packages.csv`, `session-info.txt`, and `network-smoke.pdf`.
These smoke checks supplement the full test suite; they do not replace
model-specific numerical comparisons.

## Objects saved on Windows and read on Linux

[`tools/validate-cross-platform-objects.R`](../tools/validate-cross-platform-objects.R)
consumes the real Windows RDS objects generated by
[`tools/validate-legacy-objects.R`](../tools/validate-legacy-objects.R).
The fixtures cover source commits `29a414b` and `72666a3`, plus the current
implementation, with six cases per source: EBICglasso, EBICglasso with missing
observations, Ising, MGM, partial correlation, and CLPN.

Each source label is checked in separate Linux R processes. A historical
source process directly reruns the original constructor with its original
arguments and the resampled data saved by Windows. A current source process
then reads the Windows object, exercises `print`, `summary`, reporting, and
actual PDF plotting, verifies the stored graph and network layers remain
unchanged, and checks a second RDS roundtrip. It refits with recovered settings
and compares every layer with the independently produced Linux historical
reference. Applicable NCT refits are compared with that same reference.

The reference deliberately uses the Linux backend versions. A historical
Windows numerical result is recorded for comparison, but exact equality with
it is not a requirement when backends have changed. This separates object
portability and parameter recovery from numerical changes in external
estimators. The fixed historical source hashes and constructor argument lists
are retained with the validation output.

Run from the package root after generating the Windows fixtures:

```sh
Rscript --vanilla tools/validate-cross-platform-objects.R \
  ../output/audit/legacy-objects-windows ../output/audit/cross-platform-objects
```

All 18 cases passed display, graph preservation, serialization and refit
checks. Maximum absolute differences from the Linux historical reference
were `1.39e-16` for the two historical source groups and zero for the current
source group, below the prespecified `1e-8` comparison tolerance. Warnings
about dense EBIC-selected networks were captured in the results rather than
discarded. These fixtures provide evidence for the six exercised model paths;
they do not establish portability for every backend's serialized object class.

## Continuous checks

The [compatibility workflow](../.github/workflows/compatibility.yaml) defines:

| Job group | Platforms | R and package selection | Checks |
|---|---|---|---|
| Complete baseline | Windows, Linux, macOS | R 4.5.3, exact recorded package versions | Installed-package smoke, full `R CMD check --no-manual`, reject skipped tests |
| Complete latest | Windows, Linux, macOS | R release, upgraded dependencies | Same full checks |
| Minimal latest | Windows, Linux, macOS | R release, upgraded hard dependencies plus check tooling | Optional-backend absence/diagnostics, `R CMD check --no-manual --no-tests` |

The workflow runs on pushes to main, pull requests, a weekly schedule, and
explicit workflow dispatch. It installs the committed source, exports
environment/check artifacts, limits BLAS/OpenMP threads, and has read-only
repository permissions. Complete jobs fail on check errors, warnings, notes,
or skipped testthat cases. Minimal jobs fail on errors or warnings and retain
informational messages and notes for review, including unavailable Suggests.
Dependency-installation failures are not counted as successful compatibility
tests.

Version selection follows the documented
[setup-r](https://github.com/r-lib/actions/tree/v2/setup-r),
[setup-r-dependencies](https://github.com/r-lib/actions/tree/v2/setup-r-dependencies),
and [pak installation](https://pak.r-lib.org/reference/pkg_install.html)
interfaces. The latest track requests dependency upgrades explicitly; it
does not equate a cached or older binary installation with latest-source
coverage.

## Earlier compatibility validation snapshot

The counts and archive below belong to the completed power/compatibility audit,
before the subsequent A–E/H changes. The [remaining-validation summary](remaining-validation-summary.md)
records the final checks for those later changes.

- Linux observed baseline: fresh quickNet installation in both complete and
  isolated minimal libraries passed. Both compatibility smoke profiles passed;
  complete-profile version checks matched the recorded snapshot. The minimal
  profile confirmed the five independently optional packages were absent and
  all four model-backend diagnostics were correct.
- Linux minimal `R CMD check --no-manual --no-tests`: installation, loading,
  static checks, documentation and runnable examples passed, with 0 errors,
  0 warnings and 0 notes. The five unavailable optional packages were listed
  as INFO. Testthat was intentionally absent and its suite was skipped by
  the explicit `--no-tests` option.
- Final complete Linux package: `R CMD check --no-manual` passed with 0 errors,
  0 warnings and 0 notes; all 2,556 assertions passed with no test warnings or skips.
  The installed final package also passed the complete baseline compatibility checker.
- Final complete native Windows package: installation, the compatibility
  checker and `R CMD check --no-manual` passed with 0 errors, 0 warnings and
  0 notes; all 2,544 assertions passed with no test warnings or skips. The 12
  assertion difference consists of historical-fixture numerical comparisons
  that run only when creation and consumption backend versions match. The
  separate Windows-to-Linux checks cover actual backend upgrades.
- The same final source archive passed the isolated Linux minimal checker and
  `R CMD check --no-manual --no-tests`, with 0 errors, 0 warnings and 0 notes.
  This intentionally does not run the testthat suite.
- macOS: no local runtime was available. The workflow is configured, but no
  macOS result or remote CI success is claimed before those jobs execute.
- Workflow YAML, embedded R steps, and the standalone checker parsed locally.
  At the time of this local audit, the workflow had not been run remotely.

Final check logs and the archive/source hashes are retained under workspace
`output/audit/compatibility/`: `linux-complete-check`,
`windows-complete-check`, `linux-minimal-final-check`, and
`final-package-manifest.json`. The checked archive's R code, help files and
tests were verified against the working source. These results apply to that
source snapshot, not to a future dependency upgrade before it is checked.
