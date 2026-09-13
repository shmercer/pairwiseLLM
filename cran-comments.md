## Release preparation: pairwiseLLM 1.5.0

This preparation adds downstream rubric calibration of completed Bayesian CJ
results and raises the minimum R version to 4.4. Ordinal modeling backends remain
optional in Suggests: ordinal, and mgcv >= 1.9-4 (with withr for monotone fitting).
Existing Phase B linking algorithms are unchanged.

## Current local evidence (2026-09-12)

On local R 4.6.1, with ordinal 2026.7.26 and mgcv 1.9-4:

- Focused documentation tests: 499 expectations passed; one pre-existing
  historical export-audit test skipped because its optional evidence file is
  absent. Current rubric navigation is independently tested.
- Rubric documentation/example tests: 44 expectations passed, including actual
  deterministic accepted Phase B results. No failures or warnings.
- Roxygen generation, three changed vignette renders, README regeneration,
  targeted pkgdown pages, and package lint passed.
- A source archive built with `R CMD build --no-build-vignettes --no-manual`.
  Its metadata and contents were reviewed; this is not a submission-ready build
  with all vignettes rebuilt and is not evidence of `R CMD check` success.

## Validation still pending

Full tests, `devtools::check()`, `covr::package_coverage()`, final website/manual
review, and current-state platform CI remain for the maintainer. Local execution
on the minimum R 4.4 version was not performed. No current-state Winbuilder,
R-hub, reverse-dependency, or CRAN acceptance claim is made. Earlier-version
check results are not evidence for this release preparation.

## Examples, optional software, and storage

New documentation snippets begin with completed CJ inputs; collection and
sampling are not performed by rubric scoring. Dependent vignette examples are
unevaluated in ordinary builds, with downstream code checked using deterministic
offline fixtures. Tests require no provider credentials. Optional modeling
packages are checked at use; no installations or backend substitutions occur.

Rubric runtime introduces no file writes or downloads. Test serialization uses
temporary directories. CmdStan and its C++17 toolchain remain optional upstream
requirements for Bayesian CJ fitting; Python remains optional for warm-start
feature extraction. Website asset downloads during the local pkgdown build were
separate from package examples/tests. Source-archive review found no compiled
executables, credentials, planning files, or caches.
