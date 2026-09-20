## Release preparation: pairwiseLLM 1.5.2

This release completes issue #259: the additive 46-feature writing_features_v2,
shared nested-CV plans, optional PLS/RBF-SVR engines, and a distinct equal-weight
same-task algorithm ensemble. Full/reduced portable artifacts support prediction,
registries and BTL/TrueSkill initialization without fitting backends. Existing
v1/glmnet behavior, legacy formats 1/2, cross-task ensembles and prior SD authority
are preserved. R >= 4.4 remains required; optional engines remain in Suggests.

The warm-start guide now follows a complete offline example with fabricated texts,
actual precomputed v2 features, and invented outcomes. Extraction can be repeated
with the existing pinned environment. No production models are fitted or bundled.

### Current Phase 7 evidence

On local R 4.6.1, the final full source suite passed 20,667 expectations with
zero failures/errors, one in-test historical audit skip, three top-level legacy
mode skips, and 31 serialization warnings reproduced on unchanged reviewed master.
The full CRAN-style check reported zero errors/warnings and one environmental NOTE
(missing HTML Tidy and V8); installed-package tests passed 19,672 expectations with
49 documented skips. A final archive refresh with --no-tests repeated examples,
package and vignette checks with the same result while the full source suite ran.

Current whole-package covr line coverage is 97.26398%; all 27 warm-start R files
meet the >=95% target (minimum 95.23810%). Package lint, Roxygen/Rd validation,
README and guide renders, full pkgdown build, local links, actual pinned Python
extraction and fresh-process backend-free deployment checks passed. Runtime R
expressions are unchanged by this documentation phase. The Phase 7 validation
report and ledgers record exact candidate SHAs, source/archive identity, dependency
versions, skips, remaining uncovered lines and the recovered coverage-reporting
error. Earlier records below are historical and do not validate this candidate.

### Pending release review

Final candidate CI and maintainer review remain pending user-supplied results.
No merge, tag, CRAN submission, publication or downstream study fitting/replay is
part of this phase. A later merge SHA is a distinct downstream D042 pin requiring
identification and revalidation. The existing regular macOS CI fix is retained;
local Linux checks do not establish cross-platform Python compatibility.

## Historical 1.5.1 preparation

## Release preparation: pairwiseLLM 1.5.1

This patch fixes ignored `store` values for both OpenAI live endpoints and
Gemini Developer API live and batch requests (#245). Explicit logical values
are validated and forwarded; omission or NULL preserves provider/project
settings. Public signatures, output schemas, R >= 4.4, and dependencies remain
unchanged. These provider-specific controls are not general retention guarantees.

## Current local evidence (2026-09-15)

On local R 4.6.1:

- Nine focused test files: 1,335 passed expectations, zero failures, errors, or
  warnings. Four existing tests skipped (three require PSOCK sockets and one
  requires an unavailable historical audit file); two existing top-level socket
  guards also produced skip notices. New storage regressions all executed.
- Scoped covr evidence from the focused tests: openai_live.R 95.37%,
  gemini_live.R 98.62%, gemini_batch_api.R 96.35%. This is file coverage, not a
  package-wide coverage result.
- Roxygen and README regeneration, metadata/hash checks, and nine targeted
  pkgdown reference pages passed. Reference examples were disabled; tests use
  mocked transport and no provider credentials.
- Package lint returned zero findings; git diff --check and numeric-prefix
  uniqueness checks passed.

## Validation still pending

Full tests, devtools::check(), package-wide coverage, current-state platform CI,
minimum-R execution, and maintainer release review remain pending. No package
build, CRAN submission, tag, publication, or provider smoke call was performed.
The 1.5.0 record below is historical and is not current patch validation.

## Historical 1.5.0 preparation

### Release preparation: pairwiseLLM 1.5.0

This preparation adds downstream rubric calibration of completed Bayesian CJ
results and raises the minimum R version to 4.4. Ordinal modeling backends remain
optional in Suggests: ordinal, and mgcv >= 1.9-4 (with withr for monotone fitting).
Existing Phase B linking algorithms are unchanged.

### Current local evidence (2026-09-12)

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

### Validation still pending

Full tests, `devtools::check()`, `covr::package_coverage()`, final website/manual
review, and current-state platform CI remain for the maintainer. Local execution
on the minimum R 4.4 version was not performed. No current-state Winbuilder,
R-hub, reverse-dependency, or CRAN acceptance claim is made. Earlier-version
check results are not evidence for this release preparation.

### Examples, optional software, and storage

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
