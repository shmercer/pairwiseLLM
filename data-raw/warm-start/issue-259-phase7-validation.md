# Issue 259 Phase 7 validation and release preparation

## Candidate and scope

Version **1.5.2**, retaining **R >= 4.4**, on `feat/259-07-docs-quality`, targeting
`master`. Reviewed predecessor: Phase 6 PR #265 final head
`8717cabb60f618a34591ecb2203d4e124148e951`, merged as
`1a91ff9359dd5c9a1bbd1ff37d7c55b489b52594` with an identical tree. Subsequent
reviewed macOS CI fix PR #266 merged as base
`9c8383d8875116b7ed49912da964a61b6be9bfb6`; the user reported all seven checks
passing on its final head `dbef186c85c8863f0827a5ba42c21b93f71df3a3`.
No outstanding review fixes were omitted and no pending CI was polled.

Documentation implementation: `f2acfbbbea7c275f4606d996af6b194f14e4cbf4`.
Final package-content candidate: `93a9142122b1588939796e311c7ac501da822c5f`.
The later reporting head is recorded exactly in the PR and local tasklists;
its package-source identity is checked against this candidate. Reporting files
under data-raw, local tasklists and cran-comments are excluded from the archive.
A later merge is a distinct D042 pin: see
[CI follow-up and pin validation](issue-259-phase7-ci-next-thread.md).

The full warm-start guide now connects actual v2 extraction, one shared CV plan,
all three fits, held-out validation, same-task equal-weight averaging, new-item
prediction, explicit prior SD, and BTL/TrueSkill/both initialization. The related
adaptive-pairing guide explains the choices in the same plain language. Both
retain executable offline examples, with optional dependencies guarded.
The example contains 48 distinct fabricated texts, actual pinned v2 feature
values and invented outcomes, with no production model or student data. Training
and prediction texts/IDs are disjoint. The explicit generator refuses overwrite.
The demonstration uses small labeled grids; frozen production grids are unchanged.

Cross-task `ensemble_warm_start_models()` retains duplicate-component handling and
component-only validation. The separate same-task constructor requires complete
format-3 identity/alignment evidence and averages calibrated deployment means
with equal weights. Full/reduced artifact semantics, backend-free deployment,
nonlinear coefficient errors, registry/bundle identity and user-chosen prior SD
are documented without suggesting learned weights, ensemble recalibration,
automatic selection or disagreement-derived uncertainty. Numeric audit tolerance
does not relax discrete identity or aligned observations.

All 11 edited runtime R files have identical parsed executable expressions to the
reviewed base. Only their Roxygen changed; 11 generated Rd files changed and
NAMESPACE is unchanged. No provider, adaptive selection/stopping, Phase B,
production model, legacy fixture or study changes were made. Optional engines
remain Suggests and no dependency installation was initiated.

## Validation commands and evidence

All checks used existing local dependencies on R 4.6.1, x86_64 Linux (Pop!_OS
24.04). [Dependency ledger](issue-259-phase7-dependencies.csv) records all package
dependencies and check tools. Fitting backends: glmnet 5.0, pls 2.9.0,
e1071 1.7.17. Existing pinned Python 3.12.3 used spaCy 3.7.5,
textdescriptives 2.8.4 and textstat 0.7.13. No claim is made about another platform.

Focused source checks used the existing pinned interpreter through
`PAIRWISELLM_TEST_PYTHON` and
`testthat::test_local(filter="^(0026|0031|9106)-", reporter="summary", stop_on_failure=TRUE, stop_on_warning=TRUE)`:
**736 passed expectations**, zero failures/errors/warnings, one existing missing
historical tasklist audit skip. [Focused ledger](issue-259-phase7-focused-results.csv).
New test 9106 executes the real guide chunks and checks exact shared partitions,
aligned outer metrics, disjoint prediction items, authoritative prior SD,
common bootstrap, TrueSkill mapping and actual Python re-extraction.

The initial full `devtools::test()` on the documentation commit found one stale
`1.3.1` assertion in test 0027: the runner records its actual installed runtime
version. The test now captures that original version, corrupts it, and checks
that resume restores it exactly. It also checks a valid release-version shape.
No provider runner or historical provider evidence changed. The corrected test
file passes all 37 expectations without warnings. Full rerun results follow below.

Final full source command:
`Rscript --vanilla data-raw/warm-start/check-issue-259-phase7.R tests OUTPUT_DIR`
(with the existing pinned interpreter selected). This invokes unfiltered
`devtools::test()`. On `93a9142122b1588939796e311c7ac501da822c5f`, it completed
successfully: **1,686 test blocks; 20,667 passed expectations; zero failures or
errors; one in-test skip; 31 warnings**. The summary also reports three existing
top-level skips for removed linking/transform modes. The in-test skip is the
unavailable historical tasklist audit. [Full results](issue-259-phase7-test-results.csv)
and [session/HEAD](issue-259-phase7-tests-session.txt) retain the evidence.

All 31 warnings say `'package:pairwiseLLM' may not be available when loading`
during source-session serialization. An isolated archive of unchanged reviewed
master `9c8383d8875116b7ed49912da964a61b6be9bfb6` reproduced exactly the same
warnings in narrow baseline runs: test 3105 (5), 9102 (8), 9103 (18), with
405 passed expectations and no failures/errors. The
[baseline warning ledger](issue-259-phase7-baseline-warning-results.csv) records
that attribution. No new package warning was hidden or dismissed without a
baseline check. The runner retains warnings for review and fails on errors or
failed expectations; CRAN-style checks separately reported zero warnings.

Coverage command (with the pinned interpreter selected):
`Rscript --vanilla data-raw/warm-start/check-issue-259-phase7.R coverage OUTPUT_DIR`.
It runs current `covr::package_coverage(type="tests")`, not a stale focused ledger.
Current whole-package line coverage is **97.26398%** (38,109/39,181 measured
lines). All **27 warm-start R files**, including all 22 changed across issue #259,
meet the >=95% target: **95.23810%–100%**. Phase 7's documentation-only
adaptive_rank/adaptive_run files measure 100% and 97.30435%, respectively.
The [complete per-file ledger](issue-259-phase7-coverage.csv) records actual counts.
There are no unmet coverage targets among issue #259's new/materially affected
files. The only package file below 95% is unchanged adaptive_btl_refit.R at
93.45563%; extending unrelated Phase B coverage is outside this phase.

The coverage run belongs to `f2acfbbbea7c275f4606d996af6b194f14e4cbf4`;
[session evidence](issue-259-phase7-coverage-session.txt) preserves that exact pin.
Every R source file is byte-identical at `93a9142122b1588939796e311c7ac501da822c5f`.
The intervening changes are an optional vignette guard, a source-only smoke-test
assertion, and excluded maintainer reporting. Covr's installed-package tests
passed 19,845 expectations, zero failures/warnings, 41 skips. Its opted-in Python
paths ran; excluded source-documentation/maintainer paths and the three legacy
mode guards explain the remaining skips. This does not count the source-only
9106 guide execution as part of instrumented coverage.

The collection saved a complete coverage object and CSVs. The command then exited
with `object 'ledger' not found`: while it was running, the maintainer script had
been lengthened by 81 bytes to change warning reporting, so Rscript read an extra
copy of the final print expression beyond its original end-of-file position.
No package source or instrumented test was changed during collection. A fresh
process re-read the saved object, recalculated line counts and percentages, and
verified exact equality with the original CSV before saving the durable ledger.
The [successful fresh-process review](issue-259-phase7-coverage-review.txt) records
this recovery. The complete instrumented suite was not needlessly repeated.
The saved coverage RDS SHA-256 is
`a28b1d7ea73107080bd67b731fe9d921868498757376d2b4a898b45ce13b8bb0`.

The [18 uncovered warm-start lines](issue-259-phase7-uncovered-warm-lines.csv)
remain explicit limits: optional-withr absence; defensive CV/engine/payload,
ensemble/PLS/SVR validation guards; the internal missing-fold fallback; registry
path failure and Windows case-folding. None is evidence of cross-platform test
execution. All corresponding files nevertheless exceed 95%. No guard was removed
or runtime behavior changed to increase coverage.

CRAN-style command:
`rcmdcheck::rcmdcheck(path=".", args="--as-cran", error_on="never")`.
Initial full check completed in 13m 51s: **0 errors, 0 warnings, 1 NOTE**.
Installed-package tests passed **19,672 expectations**, with no failures/warnings
and 49 reported skips. These skips cover excluded repository documentation,
maintainer workflows/smoke scripts, unselected opt-in Python integration, and
three already removed legacy linking/transform modes. The source suite exercises
the available source/Python paths separately. No live provider request was made.

The NOTE is environmental: HTML Tidy is absent, so R's HTML manual validation was
skipped; V8 is absent, so mathematical-render validation was also skipped.
These tools were not installed as a side effect. The local pkgdown link/anchor
and browser checks below supplement, but do not claim to replace, those checks.
The final package-content candidate was rebuilt and checked with
`args=c("--as-cran", "--no-tests")` while its corrected full source suite ran
separately. This refresh repeated package, examples and vignette checks in
3m 13s: **0 errors, 0 warnings, the same 1 environmental NOTE**. The explicit
`--no-tests` result is not presented as a second installed-package test run.
Both check logs are retained: [initial full check](issue-259-phase7-full-check.txt)
and [final archive check](issue-259-phase7-final-check.txt).

Other verification:

- `devtools::document()` regenerated the 11 Rd topics; all 121 Rd files parse.
  NAMESPACE has no diff, and documentation dependency auditing passes.
- `lintr::lint_package()` returned zero lints. New maintainer scripts were also
  linted explicitly. `git diff --check` passes.
- README, adaptive-warm-start and adaptive-pairing render successfully. README's
  source hash is synchronized. An initial guide chunk depended on a setup-only
  option variable; ordinary explicit namespace guards fixed source extraction.
  The small toy alpha=1 fit initially did not converge; only the labeled example
  grid changed to c(0, 0.5). Neither issue survives final validation.
- Full `pkgdown::build_site(..., install=TRUE, new_process=TRUE)` succeeds using
  a temporary installation of this package and existing dependencies. It builds
  all 13 articles and the reference/news pages. An initial install=FALSE attempt
  loaded an older installed package; rebuilding against this source resolved it.
- A local HTML parser checked links/anchors on the site home, reference index,
  news and both changed articles: zero broken local targets. Chrome desktop and
  mobile first-view screenshots were reviewed for readable layout and version
  1.5.2. A separate attempted scripted table screenshot did not complete and was
  terminated; no table-specific browser assertion is claimed.
- Fresh-process `check-issue-259-pls-deployment.R`,
  `check-issue-259-svr-deployment.R` and
  `check-issue-259-algorithm-deployment.R` pass full and reduced deployment for
  **both v1 and v2**, without optional fitting/Python packages loaded. These use
  synthetic inputs; they do not fit or replay a substantive study.
- Current package-version references in DESCRIPTION, README source/output,
  CITATION, codemeta, NEWS and documentation tests are 1.5.2. Historical release
  headings, 1.3.1 provider evidence/CRAN submission and 1.5.1 legacy fixtures retain
  their real provenance. Schema versions are independent of package versions.
- Frozen SHA-256 values remain exact: v1 schema
  `1414573759c302dc24e9041cfe2eb084fb4be1fac1fd26440b2011d4f9a736f7`;
  v2 schema
  `d9f271abae50eeac6d06a9ab7304309932174d54193ba8fb26348e9e886b2492`;
  legacy RDS `77fe4871283d7d938334d99438d5d54f0554c85f02d35ff1f6ba7c1a446433dc`.

Final source archive: `pairwiseLLM_1.5.2.tar.gz`, 3,367,874 bytes, 531 files;
SHA-256 `9628d67f13697e5698022742dc8304b7a59ee323c720bf1999f3ab0ac5c6a946`.
The [package-source manifest](issue-259-phase7-package-source-manifest.csv)
records SHA-256 values for all 490 tracked source files included in the archive.
All are byte-identical to the final package-content candidate, except DESCRIPTION
where R adds standard build metadata and wraps fields; all original source field
values agree after whitespace normalization. Generated vignette outputs are
additional build products. The archive excludes data-raw, tasklists and studies.
The first full-check archive, before the final example guard/test correction,
had SHA-256 `c381f697febde6b35367f1e3499c870af0ea50e368ee7379bc6b36276196644d`;
it is distinct from the final archive above.

## Files and handoff

Exact implementation/correction files relative to reviewed master:

- `DESCRIPTION`
- `NEWS.md`
- `R/adaptive_rank.R`
- `R/adaptive_run.R`
- `R/warm_start_coefficients.R`
- `R/warm_start_cv.R`
- `R/warm_start_ensemble.R`
- `R/warm_start_feature_schema.R`
- `R/warm_start_features.R`
- `R/warm_start_model_io.R`
- `R/warm_start_model_registry.R`
- `R/warm_start_predictions.R`
- `R/warm_start_prior.R`
- `README.Rmd`
- `README.md`
- `_pkgdown.yml`
- `codemeta.json`
- `cran-comments.md`
- `data-raw/warm-start/README.md`
- `data-raw/warm-start/check-issue-259-phase7.R`
- `data-raw/warm-start/create-vignette-example.R`
- `data-raw/warm-start/issue-259-handoff.md`
- `data-raw/warm-start/issue-259-phase7-ci-next-thread.md`
- `data-raw/warm-start/issue-259-phase7-focused-results.csv`
- `inst/CITATION`
- `inst/extdata/warm-start-example.md`
- `inst/extdata/warm-start-example.rds`
- `inst/models/README.md`
- `inst/python/README.md`
- `man/adaptive_rank.Rd`
- `man/adaptive_rank_start.Rd`
- `man/ensemble_warm_start_models.Rd`
- `man/extract_warm_start_features.Rd`
- `man/fit_warm_start_model.Rd`
- `man/make_warm_start_prior.Rd`
- `man/predict.pairwiseLLM_warm_ensemble.Rd`
- `man/register_warm_start_model.Rd`
- `man/save_warm_start_model.Rd`
- `man/warm_start_coefficients.Rd`
- `man/warm_start_feature_schema.Rd`
- `tests/testthat/README.md`
- `tests/testthat/test-0026-documentation-contracts.R`
- `tests/testthat/test-0027-smoke-runner.R`
- `tests/testthat/test-9106-warm-start-documentation-workflow.R`
- `vignettes/adaptive-pairing.Rmd`
- `vignettes/adaptive-warm-start.Rmd`

Reporting adds this report, current ledgers and the final handoff. The ignored
active task and both local indexes mirror the final status. User-owned untracked
`data-raw/studies/` remains uninspected and untouched.

Phase 7 local implementation and validation are complete with the limits above.
PR CI and user review remain pending. On opening the PR, stop and await the user's
results; do not poll, merge, tag, publish, or start downstream fitting/replay.
The follow-up prompt gives exact compatibility tests and SHA/tree checks for a
later D042 pin. Full construction evidence, task/ordered ID/outcome/schema/CV
identity, outer-context alignment, frozen grids and weighting/SE/tie rules,
learned OOF calibration, R4.7 RNG provenance, portable SVR dimensions/order,
legacy formats and prior SD authority remain required in later work.
