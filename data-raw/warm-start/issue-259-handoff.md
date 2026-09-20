# Issue 259 execution and thread handoff

Authority: [issue #259](https://github.com/shmercer/pairwiseLLM/issues/259) and
[the architecture contract](issue-259-design.md). The user approved seven stacked
PRs, one implementation phase per thread, and version 1.5.2 in Phase 7.
Do not use study outcomes to choose features, tuning, engines or exceptions.

## Phase and branch map

| Phase | Scope | Branch (prefix feat/259-) | PR base | Status |
|---|---|---|---|---|
| 1 | Architecture, frozen regression evidence, handoffs | 01-architecture | master | Complete; PR #260 merged to reviewed master |
| 2 | CV plans, format 3, compatible glmnet and single-model interoperability | 02-cv-engines | master (Phase 1 merged) | Complete; PR #261 merged, all seven checks passed |
| 3 | Audited v2 schema commit, extraction and audit | 03-writing-v2 | master (Phase 2 merged) | Complete; PR #262 merged, all seven checks passed |
| 4 | PLS backend and portable linear deployment | 04-pls | master (Phase 3 merged) | Complete; PR #263 merged, all seven reviewed-head checks passed |
| 5 | RBF-SVR backend and portable kernel deployment | 05-svr-rbf | master (Phase 4 merged) | Local implementation/verification complete; PR/CI handoff follows |
| 6 | Same-task algorithm ensemble and all consumers | 06-algorithm-ensemble | Phase 5 branch | Not started |
| 7 | End-to-end docs, full validation, 1.5.2 | 07-docs-quality | Phase 6 branch | Not started |

Fetch and verify actual upstream/PR state before each phase. Build a child branch
from the verified predecessor tip, or its reviewed merge result if already merged.
Open a draft phase PR after local checks and handoff. For Phase 5, the user
explicitly requires ending the turn once the PR is open and waiting for them to
return CI results; do not poll CI or wait for checks before that handoff. Use small commits
and Refs #259; only the final completed PR targets master with a closing reference.
Normal pending review does not block the next stacked phase. Scientific-contract
conflicts or unresolved foundational defects do.

When an ancestor merges, retarget/restack descendants. Verify parent and child
commit ranges before rebasing; use force-with-lease only for task-owned branches.
Review final phase diffs and rerun checks affected by conflict resolution. No
merge, tag, publication or branch deletion without additional user authorization.

## Starting and ending threads

Start a new thread after each phase's commits, PR and handoff. Also use a fresh
thread for substantial review remediation or a committed mid-phase checkpoint
when the remaining work would otherwise depend on a long debugging conversation.
Do not rely on a context-percentage threshold. Codex must checkpoint proactively.
Finish or deliberately stop active foreground commands before ending a thread.
Automatic compaction is not a handoff: reread this ledger and reverify Git state.

At every start:

1. Read AGENTS.md, the main tasklists/00-index.md (including handoffs/feedback),
   the active numbered tasklist, this ledger/design and the complete issue.
2. Reverify branch, HEAD, version, working tree, remote/PR base/head and predecessor.
3. Inspect the exact current source/tests for symbols to change. Current code
   controls implementation details; the issue/user decisions control methodology.
4. Reconcile new review feedback; preserve unrelated files, especially
   data-raw/studies/. Never use study results for implementation decisions.
5. Run the active phase only. Do not require earlier PR merges to continue the stack.

Every phase updates its own local completion section and the main local index,
plus this tracked ledger. Local tasklists remain intentionally git-ignored;
this tracked design/ledger is the portable recovery source. If local tasklists
are absent in a fresh checkout, reconstruct the active one from these documents.

Every checkpoint records: status/acceptance; branch/parent/base/implementation SHA;
package version and PR URL/base/head; exact files/behavior; locked/new decisions;
commands/results/dependency versions/coverage; documentation and generated diffs;
unresolved issues and exact next actions; uncommitted/untracked file ownership;
active-process state; and an exact next-thread prompt. Temporary logs supplement
these facts, never replace them. Do not store private inputs in tracked reports.

Record the implementation commit described by a report, not the report's own
self-referential commit hash. Final delivery heads belong in the PR/local handoff.

## Phase 1 completion report

Status: implementation and focused verification complete on
feat/259-01-architecture; [PR #260](https://github.com/shmercer/pairwiseLLM/pull/260)
targets master, user review/CI pending. Verified parent master and origin/master
at 013f869b1ba2272b88e85937023d57eb2c2c99fa; package 1.5.1, R >=4.4.
Architecture commit: 1a4be1e. Fixture/test implementation commit described by this
report: 7afa871c37d7157ca605630c94774b28ff336f3f. This final reporting commit also
adds the narrow v1 CSV LF checkout rule. Use the verified PR head for continuation,
including this report; final delivery head is recorded in the PR/local handoff.
Untracked data-raw/studies/ is user-owned and excluded from this work.

Files introduced/changed:

- issue-259-design.md: complete architecture, audit inventory, statistical and
  artifact boundaries, approved v2 membership and sequencing.
- issue-259-handoff.md: seven-phase Git/thread workflow and completion evidence.
- data-raw/warm-start/README.md: links to the development design/ledger.
- capture-issue-259-baseline.R: explicit guarded synthetic historical capture.
- check-issue-259-baseline.R: reproducible focused test/covr collector with saved
  evidence summarization, excluded from installed runtime with data-raw.
- issue-259-phase1-test-results.csv and issue-259-phase1-coverage-ledger.csv:
  durable per-test and per-file baseline evidence.
- tests/testthat/README.md: reserve 0113-0119, 3107-3109, 9104-9106.
- .gitattributes: preserve LF bytes of the audited v1 CSV on Windows checkouts.
- tests/testthat/test-0113-warm-start-legacy-regression.R: frozen legacy loading,
  deployment, cross-task arithmetic and numerical refit checks.
- tests/testthat/fixtures/warm-start-legacy/{README.md,baseline-1.5.1.rds}:
  provenance, immutable pre-refactor evidence and reproduction limitations.

Local ignored tasklists record each phase. No R runtime/Python/schema/Roxygen/Rd/
NAMESPACE/dependency/version change is made in Phase 1. No v2 fitting occurs.

Applied decisions: preserve legacy validators and cross-task duplicates; new
format3 and standalone same-task class; legacy-order reusable folds; fold-local
outcome scales; primitives-first feature policy; optional engines; 1.5.2 only in
Phase 7. New sequencing finding: single-model format3 prior/storage/registry/
bundle interoperability must land in Phase 2, not wait for the new ensemble.

Current dependency inventory: R4.6.1, glmnet5.0, pls2.9.0, e1071 1.7.17,
testthat3.3.2, covr3.6.5, pkgload1.5.3, lintr3.4.0, withr3.0.3.

### Commands and results

- `Rscript --vanilla data-raw/warm-start/capture-issue-259-baseline.R`:
  captured three synthetic cases using unchanged baseline runtime. The later
  guarded recapture command with `/tmp/issue259-phase1-recapture` produced
  byte-identical RDS evidence (`cmp` passed), SHA-256
  `77fe4871283d7d938334d99438d5d54f0554c85f02d35ff1f6ba7c1a446433dc`.
  A subprocess check verified existing-fixture capture refuses overwrite.
- `testthat::test_local(filter = "^0113-", reporter = "summary",
  stop_on_failure = TRUE, stop_on_warning = TRUE)` passed initially (147
  expectations); the subsequent byte-hash assertion is included in the final
  focused run: new test file totals **148 expectations in 3 blocks**, no skips.
- `Rscript --vanilla /tmp/issue259-phase1-coverage.R` executed the explicit
  filter `^(0031|010[0-9]|011[0-3]|310[0-2]|510[015]|910[0-2])-` under
  `covr::environment_coverage(asNamespace("pairwiseLLM"), ...)`:
  **24 files, 120 test blocks, 4,901 passed expectations, 0 failures/errors/
  warnings, 5 existing opt-in Python skips**. No provider calls or sampling.
- The original collector saved coverage successfully, then its summary code
  used an incorrect column name. Recovered the already saved evidence with
  `Rscript --vanilla data-raw/warm-start/check-issue-259-baseline.R
  /tmp/issue259-phase1 --summarize-only`, using `covr::tally_coverage(by="line")`.
  No tests were rerun to repair reporting. The tracked script's ordinary mode
  reproduces the same explicit filtered collection; it is not a full-suite run.
- Changed R scripts/test file: `lintr::lint()` reports zero lints after fixing
  quote style in the new collector. `git diff --check`, test-prefix uniqueness,
  and source/resource byte comparisons pass. No Roxygen/generated docs changed.
- `git diff --quiet 013f869b1ba2272b88e85937023d57eb2c2c99fa -- R inst
  DESCRIPTION NAMESPACE` passes. V1 SHA remains the value in the design note.
  `git check-attr text eol -- inst/warm-start/feature-schema-writing-v1.csv`
  verifies LF checkout on all platforms; the CSV itself is unchanged.

Logs/objects: `/tmp/issue259-phase1-new-tests.log`,
`/tmp/issue259-phase1-coverage.log`, `/tmp/issue259-phase1-coverage.rds` and
`/tmp/issue259-phase1-coverage-lines.csv`. Durable CSVs beside this report retain
the exact per-test results and coverage totals if temporary logs disappear.

The five skips are the pre-existing real-Python paths in 0102, 0108, 0109, 5100
and 9100; PAIRWISELLM_TEST_PYTHON was intentionally not selected. Python extraction
is unchanged in Phase 1; actual pinned-environment extraction is Phase 3 work.

### Coverage and review limits

Measured scoped baseline line coverage across all 19 existing warm-start R files
is **95.38462-100%**; the exact numerators/denominators are in
[the coverage ledger](issue-259-phase1-coverage-ledger.csv). No runtime R file is
new or materially changed in Phase 1. This is a baseline for later comparison,
not a claim of full-package coverage or coverage of future engines. Historical
numerical refits are gated to glmnet5.0; legacy deployment checks still run
without that version. No full-suite test, CRAN check or final release validation
was run in this phase.

Independent read-only inventory/design/test review found no scientific or
interface blocker. Review prompted capture safeguards: reject untracked runtime
source/data (including ignored source), disable test-helper loading, refuse
overwrite and permit separate-output reproduction. Pre-existing shell histories
and compiled caches remain untouched. Byte-identical recapture validates these
changes. A narrow LF checkout rule prevents Windows newline conversion from
invalidating the already-frozen v1 CSV hash.

No unresolved foundational conflict. All foreground commands finished; no phase
process is left running. Tracked changes are committed; user-owned untracked
data-raw/studies/ remains untouched. Local ignored tasklists/index contain the
matching completion record. Phase 2 has not started.

User review and final full/package/platform validation remain pending. Full tests,
CRAN-style check, docs/rendering and current new-path coverage are Phase 7 work,
not claims made by a successful Phase 1 fixture fit.

## Phase 2 completion report — 2026-09-19

Implementation and focused verification complete. Branch `feat/259-02-cv-engines`,
[PR #261](https://github.com/shmercer/pairwiseLLM/pull/261), base `master` at
`33e5737c66901be2cb0191692e5066434cd4762e`. Implementation commit: `d44ea758cd07cb7f4bfdfa4b474e80b0498e862e`.
This report is in a later reporting commit; use the verified final PR head for
continuation. Package remains **1.5.1**, R >=4.4. No dependency change.
PR #260 was verified MERGED at the reviewed master SHA, and master/origin/master
matched before branching and again before delivery. No other open PR existed at
start. Local prior branch retained. Final PR ready/head/CI state is recorded in
GitHub and the local handoff; user review and full validation remain pending.

### Delivered behavior and decisions

- Exported `make_warm_start_cv_plan()` and appended engine/cv_plan/engine_control
  arguments. Plans bind task, exact numeric theta and ordered normalized IDs;
  store standardization and RNG provenance; use canonical version-2 XDR hashing;
  preserve caller RNG kind and present/absent seed state. Draw order is outer,
  ordered outer-training inner, full-data inner. Validation precedes extraction
  and fitting; explicit seed/count conflicts fail, omitted values use the plan.
- New public glmnet fits use format 3, full audit, shared-plan evidence, selected
  hyperparameters and typed numeric linear payloads. Top-level compatibility
  views are checked. Engine dispatch preserves current reference paths, losses,
  SE/ties, solver settings, split-local scaling, OOF calibration and refit order.
  Reserved PLS/SVR names fail explicitly; those backends remain later phases.
- Format-3 validation retains the legacy glmnet audit depth through a checked
  legacy projection and adds exact plan/payload consistency. Legacy constructors
  and formats 1/2 retain their validators. Full and reduced prediction, coefficients,
  print/summary, save/load, registry, bundle and warm-prior paths work now.
- Reduction retains format 3, explicit summary-only status, deployment payload,
  compact identity and labeled summaries; the full plan/row evidence is omitted.
  Typed payload copying avoids the dimension-dropping general list copier.
  Legacy bundle component JSON records are unchanged; new records carry CV identity.
  Registry engine/version/component-engine columns preserve cross-task n=NA and
  component-only metrics. Repeated cross-task components remain accepted.
- No frozen fixture/schema bytes, v1 extraction, builder workflow, production
  model, study outcome, provider, adaptive pairing, Phase B estimator or dependency
  was changed. Prior centering/SD and persistence/resume authority remain intact.
  Same-task algorithm ensembles and version1.5.2 remain assigned to later phases.

### Exact changed files

Implementation commit contains these 45 files:

```text
DESCRIPTION
NAMESPACE
NEWS.md
R/warm_start_bundle.R
R/warm_start_cv.R
R/warm_start_cv_plan.R
R/warm_start_engine.R
R/warm_start_ensemble.R
R/warm_start_format3.R
R/warm_start_model.R
R/warm_start_model_artifact.R
R/warm_start_model_io.R
R/warm_start_model_registry.R
R/warm_start_predict.R
R/warm_start_prior.R
data-raw/warm-start/check-issue-259-phase2.R
data-raw/warm-start/issue-259-design.md
data-raw/warm-start/issue-259-phase2-coverage-ledger.csv
data-raw/warm-start/issue-259-phase2-test-results.csv
man/ensemble_warm_start_models.Rd
man/extract_warm_start_features.Rd
man/fit_warm_start_model.Rd
man/make_warm_start_cv_plan.Rd
man/make_warm_start_prior.Rd
man/pairwiseLLM_warm_model.Rd
man/predict.pairwiseLLM_warm_ensemble.Rd
man/predict.pairwiseLLM_warm_model.Rd
man/prepare_warm_start_model.Rd
man/register_warm_start_model.Rd
man/save_warm_start_model.Rd
man/summary.pairwiseLLM_warm_ensemble.Rd
man/summary.pairwiseLLM_warm_predictions.Rd
man/warm_start_coefficients.Rd
man/warm_start_feature_schema.Rd
man/warm_start_python_status.Rd
tests/testthat/README.md
tests/testthat/helper-warm-start-phase2.R
tests/testthat/test-0104-warm-start-model-core.R
tests/testthat/test-0109-warm-start-ensemble.R
tests/testthat/test-0114-warm-start-cv-plan.R
tests/testthat/test-3100-warm-start-model-io.R
tests/testthat/test-3107-warm-start-format3-artifacts.R
tests/testthat/test-9100-warm-start-bundle-workflow.R
tests/testthat/test-9104-warm-start-shared-plan.R
vignettes/adaptive-warm-start.Rmd
```

This reporting commit also updates `data-raw/warm-start/issue-259-handoff.md`.
Ignored local reports updated: `tasklists/issue-259/02-cv-engines.md`,
`tasklists/issue-259/00-index.md`, and `tasklists/00-index.md`.
Local tasklists remain ignored; user-owned untracked `data-raw/studies/` is
preserved and excluded. All task-owned tracked changes are committed at delivery.

### Validation commands and results

- `testthat::test_local(filter = "^0113-", reporter = "summary",
  stop_on_failure = TRUE, stop_on_warning = TRUE)`: frozen compatibility passed
  **148 expectations**. Frozen tests/expected data were not altered.
- Initial new-file filter `^(0114|3107|9104)-` passed CV/artifact checks but the
  workflow test incorrectly reversed requested IDs along with feature rows.
  The intended identity guard rejected it. Corrected only that test to retain
  requested order while shuffling feature rows; subsequent run below passed.
- `Rscript --vanilla data-raw/warm-start/check-issue-259-phase2.R
  /tmp/issue259-phase2`: exact filter
  `^(0031|010[0-9]|011[0-4]|310[0-27]|510[015]|910[0-24])-`, under
  `covr::environment_coverage(asNamespace("pairwiseLLM"), ...)`.
  **27 files, 130 blocks, 5,111 passed expectations, zero failures/errors/warnings,
  five existing opt-in Python skips** (0102/0108/0109/5100/9100).
- A later brace-only lint correction in prediction changed source line locations.
  `Rscript --vanilla /tmp/issue259-phase2-postlint.R` refreshed scoped coverage
  with exact filter `^(0104|3107)-`: **14 blocks, 226 passed**, no failures/errors/
  warnings/skips. Final saved coverage replaces only `warm_start_predict.R:`
  entries with this fresh run; all other entries retain the broad focused run.
  This was an additional validation run, not 226 distinct additional tests.
  The tracked collector reproduces coverage on the final source in one run.
- `Rscript --vanilla data-raw/warm-start/check-issue-259-phase2.R
  /tmp/issue259-phase2-final --summarize-only`: reports saved final evidence.
  Durable per-run results and current per-file ledger are
  `issue-259-phase2-test-results.csv` and `issue-259-phase2-coverage-ledger.csv`.
- Fresh R processes (`/tmp/issue259-phase2-fresh-{build,predict}.R`) fitted one
  synthetic model, serialized full/reduced objects, then verified identical
  predictions and priors with neither glmnet nor reticulate loaded in deployment.
- `devtools::document(quiet = TRUE)` regenerated Collate, one export/new topic,
  affected API topics and warm-start family links. Initial new-topic link notice
  disappeared on regeneration; source/generated diffs inspected. Six directly
  changed/new API Rd topics parsed successfully with `tools::parse_Rd()`.
- `pkgload::load_all(quiet = TRUE); rmarkdown::render(
  "vignettes/adaptive-warm-start.Rmd", output_dir = "/tmp/issue259-phase2-vignette",
  intermediates_dir = "/tmp/issue259-phase2-vignette", quiet = TRUE)` passed.
  Vignette now executes a shared-plan example and explains format generations.
- `lintr::lint_package()` identified only added brace/semicolon/trailing-space/
  sequence style findings. Corrected them; targeted final `lintr::lint()` calls
  for every affected source/test with findings and new files/scripts returned zero.
  No unrelated source changes were made to satisfy lint.
- 178 unique numeric test prefixes; `git diff --check` and staged diff check pass.
  The staged CSV initially used CRLF; normalized that new report to LF before
  commit. No test rerun was needed for this reporting-only correction.
  Byte/source comparison against reviewed master confirms unchanged v1 extraction,
  schema resources, frozen regression data and maintainer builder. V1 SHA-256:
  `1414573759c302dc24e9041cfe2eb084fb4be1fac1fd26440b2011d4f9a736f7`.
  Frozen fixture SHA-256:
  `77fe4871283d7d938334d99438d5d54f0554c85f02d35ff1f6ba7c1a446433dc`.

### Coverage, review and limits

All **22 warm-start R files** measured at least 95% scoped line coverage.
New files: CV plan **75/78 (96.15385%)**, engine boundaries **21/22 (95.45455%)**,
format-3 contract **70/70 (100%)**. Materially affected existing files: CV
103/105, bundle69/69, ensemble81/84, model133/133, artifact107/107,
registry89/92, prediction29/29, prior124/130. Model I/O changes are docs-only;
its existing runtime measured37/37. Exact percentages for all files are in the
ledger. This is current focused coverage, not package-wide coverage.

Environment: R4.6.1, glmnet5.0, withr3.0.3, testthat3.3.2, covr3.6.5.
Historical numerical regression remains gated to glmnet5.0 by the frozen tests;
legacy deployment checks remain independent of that backend version.
Independent read-only implementation review found no blocking numerical,
statistical, validation or interoperability issue. Its recommended direct
single-model prior checks were added and passed for full/reduced objects.

No full suite, CRAN check, full-package coverage, pinned real-Python extraction,
provider calls, MCMC, study fitting, merge/tag/publication or Phase 3 work.
CI and maintainer review remain pending at handoff; Phase 7 owns full validation.
No phase command is left running. Temporary logs under `/tmp/issue259-phase2-*`
supplement the durable results; they are not needed for continuing the series.

### Requirements for Phase 3

Preserve legacy RNG order, ordered ID/exact outcome plan binding, context-local
outcome scales, strict legacy validators, complete format-3 audit depth,
full/reduced storage/prior interoperability, old cross-task duplicate acceptance,
unchanged v1 bytes/extraction and frozen fixtures. Do not change engine grids or
use study outcomes. Materialize and separately commit the approved source-audited
46-feature schema/inventory before consumers rely on it; record the actual hash.
Keep the 1.5.1 version until Phase 7. Do not implement PLS/SVR or same-task ensembles
in Phase 3. No unresolved foundational defect blocks the next phase.

## Phase 2 CI remediation — R-devel RNG provenance (2026-09-19)

The user reported CI failures before final delivery. At PR head
`735d71e3dcfff362094a5280ccaae51e0ea9d389`, R-devel failed during the warm-start
vignette build with `Invalid CV plan RNG provenance`; the earlier implementation
head had the same failure. Evidence:
[failed R-devel job](https://github.com/shmercer/pairwiseLLM/actions/runs/35474964244/job/105982525762).
R-devel is R4.7.0 and now reports a fourth RNG kind for binomial generation.
The original new validator incorrectly required exactly three entries.
The [R-devel base manual](https://stat.ethz.ch/CRAN/doc/manuals/r-devel/packages/base/refman/base.html)
and R source confirm `binom.kind` and the `Buggy BTPE`/`BTPE` labels.

Correction: accept validated three- or four-entry provenance, preserve actual
metadata without padding/rewriting old plans, and validate the optional binomial
entry. CV still draws only uniform/sample values, so fold construction is
unchanged. Legacy plan digests remain unchanged. A narrow internal RNG-query
wrapper allows package-scoped regression mocks without modifying base bindings.

Exact follow-up files: `R/warm_start_cv_plan.R`,
`tests/testthat/test-0114-warm-start-cv-plan.R`, this handoff,
`issue-259-phase2-test-results.csv`, `issue-259-phase2-coverage-ledger.csv`.
All three ignored local handoffs are updated too. No new API, dependency, version,
R floor, extraction or statistical change. The follow-up commit is identified by
the final PR/local delivery head, with parent735d71e above.

Validation:

- Initial focused0113/0114/3107 rerun confirmed148 frozen and67 artifact
  expectations but the new regression attempted to mock an absent package
  binding for base RNGkind. Added the narrow query wrapper; no base namespace
  override was used. The first coverage attempt was explicitly stopped before
  results because it had loaded the pre-wrapper source.
- Corrected `testthat::test_local(filter = "^0114-", reporter = "summary",
  stop_on_failure = TRUE, stop_on_warning = TRUE)` passed151 expectations.
  Tests cover both fourth-entry labels, identical folds, caller RNG preservation,
  malformed lengths/labels, and full/reduced model/prediction interoperability.
- `Rscript --vanilla /tmp/issue259-phase2-r47-coverage.R` runs that same0114
  filter under scoped environment coverage: **7 blocks/151 passed, zero failures,
  errors, warnings or skips**. Final evidence replaces only CV-plan file entries;
  all unaffected files retain their already passing coverage. Current CV-plan
  coverage is **76/79 =96.20253%**; all22 warm R files remain >=95%.
  Durable results have a separate `r47-remediation` run label. The tracked phase
  collector can reproduce all focused tests/coverage on the final source.
- Affected source/test lint and `git diff --check` pass. The changed warm-start
  vignette rendered again to `/tmp/issue259-phase2-r47-vignette/` with the corrected
  validator. No Roxygen/generated-document changes were necessary.
- Independent read-only review found no blocker. Actual R4.7 execution awaits
  the corrective commit's CI; local R4.6.1 exercises its four-entry metadata via
  scoped mocks. At the last pre-fix check, pkgdown passed; only R-devel failed,
  and the other checks were still running.

PR261 is OPEN/ready, targeting master; no merge/publication or Phase3 work.
No study files were inspected or changed. Corrective delivery and final CI status
are recorded in the PR and the local handoffs; the Phase3 prompt below requires
using the verified final head, including this remediation.

## Historical Phase 3 entry prompt

Start Phase 3 in a new thread using this exact prompt:

```text
Implement Phase 3 of issue #259 in shmercer/pairwiseLLM.

Phase 2 implementation: d44ea758cd07cb7f4bfdfa4b474e80b0498e862e.
Phase 2 PR: https://github.com/shmercer/pairwiseLLM/pull/261
Branch: feat/259-02-cv-engines; base master at 33e5737c66901be2cb0191692e5066434cd4762e.
Use the verified final PR head, including the later completion-report commit,
or its reviewed merge. Reverify actual Git/PR state and reconcile review fixes.

Read AGENTS.md, tasklists/00-index.md, tasklists/issue-259/03-writing-v2.md,
data-raw/warm-start/issue-259-design.md, data-raw/warm-start/issue-259-handoff.md,
and the complete issue #259. Reconstruct absent ignored tasklists from the
tracked design/ledger. Create feat/259-03-writing-v2 from the verified Phase 2
head, targeting feat/259-02-cv-engines; if Phase 2 has merged, branch from its
reviewed merge and target master instead.

Materialize and separately commit the approved audited 46-feature v2 schema
and full inventory before implementing extraction consumers. Record the actual
SHA-256. Implement schema-aware R/Python dispatch/decoding, readability features
and missing-value handling from pinned sources, without new Python dependencies.
Preserve v1 extraction, schema/hash/order, frozen regression fixtures, exact
legacy-order CV plans, format-3 full/reduced interoperability and statistical
contracts. Do not inspect study outcomes or modify unrelated data-raw/studies/.

You are authorized to implement, run focused checks, commit, push, and create/
update the Phase 3 PR. Do not merge, tag, publish, or start Phase 4. Update durable
and local handoffs with exact Git/PR state, validation and coverage evidence,
and provide the exact Phase 4 new-thread prompt before ending.
```


## Phase 3 completion report — 2026-09-19 (Pacific)

Implementation and focused validation complete on `feat/259-03-writing-v2`;
[PR #262](https://github.com/shmercer/pairwiseLLM/pull/262) targets `master`.
Verified parent/base is Phase 2's reviewed squash merge
`a36f518b593120d6a869b3803a6fa3193e84d830`. PR #261 was MERGED at
2026-09-19T23:41:51Z from `fa141068b9a63a537405ec61df756c2ae33be8e0`, with all
seven checks SUCCESS and no subsequent review comments/reviews in its PR record.
Its merge tree equals the verified head exactly. Fetched master remains at that
merge; Phase 2's final R-devel four-entry RNG-provenance correction is preserved.
Package remains **1.5.1**, R >=4.4; no dependencies changed.

Schema/inventory-only commit: `8e619881f3d9b2f9eda8c59c082a3e8b78107fac`.
Metadata wording follow-up: `3509f26623e6d3eaf7f0303da8bebd11973907d4`.
Implementation commit described by this report:
`c09d0eae1dd4bef085660aeb643730505dbc1cdf`.
CI test-only correction: `94202aa897fef7210ea13bd14929592d7ed0b073`.
This report is a later commit; the final delivery head is recorded in the PR and
local handoffs. No merge, tag, release, branch deletion or Phase 4 work occurred.

### Behavior, schema and preserved contracts

- Materialized the approved 46-row schema and full 116-candidate inventory before
  extraction consumers. All v1 rows/definitions appear first with only schema ID
  changed, then the 26 approved additions in inventory order. CSV LF attributes
  preserve byte hashes across checkout platforms. No outcome-driven selection.
- Final schema SHA-256:
  `d9f271abae50eeac6d06a9ab7304309932174d54193ba8fb26348e9e886b2492`.
  Inventory SHA-256:
  `845466af5118606c433595abac277dffc9c390d9d6fdf004cb57830351f522b4`.
  Both are recorded in installed `inst/python/schema-writing-v2.json`, which
  references the immutable environment manifest and additionally hashes the
  textstat public wrapper. All 73 original upstream source hashes match locally.
- R dispatch selects the explicit v1/v2 CSV; decoding retains requested fields
  instead of filtering against v1. V2 validates added count types and upstream
  permitted missingness without imputing. V1 remains the default, with unchanged
  extraction/validation semantics. Response schema mismatches still fail.
- Python validates the selected CSV hash, enables TD readability only for v2,
  and calls each whitelisted textstat method with its pinned defaults. Missing
  token/syllable/readability summaries and short-document coherence stay missing.
  Valid zeros and negative Linsear–Write values remain unchanged. No new Python
  package, resource, network call or installation is introduced.
- Audited nuances: TD syllables count hyphens in lowercased Pyphen inserted forms;
  textstat removes hyphens without splitting, and unique difficult-word forms
  preserve case although classification lowercases. Counts differ from readability
  occurrences. Character-denominator algebraic exclusions explicitly retain their
  punctuation-dependent domain; inherited entropy transform exclusions do not
  claim unconditional reconstruction from normalized entropy. Membership unchanged.
- Existing CV/statistical code, shared-plan identity, format-3 full/reduced
  contracts, old formats 1/2 and cross-task ensemble semantics are preserved.
  Synthetic v2 models reuse exactly the v1 plan and round-trip through prediction,
  reduction, storage, registry, coefficients and warm-prior construction.

### Exact changed files

The two schema commits and implementation commit contain 25 files:

```text
.gitattributes
NEWS.md
R/warm_start_feature_schema.R
R/warm_start_features.R
data-raw/warm-start/README.md
data-raw/warm-start/audit_feature_schema_v2.py
data-raw/warm-start/check-issue-259-phase3.R
data-raw/warm-start/feature-inventory-v2.csv
data-raw/warm-start/feature-schema-v2-audit.md
data-raw/warm-start/issue-259-design.md
data-raw/warm-start/issue-259-phase3-coverage-ledger.csv
data-raw/warm-start/issue-259-phase3-test-results.csv
inst/python/README.md
inst/python/pairwisellm_warm_start.py
inst/python/schema-writing-v2.json
inst/warm-start/feature-schema-writing-v2.csv
man/extract_warm_start_features.Rd
man/warm_start_feature_schema.Rd
man/warm_start_python_status.Rd
tests/testthat/README.md
tests/testthat/fixtures/warm-start-features-v2/README.md
tests/testthat/fixtures/warm-start-features-v2/golden.json
tests/testthat/test-0100-warm-start-feature-schema.R
tests/testthat/test-0115-warm-start-writing-v2.R
vignettes/adaptive-warm-start.Rmd
```

This reporting commit adds this tracked handoff (26 total phase files). Local
ignored records: `tasklists/issue-259/03-writing-v2.md`,
`tasklists/issue-259/00-index.md`, `tasklists/00-index.md`. Ignore policy unchanged.
User-owned untracked `data-raw/studies/` was not inspected or modified. No other
uncommitted work belongs to this task at delivery; no active foreground process.

### Exact checks and current evidence

- `/home/sterett/.virtualenvs/pairwisellm-writing-v1/bin/python
  data-raw/warm-start/audit_feature_schema_v2.py --record`: created independent
  upstream-only evidence for 16 synthetic texts; refuses existing destinations.
  New fixture SHA-256:
  `3864d653e0e85c0768c709a2c1d515e9d4fd125949b02d481fefc3bc8fb784d7`.
  No original golden values were updated.
- Same interpreter, `data-raw/warm-start/audit_feature_schema_v2.py`:
  **5 test blocks passed**. Source/schema/inventory hashes, direct upstream
  agreement, explicit formula/default oracles, missingness, deterministic repeated
  protocol, exact extracted v1-prefix equality, status and corruption guards.
- Same interpreter, `tests/testthat/fixtures/warm-start-features/test_backend.py
  inst/python`: **9 original test blocks passed**, including immutable v1 golden
  values, resource/download guards and malformed response checks. Both Python
  suites display the existing spaCy/Click deprecation notice during stack import;
  no new runtime warning was introduced.
- Initial `PAIRWISELLM_TEST_PYTHON=/home/sterett/.virtualenvs/pairwisellm-writing-v1/bin/python
  Rscript --vanilla -e 'testthat::test_local(filter="^0115-", reporter="summary",
  stop_on_failure=TRUE, stop_on_warning=TRUE)'` passed. The later coverage run
  includes a strengthened assertion of the stored all-missing removal reason.
- Final `PAIRWISELLM_TEST_PYTHON=/home/sterett/.virtualenvs/pairwisellm-writing-v1/bin/python
  Rscript --vanilla data-raw/warm-start/check-issue-259-phase3.R /tmp/issue259-phase3`:
  explicit filter `^(010[0-3]|011[3-5]|3107|9104)-`, instrumented via
  `covr::environment_coverage(asNamespace("pairwiseLLM"), ...)`:
  **9 files, 42 blocks, 992 passed expectations, 0 failures/errors/warnings/skips**.
  Real pinned extraction was enabled for both schema paths. Per-file totals:
  0100 72; 0101 47; 0102 122; 0103 86; frozen 0113 148; 0114 151;
  new 0115 290; 3107 67; 9104 9. Durable CSV ledgers retain exact results.
- Current scoped whole-file R coverage: `warm_start_feature_schema.R` **36/36
  (100%)**, `warm_start_features.R` **83/83 (100%)**; additionally exercised
  unchanged bridge `warm_start_python.R` **77/77 (100%)**. These are current
  measurements, not inherited percentages or a package-wide coverage claim.
- `devtools::document(quiet=TRUE)` regenerated exactly the three Rd topics above.
  NAMESPACE, DESCRIPTION/Collate, dependency/version metadata unchanged. All three
  topics parsed with `tools::parse_Rd()`; source/generated diffs inspected.
- `pkgload::load_all(quiet=TRUE); rmarkdown::render(
  "vignettes/adaptive-warm-start.Rmd", output_dir="/tmp/issue259-phase3-vignette",
  intermediates_dir="/tmp/issue259-phase3-vignette", quiet=TRUE)` passed.
  The new schema-inspection example executes offline; existing model examples run.
- Targeted `lintr::lint()` on both changed R files, both changed/new tests and the
  new R collector: zero lints. Final `lintr::lint_package()`: zero lints.
  `git diff --check`, staged diff check, 179 unique numeric test prefixes, and
  `git check-attr text eol` for all three hashed CSV resources passed.
- Exact Git comparison with reviewed base confirms unchanged original v1 schema,
  inventory, environment/value manifests, dependency locks, golden/legacy fixtures
  and CV/format-3/preprocessing source. V1 CSV SHA remains
  `1414573759c302dc24e9041cfe2eb084fb4be1fac1fd26440b2011d4f9a736f7`;
  frozen legacy RDS SHA remains
  `77fe4871283d7d938334d99438d5d54f0554c85f02d35ff1f6ba7c1a446433dc`.

Temporary supplemental evidence is `/tmp/issue259-v2-{r-tests,python-final,doc-lint}.log`,
`/tmp/issue259-v1-python.log`, `/tmp/issue259-phase3-{coverage,vignette-lint}.log`,
plus scoped coverage RDS/line CSV. Durable results above are sufficient if these
logs disappear. All commands completed before handoff.

Initial findings resolved: source-wording corrections were committed separately
before consumer validation, without membership/default changes. The first Python
formula oracle used exact equality for a differently ordered LIX arithmetic
expression (difference 1.4e-14); replaced formula assertions with 12-decimal
agreement. Direct backend/upstream equality and repeat identity remain exact.
No implementation regression or scientific contract redesign was required.
Independent read-only source audit and implementation/test review found no
remaining blocker. No full-suite/check/package-wide covr, production fitting,
provider calls, MCMC, dependency installation or release action was run.

Maintainer review, PR CI completion and eventual Phase 7 full package/platform
validation remain pending. Real Python validation is Linux x86_64 only; successful
R CI on other platforms does not attest to those Python stacks. Phase 4 must
preserve both schema hashes/order, all frozen evidence, explicit CV partitions,
fold-local preprocessing/outcome scales, weighted tuning/SE/tie rules, OOF
calibration and format-3 audits. Do not inspect study outcomes or edit study files.

### CI test-dependency correction

Initial PR checks at implementation head `c09d0ea` passed pkgdown and coverage,
but all five R-CMD-check jobs failed. The inspected
[Ubuntu release log](https://github.com/shmercer/pairwiseLLM/actions/runs/35477626099/job/105989493863)
reported exactly one check warning: the new test used undeclared `digest`.
Its complete package tests passed (18,503 expectations, 47 skips, no test
failures/warnings); this is CI evidence, not a local full-suite run.

Corrected only test0115 and the durable test ledger in commit
`94202aa897fef7210ea13bd14929592d7ed0b073`. Use the existing base-R MD5 byte
check on every supported R, additionally computing SHA-256 through tools when
available. R added tools::sha256sum in 4.5.0; discover it dynamically to retain
R4.4 compatibility. The Python runtime/audit always checks actual SHA-256, and
R independently checks frozen manifest/fixture SHA identities. No dependency
was added and no hash, fixture, runtime or statistical contract changed.

The focused command with PAIRWISELLM_TEST_PYTHON enabled and
`testthat::test_local(filter="^0115-", reporter="summary", stop_on_failure=TRUE,
stop_on_warning=TRUE)` passed **5 blocks/291 expectations**, no failures/errors/
warnings/skips, followed by zero test-file lints. Test results are appended to
the CSV under `ci-test-dependency-fix`; original rows carry `phase3-coverage`.
Coverage remains current because no runtime source changed. Do not add duplicate
follow-up expectations to the original 992 as though they were distinct tests.

The combined test/reporting command subsequently used the wrong test-directory
argument for a tools check. Corrected standalone command:
`x <- tools:::.check_packages_used_in_tests(".", "tests/testthat");
stopifnot(!any(lengths(x[c("others", "imports", "data", "parse_errors")])),
!nzchar(x$methods_message))` passed with **zero findings**. An intermediate
summary assertion had also counted the empty methods-message string as a
finding; corrected after inspecting the returned structure. These were reporting
harness errors, not test failures. Logs:
`/tmp/issue259-phase3-ci-fix.log` and
`/tmp/issue259-phase3-test-dependencies.log`. Final diff checks passed.
Replacement CI was triggered by the corrected push; final status is recorded
in the local delivery handoff and PR. No known unresolved local defect remains.

## Phase 3 next-thread prompt (historical)

Start Phase 4 in a new thread using this exact prompt:

```text
Implement Phase 4 (PLS) of issue #259 in shmercer/pairwiseLLM.

Phase 3 PR: https://github.com/shmercer/pairwiseLLM/pull/262
Branch: feat/259-03-writing-v2; base master at a36f518b593120d6a869b3803a6fa3193e84d830.
Implementation: c09d0eae1dd4bef085660aeb643730505dbc1cdf.
CI test-dependency correction: 94202aa897fef7210ea13bd14929592d7ed0b073.
Use the verified final PR head, including its later handoff commit and any review fixes.

Read AGENTS.md, tasklists/00-index.md, tasklists/issue-259/04-pls.md,
data-raw/warm-start/issue-259-design.md, data-raw/warm-start/issue-259-handoff.md,
and the complete issue #259. Reconstruct missing ignored tasklists from the
tracked design/handoff. Verify current Git/PR/CI state and reconcile review fixes.
Create feat/259-04-pls from the verified Phase 3 head, targeting its branch.
If Phase 3 has merged, use its reviewed merge and target master instead.

Implement optional pls in Suggests, explicit kernelpls without backend scaling/CV,
a legal common component grid across every inner fit and context refit bounded
by rank/p/n-1 and capped at 10 (rank tolerance 1e-7), weighted MSE/SE and 1-SE
selection favoring fewer components. Preserve complete tuning/OOF/outer audits,
contextual degeneracy errors and portable linear coefficients/intercept.
Verify deterministic partitions, leakage boundaries, legal grids, backend
prediction equivalence, full/reduced artifacts and deployment without pls.

Preserve v1/v2 extraction, hashes/order and frozen fixtures, reusable CV plans,
format-3 interoperability and every statistical contract. V2 schema SHA-256:
d9f271abae50eeac6d06a9ab7304309932174d54193ba8fb26348e9e886b2492.
Do not inspect study outcomes or modify unrelated data-raw/studies/.

You are authorized to implement, test, commit, push, and create/update the Phase 4
PR. Do not merge, tag, publish, or start Phase 5. Update durable/local handoffs and
provide the exact Phase 5 new-thread prompt. Package remains 1.5.1 until Phase 7.
```


## Phase 4 completion report — 2026-09-19 Pacific

Implementation and focused local verification complete on `feat/259-04-pls`;
[PR #263](https://github.com/shmercer/pairwiseLLM/pull/263) targets `master`.
Implementation commit: `d810f2430eedcefb3c90f59b57bfcf6cd03873a4`.
Base: reviewed Phase 3 squash merge `9867bf18a3e017623235c6040ebb61d960c46573`.
Package remains **1.5.1**, R **>=4.4**. This report describes the implementation
commit; final reporting head/readiness and CI state are recorded in the PR/local
handoff after delivery, avoiding a self-referential commit identifier.

Phase 3 reconciliation: PR #262 merged at 2026-09-20T01:24:09Z, from reviewed
head `16f9d4181e0ba0291faad2f06b84c30999c9a6ce`, with all seven checks SUCCESS.
Its reviewed and merged trees match. Local/remote master match the merge;
Phase 3 branches were already deleted. No inline review comments were present.
Preserved its test-dependency correction and Phase 2 R4.7 RNG-kind correction.
Issue #259 was already CLOSED when read; issue state was not changed and this PR
uses `Refs #259`. The user explicitly authorized Phase 4 commits/push/PR, not merge.

### Exact files changed

- `DESCRIPTION`
- `NEWS.md`
- `R/warm_start_coefficients.R`
- `R/warm_start_cv.R`
- `R/warm_start_engine.R`
- `R/warm_start_format3.R`
- `R/warm_start_model.R`
- `R/warm_start_model_artifact.R`
- `R/warm_start_pls.R`
- `R/warm_start_pls_validation.R`
- `R/warm_start_predict.R`
- `R/warm_start_validation.R`
- `codemeta.json`
- `data-raw/warm-start/check-issue-259-phase4.R`
- `data-raw/warm-start/check-issue-259-pls-deployment.R`
- `data-raw/warm-start/issue-259-design.md`
- `data-raw/warm-start/issue-259-handoff.md`
- `data-raw/warm-start/issue-259-phase4-coverage-ledger.csv`
- `data-raw/warm-start/issue-259-phase4-test-results.csv`
- `man/fit_warm_start_model.Rd`
- `man/pairwiseLLM_warm_model.Rd`
- `man/predict.pairwiseLLM_warm_model.Rd`
- `man/warm_start_coefficients.Rd`
- `tests/testthat/README.md`
- `tests/testthat/helper-warm-start-pls.R`
- `tests/testthat/test-0114-warm-start-cv-plan.R`
- `tests/testthat/test-0116-warm-start-pls.R`
- `tests/testthat/test-3107-warm-start-format3-artifacts.R`
- `tests/testthat/test-9104-warm-start-shared-plan.R`
- `vignettes/adaptive-warm-start.Rmd`

Ignored local files also updated: `tasklists/issue-259/04-pls.md`,
`tasklists/issue-259/00-index.md`, and `tasklists/00-index.md`. They remain ignored.
User-owned untracked `data-raw/studies/` was neither inspected nor modified.
No provider, adaptive-selection, Phase B, Python backend, schema, golden fixture,
production model, release version or minimum-R change.

### Behavior and locked decisions

- Optional `pls` in Suggests (also reflected in codemeta), guarded only for fitting.
  Explicit `kernelpls`, no backend scaling/CV, centering enabled. Only PLS `ncomp`
  controls; explicit glmnet-only controls fail. Public defaults remain glmnet/v1.
- Common per-context component grid includes every inner-training matrix and
  context refit; centered non-LAPACK QR tolerance1e-7, rank/p/n-1 limits, cap10.
  Default1:limit; all explicit sorted positive integer candidates must be legal.
  No failed-candidate omission, algorithm fallback or automatic engine selection.
- Existing split-local preprocessing, context outcome mean/sample SD, weighted
  MSE/SE, numerical tie tolerance, OOF calibration and outer-validation ordering
  preserved. Minimum-error/1-SE choices favor fewer components. Degenerate/nonfinite
  fits fail with context; finite negative calibration slopes remain legal.
- Full format3 stores all candidate OOF predictions, fold losses, rank/dimension
  bounds and complete selections/calibration/outer records. Validators reconstruct
  losses, weighted summaries, choices, calibration and validation metrics, and bind
  partitions/scales to the unchanged CV plan. Original feature tables are not
  retained; recorded rank consistency is checked, but rank recomputation needs
  those original inputs. This does not imply authenticity of an artifact.
- Portable ordered beta and `Ymeans - Xmeans %*% beta`; selected ncomp in training
  hyperparameters, no fabricated alpha/lambda. PLS uses its own validator alongside
  the strict existing glmnet/legacy path. Full/reduced artifacts, coefficients,
  storage/registry/bundles, old cross-task ensembles and priors work without pls.
  Reduced artifacts retain explicit summaries and omit row/outer/rank evidence.

### Focused verification and coverage

Environment: R4.6.1, pls2.9.0, glmnet5.0, withr3.0.3, covr3.6.5, testthat3.3.2.
No package installation or download was needed.

Final scoped command:

```sh
PAIRWISELLM_TEST_PYTHON=/home/sterett/.virtualenvs/pairwisellm-writing-v1/bin/python Rscript --vanilla data-raw/warm-start/check-issue-259-phase4.R /tmp/issue259-phase4
```

Explicit filter `^(0031|010[0-9]|011[0-6]|310[0-27]|510[015]|910[0-24])-`;
`covr::environment_coverage(asNamespace("pairwiseLLM"), ...)`:
**29 files, 147 test blocks, 6,000 passed expectations; 0 failures/errors/warnings/skips**.
Both real pinned Python extraction paths ran. Frozen0113:148 passed; new0116:398
passed. See committed test-results CSV for per-file/block results. Follow-up
expectations below are not added to this total as distinct coverage evidence.

All24 measured warm-start R files exceed95% line coverage. Current materially
changed runtime files: PLS97/98=98.98%; PLSvalidation114/116=98.28%; CV111/113=98.23%;
engine27/28=96.43%; format3 74/74=100%; model140/140=100%; model-artifact109/109=100%;
validation107/107=100%. Documentation-only changed runtime files: coefficients
27/28=96.43%, predict29/29=100%. The committed coverage ledger includes every
measured file; this is scoped whole-file coverage, not package-wide coverage.
Uncovered new-file branches are the defensive nonfinite inner-prediction error
and two reduced-grid corruption checks. No claim of100% branch coverage.

Additional completed checks:

- `Rscript --vanilla -e 'testthat::test_local(filter="^(0031|0116|3107|9104)-", reporter="summary", stop_on_failure=TRUE, stop_on_warning=TRUE)'`: all four files passed.
- `Rscript --vanilla data-raw/warm-start/check-issue-259-pls-deployment.R`: full/reduced
  artifacts for both schemas passed in a fresh process whose library excludes
  pls/glmnet/reticulate; required namespaces were verified unavailable/unloaded.
- `devtools::document(quiet=TRUE)`: exactly four changed Rd topics listed above;
  no NAMESPACE/export change. Generated diffs inspected; all four parsed with
  `tools::parse_Rd()`.
- `pkgload::load_all(quiet=TRUE); rmarkdown::render("vignettes/adaptive-warm-start.Rmd",
  output_dir="/tmp/issue259-phase4-vignette", intermediates_dir="/tmp/issue259-phase4-vignette",
  quiet=TRUE)`: passed with the guarded synthetic shared-plan PLS example executed.
- Targeted lint plus final `lintr::lint_package()`: zero lints.
  `tools:::.check_packages_used_in_tests(".", "tests/testthat")`: zero undeclared
  dependency/import/data/parse/method findings. `git diff --check` and staged
  checks passed; 180 unique numeric prefixes, new0116 reservation documented.
- Exact Git comparisons: no changes to frozen schema/extraction/inventory/locks/
  fixtures. SHA-256 v1:1414573759c302dc24e9041cfe2eb084fb4be1fac1fd26440b2011d4f9a736f7;
  v2:d9f271abae50eeac6d06a9ab7304309932174d54193ba8fb26348e9e886b2492;
  legacyRDS:77fe4871283d7d938334d99438d5d54f0554c85f02d35ff1f6ba7c1a446433dc.
- Independent read-only statistical/source and final test/documentation reviews
  found no blocker. Local metadata consistency suggestion (codemeta pls) applied.

Resolved development findings: the first rank-grid test accidentally crossed the
existing missingness threshold; corrected synthetic missingness without changing
preprocessing. An artifact test referenced a helper scoped to0113; replaced it
with a direct frozen-fixture read. A documentation assertion expected “stored
elastic-net”; expanded source wording to “stored elastic-net or PLS”. The first
coverage collection was interrupted after that known assertion failure; only the
completed final collection above supplies current counts/coverage. Brace/quote
lint findings were corrected before final verification. No statistical-contract
change was needed.

Temporary supplemental evidence: `/tmp/issue259-phase4-{coverage,focused-final,
engine-artifacts,isolated,vignette,lint-clean,dependency-audit}.log`, plus coverage
RDS/line CSV. Durable result CSVs and this report remain when temporary logs expire.
No unfiltered local devtools::test/check/package_coverage, production/study fitting,
provider calls or MCMC was run. CI performs its configured checks independently.

### Delivery and next-phase invariants

At implementation head, pkgdown SUCCESS; the other six CI checks are running.
Final-head CI/readiness is recorded in the PR/local handoff after the reporting
push. Maintainer review and Phase7 full local release/platform validation remain
pending. No known unresolved local defect. Isolated-library execution was Linux;
no cross-platform Python/backend-free library-isolation claim is made.

Phase5 must preserve both schemas/hashes/order, all frozen evidence, reusable
CV plans and R4.7 RNG provenance, split-local preprocessing/context outcome scales,
weighted tuning/ties, OOF calibration, strict full/reduced format3 audits and PLS
portable deployment. It adds only the frozen RBF-SVR engine, leaving algorithm
ensembles to Phase6. No merge/tag/publication or Phase5 work has occurred.

## Historical Phase 5 entry prompt

Start Phase5 in a new thread using this exact prompt (verify the latest PR head,
including the reporting commit, before branching):

```text
Implement Phase 5 (RBF-SVR) of issue #259 in shmercer/pairwiseLLM.

Phase 4 PR: https://github.com/shmercer/pairwiseLLM/pull/263
Branch: feat/259-04-pls
Implementation commit: d810f2430eedcefb3c90f59b57bfcf6cd03873a4
Base: master at 9867bf18a3e017623235c6040ebb61d960c46573
Use the verified final PR head, including its reporting commit and later review fixes.

Read AGENTS.md, tasklists/00-index.md, tasklists/issue-259/05-svr-rbf.md,
data-raw/warm-start/issue-259-design.md, data-raw/warm-start/issue-259-handoff.md,
and the complete issue #259. Reconstruct missing ignored tasklists from the
tracked design/handoff. Verify current Git/PR/CI state and reconcile review fixes.
Create feat/259-05-svr-rbf from the verified Phase 4 head, targeting its branch.
If Phase 4 has merged, use its reviewed merge and target master instead.

Implement optional e1071 in Suggests, epsilon-regression with radial kernel,
scale=FALSE, cross=0, and probability=FALSE. Defaults are cost=2^(-2:4),
gamma_multiplier=2^(-2:2), and fixed epsilon=0.10. Each fit uses actual
gamma=gamma_multiplier/its retained predictor count. Use the locked weighted
MSE/SE rules; minimum-error ties and eligible 1-SE choices favor lower cost,
then lower gamma multiplier. Preserve complete tuning/OOF/outer audits,
contextual errors, deterministic shared partitions and leakage boundaries.

Store only portable numeric support vectors, dual coefficients, rho and actual
gamma, preserving feature order and matrix dimensions. Prediction is
exp(-gamma*squared_distance) %*% dual - rho, without e1071. Verify backend
prediction equivalence, full/reduced artifacts, storage/registry/bundle/prior
interoperability and deployment without e1071. Nonlinear coefficient requests
must produce informative typed errors rather than fabricated linear weights.
Do not store opaque backend fits.

Preserve PLS, legacy glmnet behavior, v1/v2 extraction, hashes/order, frozen
fixtures, reusable CV plans including R4.7 RNG provenance, format-3 audits,
and every statistical contract. V2 schema SHA-256:
d9f271abae50eeac6d06a9ab7304309932174d54193ba8fb26348e9e886b2492
Do not inspect study outcomes or modify unrelated data-raw/studies/.

You are authorized to implement, test, commit, push, and create/update the
Phase 5 PR. Do not merge, tag, publish, or start Phase 6. Update durable/local
handoffs and provide the exact Phase 6 new-thread prompt.
Package remains 1.5.1 until Phase 7.
```
