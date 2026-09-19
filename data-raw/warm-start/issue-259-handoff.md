# Issue 259 execution and thread handoff

Authority: [issue #259](https://github.com/shmercer/pairwiseLLM/issues/259) and
[the architecture contract](issue-259-design.md). The user approved seven stacked
PRs, one implementation phase per thread, and version 1.5.2 in Phase 7.
Do not use study outcomes to choose features, tuning, engines or exceptions.

## Phase and branch map

| Phase | Scope | Branch (prefix feat/259-) | PR base | Status |
|---|---|---|---|---|
| 1 | Architecture, frozen regression evidence, handoffs | 01-architecture | master | Complete; PR #260 merged to reviewed master |
| 2 | CV plans, format 3, compatible glmnet and single-model interoperability | 02-cv-engines | master (Phase 1 merged) | Complete; PR #261, review/CI pending |
| 3 | Audited v2 schema commit, extraction and audit | 03-writing-v2 | Phase 2 branch | Not started |
| 4 | PLS backend and portable linear deployment | 04-pls | Phase 3 branch | Not started |
| 5 | RBF-SVR backend and portable kernel deployment | 05-svr-rbf | Phase 4 branch | Not started |
| 6 | Same-task algorithm ensemble and all consumers | 06-algorithm-ensemble | Phase 5 branch | Not started |
| 7 | End-to-end docs, full validation, 1.5.2 | 07-docs-quality | Phase 6 branch | Not started |

Fetch and verify actual upstream/PR state before each phase. Build a child branch
from the verified predecessor tip, or its reviewed merge result if already merged.
Open a draft phase PR, then mark ready after checks and handoff. Use small commits
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

## Next-thread prompt

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
