# Issue 259 execution and thread handoff

Authority: [issue #259](https://github.com/shmercer/pairwiseLLM/issues/259) and
[the architecture contract](issue-259-design.md). The user approved seven stacked
PRs, one implementation phase per thread, and version 1.5.2 in Phase 7.
Do not use study outcomes to choose features, tuning, engines or exceptions.

## Phase and branch map

| Phase | Scope | Branch (prefix feat/259-) | PR base | Status |
|---|---|---|---|---|
| 1 | Architecture, frozen regression evidence, handoffs | 01-architecture | master | Complete; PR #260, user review pending |
| 2 | CV plans, format 3, compatible glmnet and single-model interoperability | 02-cv-engines | Phase 1 branch | Not started |
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

## Next-thread prompt

**Start a new thread for Phase 2 using this prompt.** No manual branch switch is
necessary. PR #260 review may proceed alongside the next stacked phase; do not
infer merge permission. Reverify the PR head because later review fixes can
supersede the implementation SHA below.

```text
Implement Phase 2 of GitHub issue #259 in shmercer/pairwiseLLM.

Read AGENTS.md, tasklists/00-index.md, tasklists/issue-259/02-cv-engines.md,
data-raw/warm-start/issue-259-design.md and issue-259-handoff.md, and the complete
issue #259 before editing. Reconstruct an absent ignored tasklist from the
tracked design/ledger rather than relying on a previous conversation.

Predecessor: feat/259-01-architecture, PR https://github.com/shmercer/pairwiseLLM/pull/260.
Fixture/test implementation: 7afa871c37d7157ca605630c94774b28ff336f3f; use the
verified final PR head including the later completion report/LF checkout rule.
Verify actual Git/PR state and reconcile changes since handoff.
Create feat/259-02-cv-engines from that predecessor (or its reviewed merge).
Preserve unrelated files, especially data-raw/studies/, and do not inspect study
outcomes. Frozen fixtures must not be refreshed to make regressions pass.

Implement reusable task/outcome-bound CV plans and engine-neutral format3 with
compatible glmnet behavior. Include single-model save/reduce/registry/bundle/
prior interoperability now. Do not change v1 extraction or implement later phases.

You are authorized to run required checks, make small commits, push, and create/
update the Phase 2 PR against its predecessor. Do not merge, tag, publish, or
start Phase 3. Update tracked/local handoffs and provide the exact Phase 3
new-thread prompt before ending.
```

For a mid-phase checkpoint, say "Resume Phase N" and identify the remaining
subphase; use its existing branch/PR. A blocked handoff must name the unresolved
decision and explicitly state that the dependent phase must wait.
