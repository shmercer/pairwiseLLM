# Issue 259 execution and thread handoff

Authority: [issue #259](https://github.com/shmercer/pairwiseLLM/issues/259) and
[the architecture contract](issue-259-design.md). The user approved seven stacked
PRs, one implementation phase per thread, and version 1.5.2 in Phase 7.
Do not use study outcomes to choose features, tuning, engines or exceptions.

## Phase and branch map

| Phase | Scope | Branch (prefix feat/259-) | PR base | Status |
|---|---|---|---|---|
| 1 | Architecture, frozen regression evidence, handoffs | 01-architecture | master | In progress |
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

Status: in progress on feat/259-01-architecture. Verified parent master and
origin/master at 013f869b1ba2272b88e85937023d57eb2c2c99fa; package 1.5.1, R >=4.4.
Untracked data-raw/studies/ is user-owned and excluded from this work.

Files introduced/changed:

- issue-259-design.md: complete architecture, audit inventory, statistical and
  artifact boundaries, approved v2 membership and sequencing.
- issue-259-handoff.md: seven-phase Git/thread workflow and completion evidence.
- data-raw/warm-start/README.md: links to the development design/ledger.
- capture-issue-259-baseline.R: explicit guarded synthetic historical capture.
- tests/testthat/README.md: reserve 0113-0119, 3107-3109, 9104-9106.
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

Validation results and coverage ledger will be recorded before ready-for-review.
Current dependency inventory: R4.6.1, glmnet5.0, pls2.9.0, e1071 1.7.17,
testthat3.3.2, covr3.6.5, pkgload1.5.3, lintr3.4.0, withr3.0.3.

User review and final full/package/platform validation remain pending. Full tests,
CRAN-style check, docs/rendering and current new-path coverage are Phase 7 work,
not claims made by a successful Phase 1 fixture fit.

## Next-thread prompt

At Phase 1 completion this block will be updated with the implementation SHA
and PR. Open a new Codex thread in the same repository; no manual branch switch
is necessary. Continue Phase 2 only after the handoff is marked complete.

```text
Implement Phase 2 of GitHub issue #259 in shmercer/pairwiseLLM.

Read AGENTS.md, tasklists/00-index.md, tasklists/issue-259/02-cv-engines.md,
data-raw/warm-start/issue-259-design.md and issue-259-handoff.md, and the complete
issue #259 before editing. Reconstruct an absent ignored tasklist from the
tracked design/ledger rather than relying on a previous conversation.

Continue from feat/259-01-architecture at the verified predecessor recorded in
the handoff/PR. Verify actual Git/PR state and reconcile changes since handoff.
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
