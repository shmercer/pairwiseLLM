# Phase 7 CI review and downstream pinning

Use this prompt after the user supplies Phase 7 CI results:

```text
Review user-supplied CI results for issue #259 Phase 7 in shmercer/pairwiseLLM.

Read AGENTS.md, tasklists/00-index.md, tasklists/issue-259/07-docs-quality.md,
data-raw/warm-start/issue-259-design.md, issue-259-handoff.md, the Phase 7
validation report/ledgers, and the complete issue #259. Reconstruct missing
ignored tasklists from the tracked design/handoff. Reconcile the latest feedback.

Expected branch: feat/259-07-docs-quality; PR base: master.
Expected package version: 1.5.2; R >=4.4. Resolve the exact delivered candidate
SHA from the Phase 7 PR and final local handoff; the implementation SHA in the
tracked report is not automatically the final reporting/PR head. Verify both
Git and PR state before changing anything. Do not poll pending CI.

Inspect the user-reported completed results for that exact head. Fix attributable
failures narrowly, preserving the frozen v1/v2 schemas, legacy fixtures, all three
engines/grids, shared CV identity and R4.7 RNG provenance, complete format-3 audits,
standalone algorithm_ensemble format1, aligned outer validation, equal calibrated
means, summary-only limits, SVR matrix shape/order, typed coefficient errors,
backend-free prediction, registry/bundle identity, and prior SD authority.

Keep complete warm-start/adaptive-pairing guides readable for non-technical users.
Keep all current-release version references synchronized. Preserve historical
release/fixture provenance. Do not use study outcomes, fit production models,
change provider collection, adaptive selection/stopping, Phase B algorithms,
or install dependencies automatically.

If code or package content changes, record the new candidate SHA and rerun the
checks affected by those changes; retain current per-file coverage evidence.
Update durable/local handoffs, commit/push authorized corrections, then end and
wait for user-supplied CI results. No merge, tag or publication without a separate
explicit instruction. Do not initiate downstream study fitting or replay.
```

## D042 validation instructions

The final reviewed and tested Phase 7 PR head is a candidate pin. A later squash
merge has a different SHA, even when its tree matches; record and validate that
merge explicitly before adopting it as the downstream D042 pin.

1. Record the exact checkout SHA, package version 1.5.2, R/platform, dependency
   versions, source/archive identity, and the PR/merge used. Confirm the package
   loaded by R was built from that checkout, rather than an older installed copy.
2. Compare the reviewed PR tree and intended merge tree, and reconcile every
   intervening change. Tree identity is evidence about content, not interchangeable
   commit identity. Re-run compatibility/interoperability validation on the pin.
3. Run the frozen legacy regression test 0113 and schema/extraction tests 0100–0102
   and 0115. Preserve exact schemas/order/missingness, with v2 SHA-256
   `d9f271abae50eeac6d06a9ab7304309932174d54193ba8fb26348e9e886b2492`.
   Actual extraction additionally requires the already provisioned pinned Python
   environment; record whether that path ran and its platform.
4. Run shared-plan/engine tests 0114, 0116, 0117; format-3 artifact and workflow
   tests 3107 and 9104; algorithm-ensemble tests 0118, 3108 and 9105; and the
   documentation deployment workflow 9106 in the source checkout. Include prior
   and adaptive tests 0110–0112 and 5100/5101/5105/6100. Use deterministic offline
   fixtures; do not evaluate a substantive study model during this validation.
5. Run the PLS, SVR, and algorithm backend-free deployment scripts in
   `data-raw/warm-start/`. Verify full/reduced artifacts and preserved numeric
   payload dimensions/order, calibrated predictions, registry identity and priors.
6. Confirm CI/review belongs to the exact pin, and record any platform, optional
   dependency, coverage, or check limitations. If package code changes, refresh
   affected tests/coverage/checks; do not carry forward results from stale source.

These instructions authorize no study execution. Adopt D042 under the downstream
study's governance before substantive v2 fitting or replay; neither synthetic
package tests nor a successful ensemble constructor establish predictive validity.
