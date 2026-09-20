# Exact Phase 6 new-thread prompt

```text
Implement Phase 6 (same-task algorithm ensemble) of issue #259 in shmercer/pairwiseLLM.

Phase 5 branch: feat/259-05-svr-rbf; PR base: master.
Phase 5 implementation commit: 5f9eb32fa60745f2a0512d6e41514b9fc6a4055e
Phase 5 base: reviewed Phase 4 merge eb570463e1da636ee5b6670231e012b0dba7bfbb.
Use the verified final Phase 5 PR head, including its reporting commit and later
review fixes, or its reviewed merge. Phase 5 CI was pending at delivery: the user
requested no polling and will supply results. Reconcile their latest feedback.

Read AGENTS.md, tasklists/00-index.md, tasklists/issue-259/06-algorithm-ensemble.md,
data-raw/warm-start/issue-259-design.md, data-raw/warm-start/issue-259-handoff.md,
and the complete issue #259. Reconstruct missing ignored tasklists from the
tracked design/handoff. Verify current Git/PR state and reconcile reviewed fixes.
Create feat/259-06-algorithm-ensemble from the verified Phase 5 head, targeting
feat/259-05-svr-rbf; if Phase 5 has merged, use its reviewed merge and target master.

Implement ensemble_warm_start_algorithms(...) as a distinct same-task API with
standalone class pairwiseLLM_warm_algorithm_ensemble, ensemble format 1 and
artifact_type="algorithm_ensemble". Require at least two named, fully audited
format-3 model components with identical task, ordered IDs, exact outcome identity,
schema and CV plan, learned oof_linear calibration and complete aligned outer
validation. Preserve component names/order; reject nested ensembles, legacy or
reduced construction inputs and incompatible evidence.

Deployment is the equal arithmetic mean of each component's calibrated prediction.
Honest ensemble validation uses aligned outer-held-out calibrated component
predictions and their matching outer-context observed outcomes. No learned
weights, ensemble-level recalibration, automatic component selection, or prior SD
from algorithm disagreement. Any between-component SD is diagnostic only.

Implement full/reduced validation, prediction/print/summary, save/load, preparation,
registry, bundle records, prediction generation and warm-prior/adaptive initialization
interoperability. Reduced artifacts retain portable component payloads, identity
and labeled validation summaries without inventing missing audit evidence. Preserve
SVR support-vector matrix dimensions/order and backend-free deployment; nonlinear
coefficient requests must remain informative typed errors.

Preserve ensemble_warm_start_models() cross-task semantics, including duplicates
and component-only validation; preserve glmnet/PLS/SVR fitting, locked grids,
weighted MSE/SE/ties, context outcome scales, OOF calibration, shared partitions
and R4.7 RNG provenance. Preserve v1/v2 extraction, hashes/order, frozen fixtures,
legacy formats 1/2 and format-3 audits. V2 schema SHA-256:
d9f271abae50eeac6d06a9ab7304309932174d54193ba8fb26348e9e886b2492
Do not inspect study outcomes or modify unrelated data-raw/studies/, provider
collection, adaptive selection/stopping or Phase B algorithms.

You are authorized to implement, run focused checks, commit, push, and create/update
the Phase 6 PR. Update durable/local handoffs and provide the exact Phase 7
new-thread prompt. Once the PR is open, end the turn and wait for user-supplied
CI results rather than polling. Do not merge, tag, publish, or start Phase 7.
Package remains 1.5.1 and R >=4.4 until the approved Phase 7 release work.
```
