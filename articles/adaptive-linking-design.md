# Design: Adaptive Linking

## Foundational concepts

Linking places separately estimated sets of comparative judgment (CJ)
items on one scale. Within-set Bradley–Terry–Luce (BTL) evidence
identifies shape, not the location difference between sets. Hub and
spoke shapes are separately centered, and an explicit offset `delta`
represents the missing location.

The three supported estimators require explicit selection. None is the
package default during study development. See the [practical
guide](https://shmercer.github.io/pairwiseLLM/articles/adaptive-linking.md),
[executable session
examples](https://shmercer.github.io/pairwiseLLM/articles/linking-sessions.md),
and [within-set
design](https://shmercer.github.io/pairwiseLLM/articles/within-set-adaptive-design.md).

## Phase A artifacts and validation

Phase A inputs are estimator-specific:

| Estimator | Phase A evidence | Updated parameters |
|----|----|----|
| `fixed_shape_offset` (E1) | Centered point summaries | Offset only |
| `gaussian_posterior_bridge` (E2) | Joint posterior draws or full reduced-coordinate Gaussian bridges | Shapes and offset |
| `joint_offset` (E3) | Raw within-set observations and the original model/prior contract | Shapes and offset |

E1 treats shape as fixed. Its uncertainty is conditional and must not be
presented as full item uncertainty. E2 preserves the full within-set
covariance, including negative covariances induced by centering. E3 uses
each Phase A observation once; a posterior derived from those same
observations cannot also serve as its prior. E3 supports maximum a
posteriori (MAP)/Laplace inference and an explicitly requested Markov
chain Monte Carlo (MCMC) audit engine.

Pass `list(artifact = artifact)` for each set to
[`prepare_link_input()`](https://shmercer.github.io/pairwiseLLM/reference/prepare_link_input.md)
and then call
[`start_link_session()`](https://shmercer.github.io/pairwiseLLM/reference/start_link_session.md)
on the prepared input. Historical artifacts are checked by their
contents and evidence contracts, not by their age. Missing required
covariance or raw evidence cannot be synthesized from marginal
summaries.

## Common-scale identification

All estimators use the shared offset prior `delta ~ Normal(0, 5^2)` by
default. Cross-set observations are explicit, ordered data: item
identities, A/B orientation, judge parameters and outcomes must agree
with the input contract. With zero cross edges the offset remains
prior-only and is reported as unidentified.

[`fit_link()`](https://shmercer.github.io/pairwiseLLM/reference/fit_link.md)
returns linked means, uncertainty with an explicit scope, offset
summaries, prediction state, covariance where defined, diagnostics,
evidence hashes and continuation. Missing uncertainty stays missing.
Each hub/spoke fit is separate; E2/E3 can update the hub differently for
different spokes. Reports retain spoke identity and never silently pool
these hub posteriors.

## D-optimal active selection

Adaptive Phase B selection is unavailable for every estimator, including
E3-MCMC, pending a separate selector-validation study. Posterior
covariance alone does not validate a D-optimal selection rule. No legacy
selector or estimator fallback exists. Within-set and Phase A adaptive
ranking retain their existing behavior.

## Probes and calibration

Pair selection is external to estimation. Held-out probes remain
separate from active cross evidence. Common prediction hooks preserve
A/B orientation and judge parameters. Probe caches bind predictions to
estimator, evidence and input identity. Linked rubric calibration uses
compatible common results and the exact Phase A hub reference; it does
not reinterpret a removed estimator’s posterior.

## Stopping, blockers, and freezing

Explicit sessions use caller-supplied `active`, `probe`, or `frozen`
reporting status. These labels do not change which observations are
fitted. Frozen spokes cannot append evidence until explicitly
reactivated. No adaptive stopping rule is enabled by these sessions.
Conditional E1 uncertainty does not establish full-uncertainty
reliability, and prior-only identification cannot pass linking stopping
criteria.

## Persistence and resume

[`save_link_session()`](https://shmercer.github.io/pairwiseLLM/reference/save_link_session.md)
and
[`load_link_session()`](https://shmercer.github.io/pairwiseLLM/reference/save_link_session.md)
preserve exact common results, input hashes and continuation state.
Identical resume is a no-op; new evidence must extend the old prefix
without changing Phase A, item order, judge or numerical mode.

Saved anchored-joint Phase B sessions are unsupported. Loading, resuming
or reporting them raises `pairwiseLLM_unsupported_legacy_link_state` and
directs the caller to restart from compatible Phase A artifacts/evidence
with an explicit estimator. The Phase B posterior is never migrated and
no estimator ID is backfilled.

## Citation

Mercer, S. H. (2026). *Design: Adaptive Linking* \[R package vignette\].
Comprehensive R Archive Network.
<https://doi.org/10.32614/CRAN.package.pairwiseLLM>
