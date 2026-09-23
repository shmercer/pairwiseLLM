# pairwiseLLM 1.6.0 linking contract

This is the downstream integration contract for epic #273 and issue #281, based
on frozen study revision `2bf3f0b4a2f257b7855f965f782f05c61853aac6`.
No estimator is selected by default. Adaptive Phase B selection remains gated.

## Entry points and Phase A requirements

| Estimator ID | Phase A statistical input per set | Engine | Uncertainty |
| --- | --- | --- | --- |
| `fixed_shape_offset` | `list(points = named_numeric_vector)` | Quadrature | Offset only, conditional on fixed shapes |
| `gaussian_posterior_bridge` | `list(draws = matrix)` with named item columns and joint posterior draws | MAP/Laplace | Joint shapes and offset, approximately |
| `joint_offset` | `list(observations = data.frame)` with original within-set judgments and compatible standard-Normal shape priors | MAP/Laplace; explicit `control$estimator$engine = "mcmc"` for audit | Joint shapes and offset |

Each also accepts `list(artifact = artifact)` if the canonical original Phase A
artifact contains that method's required input. E1/E2 do not replay raw Phase A
outcomes. E3 does not use Phase A posterior summaries as an additional prior.

`prepare_link_input(estimator, hub, spoke, phase_a, cross, judge, control,
provenance)` creates the input. Each identity is a `list(set_id, items)`;
`items` contains `item_id` and optional unique `global_item_id`. All evidence
rows contain `observation_id`, `A_set`, `A_item`, `B_set`, `B_item`, `y_A`.
One means A won. Judgment IDs must be unique across raw evidence blocks;
legitimate repeated pairs have distinct judgment IDs. Keep final/probe outcomes
outside `cross`; a session status is not an evidence partition.

`judge` contains fixed `beta`, `epsilon`, `model_variant`, `link = "logit"`,
and `source`. Omitted model components must equal zero. The common default offset
prior is `list(mean = 0, sd = 5)` under `control$delta_prior`. E3 shape priors are
independent standard Normals in orthonormal sum-to-zero coordinates. See the
function help for numerical controls and diagnostic thresholds.

## Outputs and prediction

`fit_link(input, previous = NULL)` returns a `pairwiseLLM_link_result`, schema 1.
Its top-level fields are `schema_version`, `estimator_id`, `estimator_version`,
`items`, `offset`, `uncertainty`, `prediction`, `diagnostics`, `provenance`,
and `continuation`. Estimator versions are currently `"1"`.

- Items include `set_id`, `item_id`, `global_item_id`, `theta_link_mean`,
  `theta_link_eap`, `theta_link_sd`, `theta_link_lower`, `theta_link_upper`, `rank_link`.
  `theta_link_eap` aliases `theta_link_mean`. E1 and E3-MCMC report posterior
  means; E2/E3 MAP report the Laplace center. Intervals are 95%.
- Offset fields: `delta_mean`, `delta_sd`, `delta_lower`, `delta_upper`,
  `identification` (`prior_only`, `cross_set`, `unidentified`, or `failed`).
- `uncertainty` contains named free-coordinate `covariance` (or `NULL`), `basis`
  and `item_transform`. Item covariance is `T %*% V %*% t(T)` for that transform
  and covariance. The uncertainty scope is in `diagnostics` and item rows.
  Undefined marginal uncertainty is explicitly missing. E1's zero hub
  variance is conditional on fixed shape, not full Phase A uncertainty.
- Diagnostics include `fit_attempted`, `fit_valid`, `covariance_valid`,
  `failure_code`, `uncertainty_scope`, and method-specific numerical/audit details.
  Inspect optimizer/gradient/Hessian results, bridge jitter or sampler audit
  results as appropriate; failed fits remain inspectable.
- `predict_link(result, pairs)` accepts the five identity
  columns above without `y_A` and returns probabilities that A wins, integrating
  over the estimator's uncertainty. Ordered presentation and fixed bias matter.

`start_link_session()`, `resume_link_session()`, `save_link_session()` and
`load_link_session()` provide schema-1 sessions. Resume requires an unchanged
ordered cross-evidence prefix, original Phase A inputs, identities and settings.
Identical resume is exact reuse. The previous posterior never becomes a new prior.
Multiple spokes preserve independent fits and distinct hub posteriors. Logs and
summaries expose estimator/uncertainty identity, offset, counts and diagnostics;
removed anchored-joint fields are not populated with invented equivalents.

## Hash and provenance semantics

`provenance` includes `package_version`, `source_commit`, `hash_scheme`,
`hash_engine_version`, `hashes`, `counts`, and method-relevant source metadata.
Supply `source_commit` explicitly; unknown revisions remain `NA_character_`.
The `link-v1-rlang` scheme uses `rlang::hash()` and records its version.

Computed hashes bind normalized inputs, identities, judge and controls. Item
order is canonicalized, but evidence row order is retained and hashed. Row
permutations may yield equivalent estimates with different evidence hashes.
Artifact hashes cover the exact original artifact; declared external source
hashes are provenance assertions, not substitutes for computed payload hashes.
Raw Phase A likelihood counts are zero for E1/E2. Unknown original source counts
are missing, not zero. `provenance$expected` on input can enforce expected hashes
and counts. Save/resume rejects reordered, removed or altered historical evidence.

## Release verification and exact revision

Run the repository's `scripts/linking-release-smoke.R` from any working directory:

```
Rscript /path/to/pairwiseLLM/scripts/linking-release-smoke.R \
  /path/to/pairwiseLLM /path/to/local-output.json
```

The runner loads that checkout and records its full Git SHA, dirty status, package
and tool versions, common cross-evidence hash, per-method input requirements,
output/diagnostic field names, identification, counts and persistence checks.
All data are synthetic; no provider or downstream study outcomes are read.
Only a clean, tested revision is a candidate pin. The reviewed PR head is distinct
from a later merge SHA; downstream production adoption must record the revision
actually used. The PR and local handoff carry validation results for that SHA.

Old anchored-joint Phase B states raise
`pairwiseLLM_unsupported_legacy_link_state`; there is no posterior migration or
fallback. Compatible original Phase A artifacts can be used to start a new link.

## Requirement-to-test matrix

Test file numbers refer to `tests/testthat/test-NNNN-*.R`. Ordinary tests are
provider-free; 6021 explicitly enables real CmdStan on synthetic data only.

| Release requirement | Primary automated evidence |
| --- | --- |
| Item ordering, relabeling, evidence ordering, sign and presentation invariance | 5093 shared E1–E3; 5075/5079/5083 independent method cases; 6021 real MCMC |
| Repeatability and provenance | 5093; 5070/5072/5073; 6021 seeded real MCMC |
| Zero-edge prior and identification | 5075/5079/5082; 6021 real MCMC |
| Held-out evidence isolation and reversed-pair overlap rejection | 5090/5093; common preparation/prediction contracts also apply to MCMC |
| Legitimate repeats and malformed evidence reuse | 5070/5093/5094; 6021 repeated Stan likelihood vs independent R density |
| E1 dense integration oracle | 5075, including displaced and separated mass |
| E2 Gaussian objective, derivatives, covariance and bounded jitter | 5078–5081 |
| E3 independent objective, derivatives, covariance and MCMC parity | 5082–5085, 6021 |
| Historical double-use regression | 5094 raw Phase A doubling and unused artifact outcomes; 5070/5088 incompatible input rejection |
| Known offset, positive/negative symmetry, weak/strong evidence, bias, lapse and near separation | 5095; real MCMC density/parity and symmetry in 6021 |
| Phase A run/import/mixed and compatible artifacts | 9003 (all E1–E3), 5088 |
| One/multiple spokes, exact save/resume, common reporting | 5086/5087, 9002/9003; MCMC draw persistence in 5086 |
| Original-hub rubric calibration transport | 5089/9200 |
| Legacy failure and unavailable selectors | 5091/5092 |
| Identical-evidence downstream public API | `scripts/linking-release-smoke.R` |

Full `testthat`, `R CMD check`, offline vignette/site rendering, coverage and the
active-documentation search are additional release gates. Mock sampler tests
exercise plumbing and failure handling; they do not replace the real 6021 audit.
