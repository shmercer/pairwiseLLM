# Test Suite Conventions

## Naming & Block Ranges

Test files must follow the numeric prefix convention below to keep the suite
organized and execution order predictable.

New major subsystems should reserve a contiguous numeric block before tests
are added.

Rubric calibration reserves 0200–0299 for core/statistical-contract tests and
9200–9299 for workflow/integration tests. These blocks were verified unused
before Task 01 of the v1.5.0 series; do not renumber unrelated tests.

- 0000–0999: Core utilities, helpers, and pure functions
  - 0034: MCMC resource allocation and scheduling contracts
- 2000–2999: Live / interactive backends (e.g., human, API-based)
- 3000–3999: Batch workflows, resume logic, persistence
  - 3070: Shared batch retrieval retry and cross-provider recovery regressions
- 4000–4999: OpenAI parameter and endpoint compatibility regressions
- 5000–5999: Adaptive pairing and ranking
  - State objects and invariants
  - Candidate generation and utility computation
  - Batch selection (exploration / exploitation / duplicates)
  - Stopping criteria and diagnostics
  - Output schemas (round logs, item summaries)
- 6000–6999: Bayesian sampler and fit integration contracts
- 9000–9999: End-to-end and integration tests

Notes specific to adaptive pairing tests:
- Prefer small synthetic problems (typically N ≤ 12) unless explicitly testing
  performance or scaling behavior.
- Snapshot tests are encouraged for adaptive output schemas, configuration
  objects, and state structure.
- Adaptive tests should validate invariants (degree counts, duplicate limits,
  ordering reversals, hard caps), not just nominal outputs.

Link estimator foundations reserve 5070–5074 for input/evidence contracts,
reduced coordinates, results, prediction, and continuation (#275). These prefixes
were verified unused on 2026-09-22.
E1 fixed-shape linking reserves 5075–5077 for quadrature/oracle inference,
prediction/continuation, and numerical failures (#276); verified unused on 2026-09-22.
E2 Gaussian bridge linking reserves 5078–5081 for bridges/artifacts,
inference/oracles, prediction/continuation, and numerical failures (#277);
verified unused on 2026-09-22.

E3 joint-offset linking reserves 5082–5085 for inference/evidence, invariance and
prediction, failures, and MCMC contracts; 6021 contains opt-in real synthetic Stan
parity tests (#278). Prefixes verified unused on 2026-09-22. Run 6021 with
`PAIRWISELLM_TEST_E3_STAN=true`; ordinary tests never compile or sample implicitly.

## Determinism Rules

- No randomness without explicit local seeding.
- Use `withr::local_seed()` whenever a test needs random numbers.
- Avoid relying on system time, implicit ordering, or global RNG state.

## Mocking Rules

- All mocks must be created **inside** `test_that()` blocks.
- Use `with_mocked_bindings()` or `local_mocked_bindings()` to avoid leakage.
- Always restore options, environment variables, and working directories via
  `withr::local_*()` helpers.
- Avoid mocking adaptive core helpers in coverage tests (leakage risk under
  `pkgload::load_all()`): `.adaptive_get_refit_fit()`, `sample_exploration_pairs()`,
  `compute_stop_metrics()`, `should_stop()`, `compute_pair_utility()`,
  `apply_degree_penalty()`, `generate_candidates()`.
- Prefer constructing small deterministic `state`, `fit`, and `candidates` data
  to reach the branch instead of mocking; this keeps full-suite behavior stable.
- Do not override base generics or assign methods in `.GlobalEnv` (e.g., `sum`,
  `Ops.*`); these can leak across tests even when scoped.

## Function Availability (namespaces)

- The full test suite may run in contexts where `pairwiseLLM` is not attached. `tests/testthat/setup.R` ensures `pairwiseLLM` functions are available for tests (exported + internal) to prevent order-dependent "could not find function" errors.
- Prefer explicit `pairwiseLLM::` / `pairwiseLLM:::` qualification (or a per-file alias at the top of the file) for clarity and to keep tests robust if the setup behavior changes.
- When mocking:
  - mock internal functions with `testthat::with_mocked_bindings(..., .package = "pairwiseLLM")`
  - do not pass the package namespace as `.env`: it controls cleanup lifetime, not the
    target package, and leaks mocks into later tests
  - mock imported functions with `testthat::with_mocked_bindings(..., .package = "<pkg>")` (e.g., `"httr2"`)

## Temp Files & Directories

- Use `withr::local_tempdir()` for temporary directories.
- Create temporary files under that directory to prevent cross-test collisions.
- Clean up only if needed; `withr` will handle most cleanup automatically.

## Network & API Keys

- Tests must never make network calls.
- Do not depend on API keys or real credentials.
- Use deterministic fixtures and mocked responses instead.

## Warm-start reservations and numbering audit

Every `test-*.R` file has a unique four-digit prefix. Helpers and setup files do not.
Warm-start blocks are 0100–0199 (schema, extraction, model and prior contracts),
3100–3199 (artifact storage and registry), 5100–5199 (adaptive integration),
6100–6199 (sampler prior integration), and 9100–9199 (end-to-end/maintainer workflows).
The 0100 block includes optional extraction/training boundaries alongside pure contracts.
Sparse replay reserves 5107–5108 for reservoir contracts/selection and 9103 for
reservoir workflow integration; these prefixes were verified unused for issue 257.
Issue 259 reserves 0113–0119 for legacy regression, shared CV, v2 features,
PLS, RBF-SVR, algorithm ensembles, and engine-boundary coverage, respectively;
3107–3109 for new artifact/registry contracts; and 9104–9106 for shared-plan,
algorithm-ensemble, and deployment workflows. All were verified unused on
2026-09-19. Existing 3103–3106 belong to adaptive persistence; do not reuse them.
Phase 2 uses 0114 for CV-plan/format-3 core contracts, 3107 for format-3 artifact
interoperability, and 9104 for shared-plan deployment workflows.
Phase 3 uses 0115 for v2 schema, extraction, missingness and format-3 interoperability.
Phase 4 uses 0116 for PLS components, fitting and audit contracts, and extends
3107/9104 for PLS artifacts, shared partitions and deployment.
Phase 5 uses 0117 for RBF-SVR fitting, tuning and audit contracts, and extends
3107/9104 for portable kernel artifacts, shared plans and deployment.
Phase 6 uses 0118 for same-task algorithm ensembles, 3108 for their full/reduced
artifacts and registry/bundles, and 9105 for prior/adaptive deployment.
Phase 7 uses 9106 for the packaged offline vignette data, real optional extraction,
and the executable documentation workflow through all predictive modes.
Keep 0025, 5025, 5027 and 6020 before 6100 when running mock-restoration regressions.
Existing 4000-series tests retain their compatibility grouping; 6020 is a sampler test.

Task 09 renamed the following files; earlier completion reports retain historical names.

| Previous filename | Current filename |
| --- | --- |
| `test-0015-adaptive-schemas.R` | `test-0028-adaptive-schemas.R` |
| `test-0016-adaptive-state.R` | `test-0029-adaptive-state.R` |
| `test-0018-btl-mcmc-theta-summary-stubs.R` | `test-0030-btl-mcmc-theta-summary-stubs.R` |
| `test-5016-reversal-on-repeat-invariant.R` | `test-5059-reversal-on-repeat-invariant.R` |
| `test-5055-targeted-coverage-regressions.R` | `test-5060-targeted-coverage-regressions.R` |
| `test-warm-start-feature-schema.R` | `test-0100-warm-start-feature-schema.R` |
| `test-warm-start-python.R` | `test-0101-warm-start-python.R` |
| `test-warm-start-features.R` | `test-0102-warm-start-features.R` |
| `test-warm-start-preprocess.R` | `test-0103-warm-start-preprocess.R` |
| `test-warm-start-model-core.R` | `test-0104-warm-start-model-core.R` |
| `test-warm-start-tuning.R` | `test-0105-warm-start-tuning.R` |
| `test-warm-start-calibration.R` | `test-0106-warm-start-calibration.R` |
| `test-warm-start-validation.R` | `test-0107-warm-start-validation.R` |
| `test-warm-start-cv.R` | `test-0108-warm-start-cv.R` |
| `test-warm-start-ensemble.R` | `test-0109-warm-start-ensemble.R` |
| `test-warm-start-prior.R` | `test-0110-warm-start-prior.R` |
| `test-warm-start-model-io.R` | `test-3100-warm-start-model-io.R` |
| `test-warm-start-model-registry.R` | `test-3101-warm-start-model-registry.R` |
| `test-warm-start-bundled-models.R` | `test-3102-warm-start-bundled-models.R` |
| `test-warm-start-adaptive.R` | `test-5100-warm-start-adaptive.R` |
| `test-btl-warm-start.R` | `test-6100-btl-warm-start.R` |
| `test-warm-start-bundle-workflow.R` | `test-9100-warm-start-bundle-workflow.R` |

Issue #279 reserves 5086--5089 for linking session identity/resume, common
reporting, Phase A imports, and rubric interoperability; verified unused on
2026-09-22. Tests remain deterministic and provider-free.

Issue #280 reserves 5090–5091 for estimator-neutral orchestration and selector
gates (verified unused on 2026-09-22). Tests remain provider-free; E3-MCMC
orchestration uses the synthetic sampler fixture, never real sampling.

Issue #274 reserves 5092 for legacy Phase B rejection, explicit estimator selection,
compatible Phase A reuse and removal of executable legacy branches (verified unused
on 2026-09-22). Test 5050 now exercises E1--E3 refits instead of skipped legacy modes.

Issue #281 reserves 5093–5095 for the shared release invariant matrix,
evidence single-use regressions, and deterministic known-offset simulations.
The real E3-MCMC release gate remains opt-in in 6021; synthetic sampler fixtures
validate plumbing only and are not numerical evidence for MCMC correctness.

Issue #281 coverage follow-up reserves 5096–5099 for release helper boundaries,
refit diagnostics, candidate handling, and recovery regressions. Tests keep all
Phase B selector gates intact; synthetic capacity snapshots test allocation
arithmetic only and do not establish selector validity.

Issue #290 reserves 0208 for verified standalone rubric-reference identities and
9205 for provider-free standalone-reference transport and serialization. Sampler
fixtures test interface contracts, not numerical MCMC correctness.

Issue #292 extends 0021, 0022 and 9205 with optional `posterior` draws-matrix
regressions that require no live sampler. From the package root, run the separate
synthetic CmdStan reference/CSV-recovery smoke explicitly with
`PAIRWISELLM_TEST_STANDALONE_STAN=true Rscript scripts/standalone-rubric-reference-stan-smoke.R`.

Issue #294 reserves 5110 for shared E2/E3 Gaussian prediction refinement, D020
scalar references, and old serialized E1/E2/E3 prediction compatibility.
