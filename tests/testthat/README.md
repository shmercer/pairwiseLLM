# Test Suite Conventions

## Naming & Block Ranges

Test files must follow the numeric prefix convention below to keep the suite
organized and execution order predictable.

New major subsystems should reserve a contiguous numeric block before tests
are added.

- 0000–0999: Core utilities, helpers, and pure functions
- 2000–2999: Live / interactive backends (e.g., human, API-based)
- 3000–3999: Batch workflows, resume logic, persistence
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
