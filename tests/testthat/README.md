# Test Suite Conventions

## Naming & Block Ranges

Test files must follow the numeric prefix convention below to keep the suite
organized and execution order predictable.

New major subsystems should reserve a contiguous numeric block before tests
are added.

- 0000–0999: Core utilities, helpers, and pure functions
- 2000–2999: Live / interactive backends (e.g., human, API-based)
- 3000–3999: Batch workflows, resume logic, persistence
- 5000–5999: Adaptive pairing and ranking
  - State objects and invariants
  - Candidate generation and utility computation
  - Batch selection (exploration / exploitation / duplicates)
  - Stopping criteria and diagnostics
  - Output schemas (round logs, item summaries)
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
  - mock internal functions with `testthat::with_mocked_bindings(..., .env = asNamespace("pairwiseLLM"))`
  - mock imported functions with `testthat::with_mocked_bindings(..., .package = "<pkg>")` (e.g., `"httr2"`)

## Temp Files & Directories

- Use `withr::local_tempdir()` for temporary directories.
- Create temporary files under that directory to prevent cross-test collisions.
- Clean up only if needed; `withr` will handle most cleanup automatically.

## Network & API Keys

- Tests must never make network calls.
- Do not depend on API keys or real credentials.
- Use deterministic fixtures and mocked responses instead.
