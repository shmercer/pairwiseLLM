# Test Suite Conventions

## Naming & Block Ranges

Tests use a 4-digit numeric prefix to keep ordering predictable and to clarify scope:

* `0000–0999`: utilities, parsing, schemas, and small helpers
* `2000–2999`: live backends (OpenAI, Anthropic, Gemini, Together, Ollama)
* `3000–3999`: batch + resume workflows
* `9000–9999`: integration-style tests (mocked E2E)

## Determinism Rules

* No randomness without explicit local seeding.
* Use `withr::local_seed()` whenever a test needs random numbers.
* Avoid relying on system time or ordering side effects.

## Mocking Rules

* All mocks must be created **inside** `test_that()` blocks.
* Use `with_mocked_bindings()` or `local_mocked_bindings()` to avoid leakage.
* Always restore options, env vars, and working directories via `withr::local_*()`.

## Temp Files & Directories

* Use `withr::local_tempdir()` for temporary directories.
* Create temp files under that directory to prevent cross-test collisions.
* Clean up only if needed; `withr` will handle most cleanup automatically.

## Network & API Keys

* Tests must never make network calls.
* Do not depend on API keys or real credentials.
* Use deterministic fixtures and mocked responses instead.
