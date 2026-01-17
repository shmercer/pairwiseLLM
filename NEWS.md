# pairwiseLLM 1.3.0 (unreleased)

## Adaptive
* Adaptive ranking entrypoints now validate v3 config/state contracts and may
  error earlier for invalid inputs.

# pairwiseLLM 1.2.0

## New Features
*   Parallel Processing: 
    - `submit_llm_pairs()` and backend-specific live functions (OpenAI, Anthropic, Gemini, Together, Ollama) now support parallel execution via `parallel = TRUE` and `workers = n` (requires the `{future}` package).
*   Incremental Saving & Resume: 
    - Added `save_path` argument to live submission functions. Results are saved to CSV incrementally, allowing interrupted jobs to resume automatically by skipping previously processed pairs.
*   Robust Error Handling: 
    - Failed API calls no longer stop the entire process. Failures are captured and returned separately, allowing for easier inspection and re-submission.
*   Added `estimate_llm_pairs_cost()` to estimate costs in live and batch mode.
*   Introduced `llm_submit_pairs_multi_batch()` and `llm_resume_multi_batches()` to split large comparison sets across multiple batches and resume polling later.
    These helpers support writing per‑batch and combined results, along with an optional jobs registry.

## Bug fixes
*   The prompt format for anthropic batch comparisons now match the anthropic live format.
*   Reverse consistency functions can now handle duplicate pairs.

## Breaking Changes
*   `submit_llm_pairs()` and its backend-specific counterparts now return a **list** containing two elements: `$results` (a tibble of successful comparisons) and `$failed_pairs` (a tibble of inputs that failed). Previous versions returned a single tibble.

# pairwiseLLM 1.1.0

## Models
* Added GPT-5.2
* Ensured models can be called with date format, e.g. `gpt-5.2-2025-12-11`
* Default temperature setting is set to 0 for non-reasoning models, provider 
  default for reasoning models (typically 1)

## Tests
* Tests added to improve coverage

## Documentation
* Changed pkgdown site layout
* Added codemeta.json
* Added repo logo
* Updated function examples
* Add references to Description

## Miscellaneous
* No longer set global variables, now done in individual functions
* Added `verbose` option in `fit_bt_model()` and `summarize_bt_fit()`
* Moved null coalescing helper to separate R file
* Changed validation of API keys in multiple functions

# pairwiseLLM 1.0.0

* Initial release.
* Unified live and batch LLM comparison framework (OpenAI / Anthropic / Gemini).
* Live support for Together.ai and local Ollama backends.
* Tools for Bradley–Terry and Elo models, positional bias checks

