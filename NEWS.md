# pairwiseLLM 1.3.0

## New features
* Resumable orchestration runners: `bt_run_adaptive()`, `bt_run_core_linking()`, and
  `bt_run_adaptive_core_linking()` support checkpointing and resume via `checkpoint_dir`,
  `resume_from`, and `checkpoint_every`.
* Resuming is now supported even when `checkpoint_store_fits = FALSE`: on resume,
  the runners recompute the necessary model fits from saved results (including the
  baseline reference fit), keeping checkpoints small while still being safely resumable.
* Adaptive sampling workflow: round-based pair proposal + fit + metrics + stopping via
  `select_adaptive_pairs()`, `bt_adaptive_round()`, and `bt_run_adaptive()`, with preset
  stopping tiers (`bt_stop_metrics()`, `bt_should_stop()`).
* Core set + core-linking workflows: core set selection (`select_core_set()`), core-link pair
  selection (`select_core_link_pairs()`), per-round planning (`bt_core_link_round()`), and
  batch runners (`bt_run_core_linking()`, `bt_run_adaptive_core_linking()`).
* Multi-judge support in BT data/modeling: `build_bt_data()` can include an optional judge
  column; `fit_bt_model()` accepts 3- or 4-column BT data and can fit judge-aware models
  (SIRT engine), enabling judge/model diagnostics.
* Richer SIRT diagnostics returned from `fit_bt_model()` (e.g., separation, fit statistics),
  powering stopping and QA.

## Improvements
* Validation/reporting: standardized validation helpers and structured reports (with an option
  for strict enforcement) across submission + orchestration flows.
* Allocation hooks: added `allocation_compose()` to combine multiple `allocation_fun` policies
  in orchestration runners.
* Reverse audit tooling: optional post-stop reversal audit to quantify consistency when pairs
  are presented in reversed order (and support positional-bias QA).
* Embeddings integration: support for embeddings-backed workflows (including a reticulate-based
  path), while still accepting user-supplied embedding matrices.
* Judge QA: `judge_fit_summary()` now accepts `top_n = Inf`, and `judge_misfit_judges()` provides
  a simple way to extract misfit judge IDs from a fit.

## Maintenance
* Expanded and updated unit tests for new branches and helper utilities (targeting ≥95% coverage for
  new/changed code).

## Bug fixes
* When `core_ids` are supplied to `bt_run_core_linking()`, `core_method` is no longer strictly
  validated; this avoids premature `match.arg()` errors during resume/checkpoint workflows and
  supports a "fixed"/caller-supplied core.
* In `bt_run_adaptive_core_linking()`, linking metadata no longer coerces a NULL fit into a partial
  list; runs with unusable `initial_results` now fail fast with structured no-fit diagnostics.

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