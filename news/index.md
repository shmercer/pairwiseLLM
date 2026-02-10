# Changelog

## pairwiseLLM 1.3.0

### New Features

#### Adaptive pairing & ranking framework

- Introduced a full adaptive pairing / adaptive ranking framework
  designed to efficiently rank large sets of writing samples using
  uncertainty-aware pair selection and Bayesian inference.

- Added
  [`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md),
  the primary user-facing wrapper that runs the complete adaptive
  workflow end-to-end, including warm start, adaptive pairing rounds,
  Bayesian BTL refits, diagnostics, and stopping.

- Advanced control is available via:

  - [`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md)
    — initialize an adaptive run and state
  - [`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md)
    — execute live adaptive comparisons
  - [`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md)
    — resume interrupted or long-running runs  
    These functions are intended for custom orchestration and
    fault-tolerant execution.

- Adaptive pairing is organized into rounds that balance global scale
  identification and local refinement using a mixture of anchor,
  long-range, mid-range, and local comparisons.

- The adaptive controller tracks a global identifiability state based on
  Bayesian diagnostics and agreement between online (TrueSkill) and
  global (BTL) rankings. Once the global scale is identified:

  - long-range comparisons are automatically tapered,
  - comparison budget is reallocated toward local and boundary-refining
    pairs,
  - exploration rates are reduced to focus on decision-relevant
    uncertainty.

- Long-range comparisons are additionally posterior-gated in later
  stages, preventing wasted comparisons on pairs that are already
  decisively ordered.

- Late-stage local pairing prioritizes near-tie pairs, with limited,
  auditable overrides to degree caps when especially informative
  comparisons are blocked.

- Adaptive runs produce fully auditable step-, round-, and refit-level
  logs, recording candidate generation, fallbacks, gating decisions,
  quota reallocations, and stopping criteria.

- All adaptive workflows use standardized configuration, state, and
  logging contracts to ensure reproducibility and future extensibility.

#### Bayesian Bradley–Terry–Luce (BTL) modeling

- Added a fully Bayesian Bradley–Terry–Luce (BTL) model implemented via
  CmdStan, providing posterior uncertainty estimates for item skill
  parameters.
- New entrypoint
  [`fit_bayes_btl_mcmc()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bayes_btl_mcmc.md)
  enables direct posterior inference from pairwise comparison data,
  independent of or integrated with adaptive workflows.
- Supports multiple model variants (including error and positional bias
  extensions) and optional refitting on increasing subsets of
  comparisons.
- Bayesian BTL outputs integrate seamlessly with adaptive ranking
  utilities
  ([`summarize_items()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_items.md),
  [`summarize_refits()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_refits.md)),
  serving as the statistical backbone for adaptive pairing decisions.

#### Model support & live API improvements

- Added support for the Gemini Flash model `gemini-3-flash-preview` for
  live pairwise comparisons.
- Added support for OpenAI service tiers / priority routing via
  `service_tier` for applicable live models.
  - Enables tiers such as `"flex"` and `"priority"` when supported by
    the selected model.
  - Integrated into the live submission path without requiring changes
    to calling code.

### Documentation

- Expanded and clarified documentation for adaptive ranking, Bayesian
  BTL, and live model configuration.
- Updated examples to reflect new adaptive and Bayesian APIs.

## pairwiseLLM 1.2.0

### New Features

- Parallel Processing:
  - [`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md)
    and backend-specific live functions (OpenAI, Anthropic, Gemini,
    Together, Ollama) now support parallel execution via
    `parallel = TRUE` and `workers = n` (requires the
    [future](https://future.futureverse.org) package).
- Incremental Saving & Resume:
  - Added `save_path` argument to live submission functions. Results are
    saved to CSV incrementally, allowing interrupted jobs to resume
    automatically by skipping previously processed pairs.
- Robust Error Handling:
  - Failed API calls no longer stop the entire process. Failures are
    captured and returned separately, allowing for easier inspection and
    re-submission.
- Added
  [`estimate_llm_pairs_cost()`](https://shmercer.github.io/pairwiseLLM/reference/estimate_llm_pairs_cost.md)
  to estimate costs in live and batch mode.
- Introduced
  [`llm_submit_pairs_multi_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_multi_batch.md)
  and
  [`llm_resume_multi_batches()`](https://shmercer.github.io/pairwiseLLM/reference/llm_resume_multi_batches.md)
  to split large comparison sets across multiple batches and resume
  polling later. These helpers support writing per‑batch and combined
  results, along with an optional jobs registry.

### Bug fixes

- The prompt format for anthropic batch comparisons now match the
  anthropic live format.
- Reverse consistency functions can now handle duplicate pairs.

### Breaking Changes

- [`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md)
  and its backend-specific counterparts now return a **list** containing
  two elements: `$results` (a tibble of successful comparisons) and
  `$failed_pairs` (a tibble of inputs that failed). Previous versions
  returned a single tibble.

## pairwiseLLM 1.1.0

CRAN release: 2025-12-22

### Models

- Added GPT-5.2
- Ensured models can be called with date format,
  e.g. `gpt-5.2-2025-12-11`
- Default temperature setting is set to 0 for non-reasoning models,
  provider default for reasoning models (typically 1)

### Tests

- Tests added to improve coverage

### Documentation

- Changed pkgdown site layout
- Added codemeta.json
- Added repo logo
- Updated function examples
- Add references to Description

### Miscellaneous

- No longer set global variables, now done in individual functions
- Added `verbose` option in
  [`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)
  and
  [`summarize_bt_fit()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_bt_fit.md)
- Moved null coalescing helper to separate R file
- Changed validation of API keys in multiple functions

## pairwiseLLM 1.0.0

- Initial release.
- Unified live and batch LLM comparison framework (OpenAI / Anthropic /
  Gemini).
- Live support for Together.ai and local Ollama backends.
- Tools for Bradley–Terry and Elo models, positional bias checks
