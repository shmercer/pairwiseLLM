# pairwiseLLM 1.2.0

## New features

* Introduced `llm_submit_pairs_multi_batch()` and `llm_resume_multi_batches()` to
split large comparison sets across multiple batches and resume polling later.
These helpers support writing per‑batch and combined results, along with an
optional jobs registry.

## Testing

Added additional unit tests covering multi‑batch submission/resumption,
retry logic, registry updates, custom tag forwarding and file handling.

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

