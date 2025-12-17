# pairwiseLLM 1.0.1

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

# pairwiseLLM 1.0.0

* Initial release.
* Unified live and batch LLM comparison framework (OpenAI / Anthropic / Gemini).
* Live support for Together.ai and local Ollama backends.
* Tools for Bradley–Terry and Elo models, positional bias checks
