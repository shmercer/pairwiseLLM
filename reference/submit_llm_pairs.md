# Backend-agnostic live comparisons for a tibble of pairs

`submit_llm_pairs()` is a backend-neutral wrapper around row-wise
comparison for multiple pairs. It takes a tibble of pairs (`ID1`,
`text1`, `ID2`, `text2`), submits each pair to the selected backend, and
binds the results into a single tibble.

## Usage

``` r
submit_llm_pairs(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  backend = c("openai", "anthropic", "gemini", "ollama"),
  endpoint = c("chat.completions", "responses"),
  api_key = NULL,
  verbose = TRUE,
  status_every = 1,
  progress = TRUE,
  include_raw = FALSE,
  ...
)
```

## Arguments

- pairs:

  Tibble or data frame with at least columns `ID1`, `text1`, `ID2`,
  `text2`. Typically created by
  [`make_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/make_pairs.md),
  [`sample_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/sample_pairs.md),
  and
  [`randomize_pair_order()`](https://shmercer.github.io/pairwiseLLM/reference/randomize_pair_order.md).

- model:

  Model identifier for the chosen backend. For `"openai"` this should be
  an OpenAI model name (for example `"gpt-4.1"`, `"gpt-5.1"`). For
  `"anthropic"` and `"gemini"`, use the corresponding provider model
  names. For `"ollama"`, use a local model name known to the Ollama
  server (for example `"mistral-small3.2:24b"`, `"qwen3:32b"`,
  `"gemma3:27b"`).

- trait_name:

  Trait name to pass through to the backend-specific comparison function
  (for example `"Overall Quality"`).

- trait_description:

  Full-text trait description passed to the backend.

- prompt_template:

  Prompt template string, typically from
  [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).

- backend:

  Character scalar indicating which LLM provider to use. One of
  `"openai"`, `"anthropic"`, `"gemini"`, or `"ollama"`.

- endpoint:

  Character scalar specifying which endpoint family to use for backends
  that support multiple live APIs. For the `"openai"` backend this must
  be one of `"chat.completions"` or `"responses"`, matching
  [`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md).
  For `"anthropic"`, `"gemini"`, and `"ollama"`, this is currently
  ignored.

- api_key:

  Optional API key for the selected backend. If `NULL`, the
  backend-specific helper will use its own default environment variable.
  For `"ollama"`, this argument is ignored (no API key is required for
  local inference).

- verbose:

  Logical; if `TRUE`, prints status, timing, and result summaries (for
  backends that support it).

- status_every:

  Integer; print status and timing for every `status_every`-th pair.
  Defaults to 1 (every pair). Errors are always printed.

- progress:

  Logical; if `TRUE`, shows a textual progress bar for backends that
  support it.

- include_raw:

  Logical; if `TRUE`, each row of the returned tibble will include a
  `raw_response` list-column with the parsed JSON body from the backend
  (for backends that support this).

- ...:

  Additional backend-specific parameters. For `"openai"` these are
  forwarded to
  [`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md)
  (and ultimately
  [`openai_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/openai_compare_pair_live.md))
  and typically include `temperature`, `top_p`, `logprobs`, `reasoning`,
  and `include_thoughts`. For `"anthropic"` and `"gemini"`, they are
  forwarded to
  [`submit_anthropic_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_anthropic_pairs_live.md)
  or
  [`submit_gemini_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_gemini_pairs_live.md)
  and may include options such as `max_output_tokens`,
  `include_thoughts`, and provider-specific controls. For `"ollama"`,
  arguments are forwarded to
  [`submit_ollama_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_ollama_pairs_live.md)
  and may include `host`, `think`, `num_ctx`, and other Ollama-specific
  options.

## Value

A tibble with one row per pair and the same columns as the underlying
backend-specific helper for the selected backend. All backends are
intended to return a compatible structure suitable for
[`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md)
and
[`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md).

## Details

At present, the following backends are implemented:

- `"openai"` →
  [`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md)

- `"anthropic"` →
  [`submit_anthropic_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_anthropic_pairs_live.md)

- `"gemini"` →
  [`submit_gemini_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_gemini_pairs_live.md)

- `"ollama"` →
  [`submit_ollama_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_ollama_pairs_live.md)

Each backend-specific helper returns a tibble with one row per pair and
a compatible set of columns, including a `thoughts` column (reasoning /
thinking text when available), `content` (visible assistant output),
`better_sample`, `better_id`, and token usage fields.

## See also

- [`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md),
  [`submit_anthropic_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_anthropic_pairs_live.md),
  [`submit_gemini_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_gemini_pairs_live.md),
  and
  [`submit_ollama_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_ollama_pairs_live.md)
  for backend-specific implementations.

- [`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md)
  for single-pair comparisons.

- [`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md)
  and
  [`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)
  for Bradley–Terry modelling of comparison results.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires an API key for the chosen cloud backend. For OpenAI, set
# OPENAI_API_KEY in your environment. Running these examples will incur
# API usage costs.
#
# For local Ollama use, an Ollama server must be running and the models
# must be pulled in advance. No API key is required for the `"ollama"`
# backend.

library(pairwiseLLM)

data("example_writing_samples", package = "pairwiseLLM")

pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 5, seed = 123) |>
  randomize_pair_order(seed = 456)

td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# Live comparisons for multiple pairs using the OpenAI backend
res_live <- submit_llm_pairs(
  pairs             = pairs,
  model             = "gpt-4.1",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  backend           = "openai",
  endpoint          = "chat.completions",
  temperature       = 0,
  verbose           = TRUE,
  status_every      = 2,
  progress          = TRUE,
  include_raw       = FALSE
)

res_live$better_id

# Live comparisons using a local Ollama backend (no API key required)
# Make sure an Ollama server is running and the model is available, e.g.:
#   ollama pull mistral-small3.2:24b
#
# res_ollama <- submit_llm_pairs(
#   pairs             = pairs,
#   model             = "mistral-small3.2:24b",
#   trait_name        = td$name,
#   trait_description = td$description,
#   prompt_template   = tmpl,
#   backend           = "ollama",
#   verbose           = TRUE,
#   status_every      = 2,
#   progress          = TRUE,
#   include_raw       = FALSE,
#   think             = FALSE,
#   num_ctx           = 8192
# )
#
# res_ollama$better_id
} # }
```
