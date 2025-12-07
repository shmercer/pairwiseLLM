# Live Ollama comparisons for a tibble of pairs

`submit_ollama_pairs_live()` is a thin row-wise wrapper around
[`ollama_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/ollama_compare_pair_live.md).
It takes a tibble of pairs (`ID1` / `text1` / `ID2` / `text2`), submits
each pair to a local Ollama server, and binds the results into a single
tibble.

## Usage

``` r
submit_ollama_pairs_live(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  host = getOption("pairwiseLLM.ollama_host", "http://127.0.0.1:11434"),
  verbose = TRUE,
  status_every = 1,
  progress = TRUE,
  think = FALSE,
  num_ctx = 8192L,
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

  Ollama model name (for example `"mistral-small3.2:24b"`,
  `"qwen3:32b"`, `"gemma3:27b"`).

- trait_name:

  Trait name to pass to
  [`ollama_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/ollama_compare_pair_live.md).

- trait_description:

  Trait description to pass to
  [`ollama_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/ollama_compare_pair_live.md).

- prompt_template:

  Prompt template string, typically from
  [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).

- host:

  Base URL of the Ollama server. Defaults to the option
  `getOption("pairwiseLLM.ollama_host", "http://127.0.0.1:11434")`.

- verbose:

  Logical; if `TRUE`, prints status, timing, and result summaries.

- status_every:

  Integer; print status and timing for every `status_every`-th pair.
  Defaults to 1 (every pair). Errors are always printed.

- progress:

  Logical; if `TRUE`, shows a textual progress bar.

- think:

  Logical; see
  [`ollama_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/ollama_compare_pair_live.md)
  for behavior. When `TRUE` and the model name starts with `"qwen"`, the
  temperature is set to `0.6`; otherwise the temperature remains `0`.

- num_ctx:

  Integer; context window to use via `options$num_ctx`. The default is
  `8192L`.

- include_raw:

  Logical; if `TRUE`, each row of the returned tibble will include a
  `raw_response` list-column with the parsed JSON body from Ollama.

- ...:

  Reserved for future extensions and forwarded to
  [`ollama_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/ollama_compare_pair_live.md).

## Value

A tibble with one row per pair and the same columns as
[`ollama_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/ollama_compare_pair_live.md),
including an optional `raw_response` column when `include_raw = TRUE`.

## Details

This helper mirrors
[`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md)
but targets a local Ollama instance rather than a cloud API. It is
intended to offer a similar interface and return shape, so results can
be passed directly into
[`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md)
and
[`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md).

Temperature and context length are controlled as follows:

- By default, `temperature = 0` for all models.

- For Qwen models (model names beginning with `"qwen"`) and
  `think = TRUE`, `temperature` is set to `0.6`.

- The context window is set via `options$num_ctx`, which defaults to
  `8192` but may be overridden via the `num_ctx` argument.

In most user-facing workflows, it is more convenient to call
[`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md)
with `backend = "ollama"` rather than using `submit_ollama_pairs_live()`
directly. The backend-neutral wrapper will route arguments to the
appropriate backend helper and ensure a consistent return shape.

As with
[`ollama_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/ollama_compare_pair_live.md),
this function assumes that:

- An Ollama server is running and reachable at `host`.

- The requested models have been pulled in advance (for example
  `ollama pull mistral-small3.2:24b`).

## See also

- [`ollama_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/ollama_compare_pair_live.md)
  for single-pair Ollama comparisons.

- [`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md)
  for backend-agnostic comparisons over tibbles of pairs.

- [`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md),
  [`submit_anthropic_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_anthropic_pairs_live.md),
  and
  [`submit_gemini_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_gemini_pairs_live.md)
  for other backend-specific implementations.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires a running Ollama server and locally available models.
# This example will not be executed automatically during package checks.

library(pairwiseLLM)

data("example_writing_samples", package = "pairwiseLLM")

pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 5, seed = 123) |>
  randomize_pair_order(seed = 456)

td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# Live comparisons for multiple pairs using a Mistral model via Ollama.
# Make sure the model is available:
#   ollama pull mistral-small3.2:24b

res_mistral <- submit_ollama_pairs_live(
  pairs             = pairs,
  model             = "mistral-small3.2:24b",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  verbose           = TRUE,
  status_every      = 2,
  progress          = TRUE
)

res_mistral$better_id

# Qwen with thinking enabled: temperature is automatically set to 0.6.
# You can also override the context window via num_ctx.
#
#   ollama pull qwen3:32b

res_qwen_think <- submit_ollama_pairs_live(
  pairs             = pairs,
  model             = "qwen3:32b",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  think             = TRUE,
  num_ctx           = 16384,
  verbose           = FALSE,
  progress          = FALSE
)

res_qwen_think$better_id
} # }
```
