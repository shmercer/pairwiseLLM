# Backend-agnostic live comparison for a single pair of samples

`llm_compare_pair()` is a thin wrapper around backend-specific
comparison functions. It currently supports the `"openai"`,
`"anthropic"`, and `"gemini"` backends and forwards the call to the
appropriate live comparison helper:

- `"openai"` →
  [`openai_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/openai_compare_pair_live.md)

- `"anthropic"` →
  [`anthropic_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_compare_pair_live.md)

- `"gemini"` →
  [`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md)

## Usage

``` r
llm_compare_pair(
  ID1,
  text1,
  ID2,
  text2,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  backend = c("openai", "anthropic", "gemini"),
  endpoint = c("chat.completions", "responses"),
  api_key = NULL,
  include_raw = FALSE,
  ...
)
```

## Arguments

- ID1:

  Character ID for the first sample.

- text1:

  Character string containing the first sample's text.

- ID2:

  Character ID for the second sample.

- text2:

  Character string containing the second sample's text.

- model:

  Model identifier for the chosen backend. For `"openai"` this should be
  an OpenAI model name (for example `"gpt-4.1"`, `"gpt-5.1"`). For
  `"anthropic"` and `"gemini"`, use the corresponding provider model
  names (for example `"claude-3-5-sonnet-latest"` or
  `"gemini-2.0-pro-exp"`).

- trait_name:

  Short label for the trait (for example `"Overall Quality"`).

- trait_description:

  Full-text definition of the trait.

- prompt_template:

  Prompt template string, typically from
  [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).

- backend:

  Character scalar indicating which LLM provider to use. One of
  `"openai"`, `"anthropic"`, or `"gemini"`.

- endpoint:

  Character scalar specifying which endpoint family to use for backends
  that support multiple live APIs. For the `"openai"` backend this must
  be one of `"chat.completions"` or `"responses"`, matching
  [`openai_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/openai_compare_pair_live.md).
  For `"anthropic"` and `"gemini"`, this argument is currently ignored.

- api_key:

  Optional API key for the selected backend. If `NULL`, the
  backend-specific helper will use its own default environment variable
  (for example `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, `GEMINI_API_KEY`).

- include_raw:

  Logical; if `TRUE`, the returned tibble includes a `raw_response`
  list-column with the parsed JSON body (or `NULL` on parse failure).
  Support for this may vary across backends.

- ...:

  Additional backend-specific parameters. For `"openai"` these are
  passed on to
  [`openai_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/openai_compare_pair_live.md)
  and typically include arguments such as `temperature`, `top_p`,
  `logprobs`, `reasoning`, and `include_thoughts`. For `"anthropic"` and
  `"gemini"` they are forwarded to the corresponding live helper and may
  include parameters such as `reasoning`, `include_thoughts`,
  `max_output_tokens`, or provider-specific options.

## Value

A tibble with one row and the same columns as the underlying
backend-specific live helper (for example
[`openai_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/openai_compare_pair_live.md)
for `"openai"`). All backends are intended to return a compatible
structure including `thoughts`, `content`, and token counts.

## Details

All backends are expected to return a tibble with a compatible
structure, including:

- `custom_id`, `ID1`, `ID2`

- `model`, `object_type`, `status_code`, `error_message`

- `thoughts` (reasoning / thinking text when available)

- `content` (visible assistant output)

- `better_sample`, `better_id`

- `prompt_tokens`, `completion_tokens`, `total_tokens`

For the `"openai"` backend, the `endpoint` argument controls whether the
Chat Completions API (`"chat.completions"`) or the Responses API
(`"responses"`) is used. For the `"anthropic"` and `"gemini"` backends,
`endpoint` is currently ignored and the default live API for that
provider is used.

## See also

- [`openai_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/openai_compare_pair_live.md),
  [`anthropic_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_compare_pair_live.md),
  and
  [`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md)
  for backend-specific implementations.

- [`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md)
  for row-wise comparisons over a tibble of pairs.

- [`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md)
  and
  [`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)
  for Bradley–Terry modelling of comparison results.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires an API key for the chosen backend. For OpenAI, set
# OPENAI_API_KEY in your environment. Running this example will incur
# API usage costs.

library(pairwiseLLM)

data("example_writing_samples", package = "pairwiseLLM")
samples <- example_writing_samples[1:2, ]

td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# Single live comparison using the OpenAI backend and chat.completions
res_live <- llm_compare_pair(
  ID1               = samples$ID[1],
  text1             = samples$text[1],
  ID2               = samples$ID[2],
  text2             = samples$text[2],
  model             = "gpt-4.1",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  backend           = "openai",
  endpoint          = "chat.completions",
  temperature       = 0
)

res_live$better_id

# Using the OpenAI responses endpoint with gpt-5.1 and reasoning = "low"
res_live_gpt5 <- llm_compare_pair(
  ID1               = samples$ID[1],
  text1             = samples$text[1],
  ID2               = samples$ID[2],
  text2             = samples$text[2],
  model             = "gpt-5.1",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  backend           = "openai",
  endpoint          = "responses",
  reasoning         = "low",
  include_thoughts  = TRUE,
  temperature       = NULL,
  top_p             = NULL,
  logprobs          = NULL,
  include_raw       = TRUE
)

str(res_live_gpt5$raw_response[[1]], max.level = 2)
} # }
```
