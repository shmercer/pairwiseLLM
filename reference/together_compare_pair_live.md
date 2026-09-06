# Live Together.ai comparison for a single pair of samples

`together_compare_pair_live()` sends a single pairwise comparison prompt
to the Together.ai Chat Completions API (`/v1/chat/completions`) and
parses the result into a small tibble. It is the Together.ai analogue of
[`openai_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/openai_compare_pair_live.md)
and uses the same prompt template and tag conventions (for example
`<BETTER_SAMPLE>...</BETTER_SAMPLE>`).

## Usage

``` r
together_compare_pair_live(
  ID1,
  text1,
  ID2,
  text2,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  tag_prefix = "<BETTER_SAMPLE>",
  tag_suffix = "</BETTER_SAMPLE>",
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

  Together.ai model name (for example the dated tested identifier
  `"deepseek-ai/DeepSeek-V4-Flash-0731"`). Check the provider's current
  serverless catalog before use.

- trait_name:

  Short label for the trait (for example "Overall Quality").

- trait_description:

  Full-text definition of the trait.

- prompt_template:

  Prompt template string, typically from
  [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).

- tag_prefix:

  Prefix for the better-sample tag. Defaults to `"<BETTER_SAMPLE>"`.

- tag_suffix:

  Suffix for the better-sample tag. Defaults to `"</BETTER_SAMPLE>"`.

- api_key:

  Optional Together.ai API key. If `NULL` or empty, the helper falls
  back to the `TOGETHER_API_KEY` environment variable via
  [`.together_api_key()`](https://shmercer.github.io/pairwiseLLM/reference/dot-together_api_key.md).

- include_raw:

  Logical; if `TRUE`, adds a list-column `raw_response` containing the
  parsed JSON body returned by Together.ai (or `NULL` on parse failure).
  This is useful for debugging parsing problems.

- ...:

  Additional Together.ai parameters, typically including `temperature`,
  `top_p`, and provider-specific options. These are passed through to
  the JSON request body as top-level fields. Omitted sampling controls
  use model/provider defaults. When `pair_uid` is supplied via `...`, it
  is used verbatim as `custom_id`.

## Value

A tibble with one row and columns:

- custom_id:

  Stable ID for the pair (`pair_uid` if supplied via `...`; otherwise
  `"LIVE_<ID1>_vs_<ID2>"`).

- ID1, ID2:

  The sample IDs you supplied.

- model:

  Model name reported by the API.

- object_type:

  API object type, typically `"chat.completion"`.

- status_code:

  HTTP-style status code (200 if successful).

- error_message:

  Error message if something goes wrong; otherwise `NA`.

- thoughts:

  Internal reasoning text from `<think>...</think>` blocks, when
  present.

- content:

  Concatenated visible assistant output (without `<think>` blocks).

- better_sample:

  "SAMPLE_1", "SAMPLE_2", or `NA`, based on the `<BETTER_SAMPLE>` tag.

- better_id:

  `ID1` if `"SAMPLE_1"` is chosen, `ID2` if `"SAMPLE_2"` is chosen,
  otherwise `NA`.

- prompt_tokens:

  Prompt / input token count (if reported).

- completion_tokens:

  Completion / output token count (if reported).

- total_tokens:

  Total token count (if reported).

- raw_response:

  (Optional) list-column containing the parsed JSON body.

## Details

For models that emit internal reasoning wrapped in `<think>...</think>`
tags, this helper will:

- Extract the `<think>...</think>` block into the `thoughts` column.

- Remove the `<think>...</think>` block from the visible `content`
  column, so `content` contains only the user-facing answer.

Models that do not use `<think>` tags return `NA` in `thoughts`, and
their full output appears in `content`. Model identifiers are forwarded
to the provider; see
[`vignette("model-compatibility")`](https://shmercer.github.io/pairwiseLLM/articles/model-compatibility.md)
for dated tested examples.

If `temperature` or `top_p` is omitted from `...`, the corresponding
field is not sent and the model/provider default applies. Explicit
values are passed through unchanged.

## See also

[`check_llm_api_keys()`](https://shmercer.github.io/pairwiseLLM/reference/check_llm_api_keys.md),
[`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md)

Other live backends:
[`anthropic_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_compare_pair_live.md),
[`check_llm_api_keys()`](https://shmercer.github.io/pairwiseLLM/reference/check_llm_api_keys.md),
[`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md),
[`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md),
[`ollama_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/ollama_compare_pair_live.md),
[`openai_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/openai_compare_pair_live.md),
[`submit_anthropic_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_anthropic_pairs_live.md),
[`submit_gemini_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_gemini_pairs_live.md),
[`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md),
[`submit_ollama_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_ollama_pairs_live.md),
[`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md),
[`submit_together_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_together_pairs_live.md),
[`submit_vertex_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_vertex_pairs_live.md),
[`vertex_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/vertex_compare_pair_live.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires TOGETHER_API_KEY set in your environment and network access.

data("example_writing_samples", package = "pairwiseLLM")
samples <- example_writing_samples[1:2, ]

td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# Dated tested Together serverless configuration
res_deepseek <- together_compare_pair_live(
  ID1               = samples$ID[1],
  text1             = samples$text[1],
  ID2               = samples$ID[2],
  text2             = samples$text[2],
  model             = "deepseek-ai/DeepSeek-V4-Flash-0731",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl
)

res_deepseek$better_id
res_deepseek$thoughts
} # }
```
