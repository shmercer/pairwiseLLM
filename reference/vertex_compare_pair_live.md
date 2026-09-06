# Live Vertex AI Gemini comparison for a single pair of samples

This function sends a single pairwise comparison prompt to the Vertex AI
Gemini API using the express-mode REST `generateContent` endpoint and
parses the result into a one-row tibble that mirrors the structure used
by the other live backends.

## Usage

``` r
vertex_compare_pair_live(
  ID1,
  text1,
  ID2,
  text2,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  api_key = NULL,
  temperature = NULL,
  top_p = NULL,
  top_k = NULL,
  max_output_tokens = NULL,
  thinking_level = NULL,
  thinking_budget = NULL,
  service_tier = "standard",
  api_version = "v1",
  include_raw = FALSE,
  include_thoughts = FALSE,
  pair_uid = NULL,
  ...
)
```

## Arguments

- ID1:

  Character ID for the first sample.

- text1:

  Character containing the first sample text.

- ID2:

  Character ID for the second sample.

- text2:

  Character containing the second sample text.

- model:

  Vertex Gemini model identifier. You may supply either a short model
  name such as `"gemini-2.5-flash"` or the fully qualified publisher
  model resource `"publishers/google/models/gemini-2.5-flash"`.

- trait_name:

  Short label for the trait (e.g. `"Overall Quality"`).

- trait_description:

  Full-text trait / rubric description.

- prompt_template:

  Prompt template string, typically from
  [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).

- api_key:

  Optional Vertex API key (defaults to `Sys.getenv("VERTEX_API_KEY")`).

- temperature:

  Optional numeric temperature. If `NULL` (default), the parameter is
  omitted and Vertex uses the provider default.

- top_p:

  Optional nucleus sampling parameter. If `NULL`, omitted.

- top_k:

  Optional top-k sampling parameter. If `NULL`, omitted.

- max_output_tokens:

  Optional maximum output token count. If `NULL`, omitted.

- thinking_level:

  Optional Gemini 3 thinking level. Supported public values are
  `"minimal"`, `"low"`, `"medium"`, and `"high"`, but exact support
  varies by Gemini 3 model family. This parameter is only valid for
  Gemini 3 and later models. Do not supply it together with
  `thinking_budget`.

- thinking_budget:

  Optional thinking budget in tokens. If supplied, the request includes
  `generationConfig$thinkingConfig$thinkingBudget`. For models earlier
  than Gemini 3, this is the supported control surface. Do not supply it
  together with `thinking_level` on Gemini 3 models.

- service_tier:

  Vertex AI service tier. Use `"standard"` (default) or `NULL` for
  provider default behavior. Use `"flex"` to request the documented
  shared flex headers or `"priority"` to request the documented
  `dedicated` request-type header.

- api_version:

  API version to use, default `"v1"`.

- include_raw:

  Logical; if `TRUE`, the returned tibble includes a `raw_response`
  list-column with the parsed JSON body.

- include_thoughts:

  Logical; if `TRUE`, requests explicit reasoning output via
  `generationConfig$thinkingConfig$includeThoughts` and stores the first
  returned text part as `thoughts` when available.

- pair_uid:

  Optional stable per-pair identifier; when supplied, this value is used
  verbatim as `custom_id` (otherwise `custom_id` defaults to
  `"LIVE_<ID1>_vs_<ID2>"`).

- ...:

  Reserved for future extensions.

## Value

A tibble with one row and columns:

- `custom_id` - stable ID for the pair (`pair_uid` if supplied).

- `ID1`, `ID2` - provided sample IDs.

- `model` - model name returned by the API (or the requested model).

- `object_type` - `"generateContent"` on success, otherwise `NA`.

- `status_code` - HTTP status code (200 on success).

- `error_message` - error message for failures, otherwise `NA`.

- `thoughts` - explicit reasoning text if `include_thoughts = TRUE` and
  the model returns it; otherwise `NA`.

- `content` - concatenated text of the assistant's final answer (used to
  locate the `<BETTER_SAMPLE>` tag).

- `better_sample` - `"SAMPLE_1"`, `"SAMPLE_2"`, or `NA`.

- `better_id` - `ID1` if `SAMPLE_1` is chosen, `ID2` if `SAMPLE_2`, or
  `NA`.

- `prompt_tokens`, `completion_tokens`, `total_tokens` - usage counts if
  reported by the API, otherwise `NA_real_`.

## Details

The prompt template should instruct the model to choose exactly one of
SAMPLE_1 or SAMPLE_2 and wrap the decision in `<BETTER_SAMPLE>` tags.

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
[`together_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/together_compare_pair_live.md)

## Examples

``` r
if (FALSE) { # \dontrun{
td <- trait_description("overall_quality")
vertex_compare_pair_live(
  ID1 = "A", text1 = "First response.",
  ID2 = "B", text2 = "Second response.",
  model = "gemini-3.8-flash",
  trait_name = td$name,
  trait_description = td$description,
  thinking_level = "low"
)
} # }
```
