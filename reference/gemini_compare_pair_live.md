# Live Google Gemini comparison for a single pair of samples

This function sends a single pairwise comparison prompt to the Google
Gemini Generative Language API (Gemini 3 Pro) and parses the result into
a one-row tibble that mirrors the structure used for OpenAI / Anthropic
live calls.

## Usage

``` r
gemini_compare_pair_live(
  ID1,
  text1,
  ID2,
  text2,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  api_key = NULL,
  thinking_level = c("low", "medium", "high"),
  temperature = NULL,
  top_p = NULL,
  top_k = NULL,
  max_output_tokens = NULL,
  api_version = "v1beta",
  include_raw = FALSE,
  include_thoughts = FALSE,
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

  Gemini model identifier (for example `"gemini-3-pro-preview"`). The
  value is interpolated into the path
  `"/{api_version}/models/<model>:generateContent"`.

- trait_name:

  Short label for the trait (e.g. `"Overall Quality"`).

- trait_description:

  Full-text trait / rubric description.

- prompt_template:

  Prompt template string, typically from
  [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).
  The template should embed `<BETTER_SAMPLE>` tags.

- api_key:

  Optional Gemini API key (defaults to `Sys.getenv("GEMINI_API_KEY")`).

- thinking_level:

  One of `"low"`, `"medium"`, or `"high"`. This controls the maximum
  depth of internal reasoning for Gemini 3 Pro. For pairwise scoring,
  `"low"` is used by default to reduce latency and cost. Currently, the
  Gemini REST API only supports `"Low"` and `"High"` values; `"medium"`
  is mapped internally to `"High"` with a warning.

- temperature:

  Optional numeric temperature. If `NULL` (default), the parameter is
  omitted and Gemini uses its own default (currently 1.0).

- top_p:

  Optional nucleus sampling parameter. If `NULL`, omitted.

- top_k:

  Optional top-k sampling parameter. If `NULL`, omitted.

- max_output_tokens:

  Optional maximum output token count. If `NULL`, omitted.

- api_version:

  API version to use, default `"v1beta"`. For plain text pairwise
  comparisons v1beta is recommended.

- include_raw:

  Logical; if `TRUE`, the returned tibble includes a `raw_response`
  list-column with the parsed JSON body.

- include_thoughts:

  Logical; if `TRUE`, requests explicit reasoning output from Gemini via
  `generationConfig$thinkingConfig` and stores the first text part as
  `thoughts`, with subsequent parts collapsed into `content`. If `FALSE`
  (default), all text parts are collapsed into `content` and `thoughts`
  is `NA`.

- ...:

  Reserved for future extensions. Any `thinking_budget` entry in `...`
  is ignored (and a warning is emitted) because Gemini 3 does not allow
  `thinking_budget` and `thinking_level` to be used together.

## Value

A tibble with one row and columns:

- `custom_id` - `"LIVE_<ID1>_vs_<ID2>"`.

- `ID1`, `ID2` - provided sample IDs.

- `model` - model name returned by the API (or the requested model).

- `object_type` - `"generateContent"` on success, otherwise `NA`.

- `status_code` - HTTP status code (200 on success).

- `error_message` - error message for failures, otherwise `NA`.

- `thoughts` - explicit chain-of-thought style reasoning text if
  `include_thoughts = TRUE` and the model returns it; otherwise `NA`.

- `content` - concatenated text of the assistant's final answer (used to
  locate the `<BETTER_SAMPLE>` tag).

- `better_sample` - `"SAMPLE_1"`, `"SAMPLE_2"`, or `NA`.

- `better_id` - `ID1` if `SAMPLE_1` is chosen, `ID2` if `SAMPLE_2`, or
  `NA`.

- `prompt_tokens`, `completion_tokens`, `total_tokens` - usage counts if
  reported by the API, otherwise `NA_real_`.

## Details

It expects the prompt template to instruct the model to choose exactly
one of SAMPLE_1 or SAMPLE_2 and wrap the decision in \<BETTER_SAMPLE\>
tags, for example:

\<BETTER_SAMPLE\>SAMPLE_1\</BETTER_SAMPLE\>

or

\<BETTER_SAMPLE\>SAMPLE_2\</BETTER_SAMPLE\>

If `include_thoughts = TRUE`, the function additionally requests
Gemini's explicit chain-of-thought style reasoning (\\thoughts\\) via
the `thinkingConfig` block and stores it in a separate `thoughts`
column, while still using the final answer content to detect the
`<BETTER_SAMPLE>` tag.

## Examples

``` r
# Requires:
# - GEMINI_API_KEY set in your environment
# - Internet access
# - Billable Gemini API usage
if (FALSE) { # \dontrun{
td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

res <- gemini_compare_pair_live(
  ID1               = "S01",
  text1             = "Text 1",
  ID2               = "S02",
  text2             = "Text 2",
  model             = "gemini-3-pro-preview",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  thinking_level    = "low",
  include_thoughts  = FALSE,
  include_raw       = FALSE
)

res
res$better_id
} # }
```
