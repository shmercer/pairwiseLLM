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

  Together.ai model name (for example `"deepseek-ai/DeepSeek-R1"`,
  `"moonshotai/Kimi-K2-Instruct-0905"`,
  `"Qwen/Qwen3-235B-A22B-Instruct-2507-tput"`,
  `"deepseek-ai/DeepSeek-V3"`).

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
  the JSON request body as top-level fields. If `temperature` is
  omitted, the function uses backend defaults (0.6 for
  `"deepseek-ai/DeepSeek-R1"`, 0 for all other models).

## Value

A tibble with one row and columns:

- custom_id:

  ID string of the form `"LIVE_<ID1>_vs_<ID2>"`.

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

  Internal reasoning text, for example `<think>...</think>` blocks from
  models like `"deepseek-ai/DeepSeek-R1"`.

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

For models such as `"deepseek-ai/DeepSeek-R1"` that emit internal
reasoning wrapped in `<think>...</think>` tags, this helper will:

- Extract the `<think>...</think>` block into the `thoughts` column.

- Remove the `<think>...</think>` block from the visible `content`
  column, so `content` contains only the user-facing answer.

Other Together.ai models (for example
`"moonshotai/Kimi-K2-Instruct-0905"`,
`"Qwen/Qwen3-235B-A22B-Instruct-2507-tput"`,
`"deepseek-ai/DeepSeek-V3"`) are supported via the same API but may not
use `<think>` tags; in those cases, `thoughts` will be `NA` and the full
model output will appear in `content`.

Temperature handling:

- If `temperature` is **not** supplied in `...`, the function applies
  backend defaults:

  - `"deepseek-ai/DeepSeek-R1"` → `temperature = 0.6`.

  - All other models → `temperature = 0`.

- If `temperature` is included in `...`, that value is used and the
  defaults are not applied.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires TOGETHER_API_KEY set in your environment and network access.

data("example_writing_samples", package = "pairwiseLLM")
samples <- example_writing_samples[1:2, ]

td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# Example: DeepSeek-R1 with default temperature = 0.6 if not supplied
res_deepseek <- together_compare_pair_live(
  ID1               = samples$ID[1],
  text1             = samples$text[1],
  ID2               = samples$ID[2],
  text2             = samples$text[2],
  model             = "deepseek-ai/DeepSeek-R1",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl
)

res_deepseek$better_id
res_deepseek$thoughts

# Example: Kimi-K2 with default temperature = 0 unless overridden
res_kimi <- together_compare_pair_live(
  ID1               = samples$ID[1],
  text1             = samples$text[1],
  ID2               = samples$ID[2],
  text2             = samples$text[2],
  model             = "moonshotai/Kimi-K2-Instruct-0905",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl
)

res_kimi$better_id
} # }
```
