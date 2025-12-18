# Live Anthropic (Claude) comparison for a single pair of samples

This function sends a single pairwise comparison prompt to the Anthropic
Messages API (Claude models) and parses the result into a small tibble.

## Usage

``` r
anthropic_compare_pair_live(
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
  anthropic_version = "2023-06-01",
  reasoning = c("none", "enabled"),
  include_raw = FALSE,
  include_thoughts = NULL,
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

  Anthropic Claude model name (for example `"claude-sonnet-4-5"`,
  `"claude-haiku-4-5"`, or `"claude-opus-4-5"`).

- trait_name:

  Short label for the trait (for example "Overall Quality").

- trait_description:

  Full-text definition of the trait.

- prompt_template:

  Prompt template string, typically from
  [`set_prompt_template`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).
  The template should embed the full instructions, rubric text, and
  `<BETTER_SAMPLE>` tagging convention.

- tag_prefix:

  Prefix for the better-sample tag. Defaults to `"<BETTER_SAMPLE>"`.

- tag_suffix:

  Suffix for the better-sample tag. Defaults to `"</BETTER_SAMPLE>"`.

- api_key:

  Optional Anthropic API key. Defaults to
  `Sys.getenv("ANTHROPIC_API_KEY")`.

- anthropic_version:

  Anthropic API version string passed as the `anthropic-version` HTTP
  header. Defaults to `"2023-06-01"`.

- reasoning:

  Character scalar indicating whether to allow more extensive internal
  "thinking" before the visible answer. Two values are recognised:

  - `"none"` – standard prompting (recommended default).

  - `"enabled"` – uses Anthropic's extended thinking mode by sending a
    `thinking` block with a token budget; this also changes the default
    `max_tokens` and constrains `temperature`.

- include_raw:

  Logical; if `TRUE`, adds a list-column `raw_response` containing the
  parsed JSON body returned by Anthropic (or `NULL` on parse failure).
  This is useful for debugging parsing problems.

- include_thoughts:

  Logical or `NULL`. When `TRUE` and `reasoning = "none"`, this function
  upgrades to extended thinking mode by setting `reasoning = "enabled"`
  before constructing the request, which in turn implies
  `temperature = 1` and adds a `thinking` block. When `FALSE` and
  `reasoning = "enabled"`, a warning is issued but extended thinking is
  still used. When `NULL` (the default), `reasoning` is used as-is.

- ...:

  Additional Anthropic parameters such as `max_tokens`, `temperature`,
  `top_p` or a custom `thinking_budget_tokens`, which will be passed
  through to the Messages API.

  When `reasoning = "none"` the defaults are:

  - `temperature = 0` (deterministic behaviour) unless you supply
    `temperature` explicitly.

  - `max_tokens = 768` unless you supply `max_tokens`.

  When `reasoning = "enabled"` (extended thinking), the Anthropic API
  imposes additional constraints:

  - `temperature` **must** be 1. If you supply a different value, this
    function will throw an error.

  - `thinking_budget_tokens` must satisfy
    `thinking_budget_tokens >= 1024` and
    `thinking_budget_tokens < max_tokens`. If you supply a value that
    violates these constraints, this function will throw an error.

  - By default, `max_tokens = 2048` and `thinking_budget_tokens = 1024`.

## Value

A tibble with one row and columns:

- custom_id:

  ID string of the form `"LIVE_<ID1>_vs_<ID2>"`.

- ID1, ID2:

  The sample IDs you supplied.

- model:

  Model name reported by the API.

- object_type:

  Anthropic object type (for example `"message"`).

- status_code:

  HTTP-style status code (200 if successful).

- error_message:

  Error message if something goes wrong; otherwise NA.

- thoughts:

  Summarised thinking / reasoning text when `reasoning = "enabled"` and
  the API returns thinking blocks; otherwise `NA`.

- content:

  Concatenated text from the assistant output (excluding thinking
  blocks).

- better_sample:

  "SAMPLE_1", "SAMPLE_2", or NA.

- better_id:

  ID1 if SAMPLE_1 is chosen, ID2 if SAMPLE_2 is chosen, otherwise NA.

- prompt_tokens:

  Prompt / input token count (if reported).

- completion_tokens:

  Completion / output token count (if reported).

- total_tokens:

  Total token count (reported by the API or computed as input + output
  tokens when not provided).

- raw_response:

  (Optional) list-column containing the parsed JSON body.

## Details

It mirrors the behaviour and output schema of
[`openai_compare_pair_live`](https://shmercer.github.io/pairwiseLLM/reference/openai_compare_pair_live.md),
but targets Anthropic's `/v1/messages` endpoint. The prompt template,
`<BETTER_SAMPLE>` tag convention, and downstream parsing / BT modelling
can remain unchanged.

The function is designed to work with Claude models such as Sonnet,
Haiku, and Opus in the "4.5" family. You can pass any valid Anthropic
model string, for example:

- `"claude-sonnet-4-5"`

- `"claude-haiku-4-5"`

- `"claude-opus-4-5"`

The API typically responds with a dated model string such as
`"claude-sonnet-4-5-20250929"` in the `model` field.

**Recommended defaults for pairwise writing comparisons**

For stable, reproducible comparisons we recommend:

- `reasoning = "none"` with `temperature = 0` and `max_tokens = 768` for
  standard pairwise scoring.

- `reasoning = "enabled"` when you explicitly want extended thinking; in
  this mode Anthropic requires `temperature = 1`. The default in this
  function is `max_tokens = 2048` and `thinking_budget_tokens = 1024`,
  which satisfies the documented constraints
  `thinking_budget_tokens >= 1024` and
  `thinking_budget_tokens < max_tokens`.

When `reasoning = "enabled"`, this function also sends a `thinking`
block to the Anthropic API:

    "thinking": {
      "type": "enabled",
      "budget_tokens": <thinking_budget_tokens>
    }

Setting `include_thoughts = TRUE` when `reasoning = "none"` is a
convenient way to opt into Anthropic's extended thinking mode without
changing the `reasoning` argument explicitly. In that case, `reasoning`
is upgraded to `"enabled"`, the default `temperature` becomes 1, and a
`thinking` block is included in the request. When `reasoning = "none"`
and `include_thoughts` is `FALSE` or `NULL`, the default temperature
remains 0 unless you explicitly override it.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires ANTHROPIC_API_KEY and network access.
library(pairwiseLLM)

data("example_writing_samples", package = "pairwiseLLM")
samples <- example_writing_samples[1:2, ]

td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# Short, deterministic comparison with no explicit thinking block
res_claude <- anthropic_compare_pair_live(
  ID1               = samples$ID[1],
  text1             = samples$text[1],
  ID2               = samples$ID[2],
  text2             = samples$text[2],
  model             = "claude-sonnet-4-5",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  reasoning         = "none",
  include_raw       = FALSE
)

res_claude$better_id

# Allow more internal thinking and a longer explanation
res_claude_reason <- anthropic_compare_pair_live(
  ID1               = samples$ID[1],
  text1             = samples$text[1],
  ID2               = samples$ID[2],
  text2             = samples$text[2],
  model             = "claude-sonnet-4-5",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  reasoning         = "enabled",
  include_raw       = TRUE,
  include_thoughts  = TRUE
)

res_claude_reason$total_tokens
substr(res_claude_reason$content, 1, 200)
} # }
```
