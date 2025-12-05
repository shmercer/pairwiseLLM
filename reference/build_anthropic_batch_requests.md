# Build Anthropic Message Batch requests from a tibble of pairs

This helper converts a tibble of writing pairs into a list of Anthropic
*Message Batch* requests. Each request has a unique `custom_id` of the
form `"ANTH_<ID1>_vs_<ID2>"` and a `params` object compatible with the
`/v1/messages` API.

## Usage

``` r
build_anthropic_batch_requests(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  reasoning = c("none", "enabled"),
  custom_id_prefix = "ANTH",
  ...
)
```

## Arguments

- pairs:

  Tibble or data frame with at least columns `ID1`, `text1`, `ID2`,
  `text2`. Typically created by
  [`make_pairs`](https://shmercer.github.io/pairwiseLLM/reference/make_pairs.md),
  [`sample_pairs`](https://shmercer.github.io/pairwiseLLM/reference/sample_pairs.md),
  and
  [`randomize_pair_order`](https://shmercer.github.io/pairwiseLLM/reference/randomize_pair_order.md).

- model:

  Anthropic Claude model name, for example `"claude-sonnet-4-5"`,
  `"claude-haiku-4-5"`, or `"claude-opus-4-5"`.

- trait_name:

  Short label for the trait (for example "Overall Quality").

- trait_description:

  Full-text description of the trait or rubric.

- prompt_template:

  Prompt template string, typically from
  [`set_prompt_template`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).
  The template should embed your full instructions, rubric text, and
  `<BETTER_SAMPLE>` tagging convention.

- reasoning:

  Character scalar indicating whether to allow extended thinking; one of
  `"none"` or `"enabled"`. See details above.

- custom_id_prefix:

  Prefix for the `custom_id` field. Defaults to `"ANTH"` so that IDs
  take the form `"ANTH_<ID1>_vs_<ID2>"`.

- ...:

  Additional Anthropic parameters such as `max_tokens`, `temperature`,
  `top_p`, or `thinking_budget_tokens`, which will be passed through to
  the Messages API.

## Value

A tibble with one row per pair and two main columns:

- custom_id:

  Character ID of the form `"<PREFIX>_<ID1>_vs_<ID2>"`.

- params:

  List-column containing the Anthropic Messages API `params` object for
  each request, ready to be used in the `requests` array of
  `/v1/messages/batches`.

## Details

The function mirrors the behaviour of
[`build_openai_batch_requests`](https://shmercer.github.io/pairwiseLLM/reference/build_openai_batch_requests.md)
but targets Anthropic's `/v1/messages/batches` endpoint. It applies the
same recommended defaults and reasoning constraints as
[`anthropic_compare_pair_live`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_compare_pair_live.md):

- `reasoning = "none"`:

  - Default `temperature = 0` (deterministic behaviour), unless you
    explicitly supply a different `temperature` via `...`.

  - Default `max_tokens = 768`, unless overridden via `max_tokens` in
    `...`.

- `reasoning = "enabled"` (extended thinking):

  - `temperature` **must** be 1. If you supply a different value in
    `...`, this function throws an error.

  - Defaults to `max_tokens = 2048` and `thinking_budget_tokens = 1024`,
    with the constraint `1024 <= thinking_budget_tokens < max_tokens`.
    Violations of this constraint produce an error.

As a result, when you build batches without extended thinking
(`reasoning = "none"`), the effective default temperature is 0. When you
opt into extended thinking (`reasoning = "enabled"`), Anthropic's
requirement of `temperature = 1` is enforced for all batch requests.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires ANTHROPIC_API_KEY and network access.
library(pairwiseLLM)

data("example_writing_samples", package = "pairwiseLLM")

pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 3, seed = 123) |>
  randomize_pair_order(seed = 456)

td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# Standard batch requests without extended thinking
reqs_none <- build_anthropic_batch_requests(
  pairs             = pairs,
  model             = "claude-sonnet-4-5",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  reasoning         = "none"
)

reqs_none

# Batch requests with extended thinking (temperature forced to 1)
reqs_reason <- build_anthropic_batch_requests(
  pairs             = pairs,
  model             = "claude-sonnet-4-5",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  reasoning         = "enabled"
)

reqs_reason
} # }
```
