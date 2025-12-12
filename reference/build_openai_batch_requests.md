# Build OpenAI batch JSONL lines for paired comparisons

This helper constructs one JSON object per pair of writing samples,
suitable for use with the OpenAI batch API. It supports both
`/v1/chat/completions` and `/v1/responses` endpoints.

## Usage

``` r
build_openai_batch_requests(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  endpoint = c("chat.completions", "responses"),
  temperature = NULL,
  top_p = NULL,
  logprobs = NULL,
  reasoning = NULL,
  include_thoughts = FALSE,
  request_id_prefix = "EXP"
)
```

## Arguments

- pairs:

  A data frame or tibble with columns `ID1`, `text1`, `ID2`, and
  `text2`.

- model:

  Character scalar giving the OpenAI model name. Supports standard names
  (e.g. `"gpt-4.1"`) and date-stamped versions (e.g.
  `"gpt-5.2-2025-12-11"`).

- trait_name:

  Short label for the trait (e.g., "Overall Quality").

- trait_description:

  Full-text definition of the trait.

- prompt_template:

  Character template containing the placeholders `{TRAIT_NAME}`,
  `{TRAIT_DESCRIPTION}`, `{SAMPLE_1}`, and `{SAMPLE_2}`. Defaults to
  [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).

- endpoint:

  Which OpenAI endpoint to target. One of `"chat.completions"` (default)
  or `"responses"`.

- temperature:

  Optional temperature parameter. Defaults to `0` for standard models
  (deterministic). Must be `NULL` for reasoning models (enabled).

- top_p:

  Optional top_p parameter.

- logprobs:

  Optional logprobs parameter.

- reasoning:

  Optional reasoning effort for `gpt-5.1/5.2` when using the
  `/v1/responses` endpoint. Typically `"none"`, `"low"`, `"medium"`, or
  `"high"`.

- include_thoughts:

  Logical; if TRUE and using `responses` endpoint with reasoning,
  requests a summary. Defaults `reasoning` to `"low"` for gpt-5.1/5.2 if
  not specified.

- request_id_prefix:

  String prefix for `custom_id`; the full ID takes the form
  `"<prefix>_<ID1>_vs_<ID2>"`.

## Value

A tibble with one row per pair and columns:

- `custom_id`: ID string used by the batch API.

- `method`: HTTP method (`"POST"`).

- `url`: Endpoint path (`"/v1/chat/completions"` or `"/v1/responses"`).

- `body`: List column containing the request body.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires OPENAI_API_KEY and network access.
library(pairwiseLLM)

data("example_writing_samples", package = "pairwiseLLM")

pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 3, seed = 123) |>
  randomize_pair_order(seed = 456)

td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# 1. Basic chat.completions batch (no thoughts)
batch_tbl_chat <- build_openai_batch_requests(
  pairs             = pairs,
  model             = "gpt-4.1",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  endpoint          = "chat.completions",
  temperature       = 0
)

# 2. GPT-5.2-2025-12-11 Responses Batch with Reasoning
batch_resp <- build_openai_batch_requests(
  pairs = pairs,
  model = "gpt-5.2-2025-12-11",
  trait_name = td$name,
  trait_description = td$description,
  prompt_template = tmpl,
  endpoint = "responses",
  include_thoughts = TRUE, # implies reasoning="low" if not set
  reasoning = "medium"
)
batch_tbl_chat
batch_tbl_resp
} # }
```
