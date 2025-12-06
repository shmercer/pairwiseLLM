# Live OpenAI comparisons for a tibble of pairs

This is a thin row-wise wrapper around
[`openai_compare_pair_live`](https://shmercer.github.io/pairwiseLLM/reference/openai_compare_pair_live.md).
It takes a tibble of pairs (ID1 / text1 / ID2 / text2), submits each
pair to the OpenAI API, and binds the results into a single tibble.

## Usage

``` r
submit_openai_pairs_live(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  endpoint = c("chat.completions", "responses"),
  api_key = Sys.getenv("OPENAI_API_KEY"),
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
  [`make_pairs`](https://shmercer.github.io/pairwiseLLM/reference/make_pairs.md),
  [`sample_pairs`](https://shmercer.github.io/pairwiseLLM/reference/sample_pairs.md),
  and
  [`randomize_pair_order`](https://shmercer.github.io/pairwiseLLM/reference/randomize_pair_order.md).

- model:

  OpenAI model name (for example "gpt-4.1", "gpt-5.1").

- trait_name:

  Trait name to pass to `openai_compare_pair_live`.

- trait_description:

  Trait description to pass to `openai_compare_pair_live`.

- prompt_template:

  Prompt template string, typically from
  [`set_prompt_template`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).

- endpoint:

  Which OpenAI endpoint to target. One of `"chat.completions"` or
  `"responses"`.

- api_key:

  Optional OpenAI API key.

- verbose:

  Logical; if TRUE, prints status, timing, and result summaries.

- status_every:

  Integer; print status / timing for every `status_every`-th pair.
  Defaults to 1 (every pair). Errors are always printed.

- progress:

  Logical; if TRUE, shows a textual progress bar.

- include_raw:

  Logical; if TRUE, each row of the returned tibble will include a
  `raw_response` list-column with the parsed JSON body from OpenAI.

- ...:

  Additional OpenAI parameters (temperature, top_p, logprobs, reasoning,
  and so on) passed on to `openai_compare_pair_live`.

## Value

A tibble with one row per pair and the same columns as
[`openai_compare_pair_live`](https://shmercer.github.io/pairwiseLLM/reference/openai_compare_pair_live.md),
including a `thoughts` column for reasoning summaries (when available).

## Details

The output has the same columns as
[`openai_compare_pair_live`](https://shmercer.github.io/pairwiseLLM/reference/openai_compare_pair_live.md),
with one row per pair, making it easy to pass into
[`build_bt_data`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md)
and
[`fit_bt_model`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md).

## Examples

``` r
if (FALSE) { # \dontrun{
library(pairwiseLLM)

data("example_writing_samples", package = "pairwiseLLM")

pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 5, seed = 123) |>
  randomize_pair_order(seed = 456)

td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# Live comparisons for multiple pairs
res_live <- submit_openai_pairs_live(
  pairs             = pairs,
  model             = "gpt-4.1",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  endpoint          = "chat.completions",
  temperature       = 0,
  verbose           = TRUE,
  status_every      = 2,
  progress          = TRUE,
  include_raw       = FALSE
)

res_live$better_id

# Using gpt-5.1 with reasoning = "low" on the responses endpoint
res_live_gpt5 <- submit_openai_pairs_live(
  pairs             = pairs,
  model             = "gpt-5.1",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  endpoint          = "responses",
  reasoning         = "low",
  temperature       = NULL,
  top_p             = NULL,
  logprobs          = NULL,
  verbose           = TRUE,
  status_every      = 3,
  progress          = TRUE,
  include_raw       = TRUE
)

str(res_live_gpt5$raw_response[[1]], max.level = 2)
} # }
```
