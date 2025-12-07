# Live Together.ai comparisons for a tibble of pairs

`submit_together_pairs_live()` is a thin row-wise wrapper around
[`together_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/together_compare_pair_live.md).
It takes a tibble of pairs (`ID1`, `text1`, `ID2`, `text2`), submits
each pair to the Together.ai Chat Completions API, and binds the results
into a single tibble.

## Usage

``` r
submit_together_pairs_live(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  api_key = NULL,
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
  [`make_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/make_pairs.md),
  [`sample_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/sample_pairs.md),
  and
  [`randomize_pair_order()`](https://shmercer.github.io/pairwiseLLM/reference/randomize_pair_order.md).

- model:

  Together.ai model name, for example `"deepseek-ai/DeepSeek-R1"`,
  `"moonshotai/Kimi-K2-Instruct-0905"`,
  `"Qwen/Qwen3-235B-A22B-Instruct-2507-tput"`,
  `"deepseek-ai/DeepSeek-V3"`.

- trait_name:

  Trait name to pass to
  [`together_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/together_compare_pair_live.md).

- trait_description:

  Trait description to pass to
  [`together_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/together_compare_pair_live.md).

- prompt_template:

  Prompt template string, typically from
  [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).

- api_key:

  Optional Together.ai API key. If `NULL` or empty, falls back to
  `TOGETHER_API_KEY` via
  [`.together_api_key()`](https://shmercer.github.io/pairwiseLLM/reference/dot-together_api_key.md).

- verbose:

  Logical; if `TRUE`, prints status, timing, and result summaries.

- status_every:

  Integer; print status / timing for every `status_every`-th pair.
  Defaults to 1 (every pair). Errors are always printed.

- progress:

  Logical; if `TRUE`, shows a textual progress bar.

- include_raw:

  Logical; if `TRUE`, each row of the returned tibble will include a
  `raw_response` list-column with the parsed JSON body from Together.ai.

- ...:

  Additional Together.ai parameters, such as `temperature`, `top_p`, or
  other provider-specific options. These are forwarded to
  [`together_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/together_compare_pair_live.md).

## Value

A tibble with one row per pair and the same columns as
[`together_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/together_compare_pair_live.md).

## Details

The output has the same columns as
[`together_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/together_compare_pair_live.md),
with one row per pair, making it easy to pass into
[`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md)
and
[`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires TOGETHER_API_KEY and network access. Running these examples will
# incur API usage costs.
library(pairwiseLLM)

data("example_writing_samples", package = "pairwiseLLM")

pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 5, seed = 123) |>
  randomize_pair_order(seed = 456)

td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# Live comparisons for multiple pairs using DeepSeek-R1
res_live <- submit_together_pairs_live(
  pairs             = pairs,
  model             = "deepseek-ai/DeepSeek-R1",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  temperature       = 0.6,
  verbose           = TRUE,
  status_every      = 2,
  progress          = TRUE,
  include_raw       = FALSE
)

res_live$better_id
} # }
```
