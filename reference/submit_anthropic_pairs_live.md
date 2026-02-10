# Live Anthropic (Claude) comparisons for a tibble of pairs

This is a robust row-wise wrapper around
[`anthropic_compare_pair_live`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_compare_pair_live.md).
It takes a tibble of pairs (ID1 / text1 / ID2 / text2), submits each
pair to the Anthropic Messages API, and collects the results.

## Usage

``` r
submit_anthropic_pairs_live(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  api_key = NULL,
  anthropic_version = "2023-06-01",
  reasoning = c("none", "enabled"),
  verbose = TRUE,
  status_every = 1,
  progress = TRUE,
  include_raw = FALSE,
  include_thoughts = NULL,
  save_path = NULL,
  parallel = FALSE,
  workers = 1,
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

  Anthropic model name (for example `"claude-sonnet-4-5"`,
  `"claude-haiku-4-5"`, or `"claude-opus-4-5"`).

- trait_name:

  Trait name to pass to `anthropic_compare_pair_live`.

- trait_description:

  Trait description to pass to `anthropic_compare_pair_live`.

- prompt_template:

  Prompt template string, typically from
  [`set_prompt_template`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).

- api_key:

  Optional Anthropic API key. Defaults to
  `Sys.getenv("ANTHROPIC_API_KEY")`.

- anthropic_version:

  Anthropic API version string passed as the `anthropic-version` HTTP
  header. Defaults to `"2023-06-01"`.

- reasoning:

  Character scalar passed to
  [`anthropic_compare_pair_live`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_compare_pair_live.md)
  (one of `"none"` or `"enabled"`).

- verbose:

  Logical; if `TRUE`, prints status, timing, and result summaries.

- status_every:

  Integer; print status / timing for every `status_every`-th pair.
  Defaults to 1 (every pair).

- progress:

  Logical; if `TRUE`, shows a textual progress bar.

- include_raw:

  Logical; if `TRUE`, each row of the returned tibble will include a
  `raw_response` list-column with the parsed JSON body from Anthropic.
  Note: Raw responses are not saved to the incremental CSV file.

- include_thoughts:

  Logical or `NULL`; forwarded to
  [`anthropic_compare_pair_live`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_compare_pair_live.md).
  When `TRUE` and `reasoning = "none"`, the underlying calls upgrade to
  extended thinking mode (`reasoning = "enabled"`), which implies
  `temperature = 1` and adds a `thinking` block. When `FALSE` or `NULL`,
  `reasoning` is used as-is.

- save_path:

  Character string; optional file path (e.g., "output.csv") to save
  results incrementally. If the file exists, the function reads it to
  identify and skip pairs that have already been processed (resume
  mode). Requires the `readr` package.

- parallel:

  Logical; if `TRUE`, enables parallel processing using `future.apply`.
  Requires the `future` and `future.apply` packages.

- workers:

  Integer; the number of parallel workers (threads) to use if
  `parallel = TRUE`. Defaults to 1. **Guidance:** Anthropic rate limits
  vary significantly by tier. Start conservatively (e.g., 2-4 workers)
  to avoid HTTP 429 errors.

- ...:

  Additional Anthropic parameters (for example `temperature`, `top_p`,
  `max_tokens`) passed on to
  [`anthropic_compare_pair_live`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_compare_pair_live.md).
  When `pair_uid` is supplied via `...`, it is used verbatim as
  `custom_id`.

## Value

A list containing three elements:

- results:

  A tibble with one row per successfully processed pair.

- failed_pairs:

  A tibble containing the rows from `pairs` that failed to process (due
  to API errors or timeouts), along with an `error_message` column.

- failed_attempts:

  A tibble of attempt-level failures (retries, timeouts, parse errors,
  invalid winners), separate from observed outcomes.

## Details

This function offers:

- **Parallel Processing:** Uses the `future` package to process multiple
  pairs simultaneously.

- **Incremental Saving:** Writes results to a CSV file as they complete.
  If the process is interrupted, re-running the function with the same
  `save_path` will automatically skip pairs that were already
  successfully processed.

- **Error Separation:** Returns valid results and failed pairs
  separately, making it easier to debug or retry specific failures.

**Temperature and reasoning behaviour**

Temperature and extended-thinking behaviour are controlled by
[`anthropic_compare_pair_live`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_compare_pair_live.md):

- When `reasoning = "none"` (no extended thinking), the default
  `temperature` is `0` (deterministic) unless you explicitly supply a
  different `temperature` via `...`.

- When `reasoning = "enabled"` (extended thinking), Anthropic requires
  `temperature = 1`. If you supply a different value, an error is raised
  by
  [`anthropic_compare_pair_live`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_compare_pair_live.md).

If you set `include_thoughts = TRUE` while `reasoning = "none"`, the
underlying calls upgrade to `reasoning = "enabled"`, which in turn
implies `temperature = 1` and adds a `thinking` block to the API
request. When `include_thoughts = FALSE` (the default), and you leave
`reasoning = "none"`, the effective default temperature is `0`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires ANTHROPIC_API_KEY and network access.

data("example_writing_samples", package = "pairwiseLLM")

pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 5, seed = 123) |>
  randomize_pair_order(seed = 456)

td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# 1. Sequential execution with incremental saving
res_claude <- submit_anthropic_pairs_live(
  pairs             = pairs,
  model             = "claude-sonnet-4-5",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  reasoning         = "none",
  save_path         = "results_seq.csv"
)

# 2. Parallel execution (faster)
res_par <- submit_anthropic_pairs_live(
  pairs             = pairs,
  model             = "claude-sonnet-4-5",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  save_path         = "results_par.csv",
  parallel          = TRUE,
  workers           = 4
)

# Inspect results
head(res_par$results)

# Check for failures
if (nrow(res_par$failed_pairs) > 0) {
  message("Some pairs failed:")
  print(res_par$failed_pairs)
}
} # }
```
