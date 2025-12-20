# Live Together.ai comparisons for a tibble of pairs

`submit_together_pairs_live()` is a robust row-wise wrapper around
[`together_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/together_compare_pair_live.md).
It takes a tibble of pairs (`ID1`, `text1`, `ID2`, `text2`), submits
each pair to the Together.ai Chat Completions API, and collects the
results.

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
  Defaults to 1 (every pair).

- progress:

  Logical; if `TRUE`, shows a textual progress bar.

- include_raw:

  Logical; if `TRUE`, each row of the returned tibble will include a
  `raw_response` list-column with the parsed JSON body from Together.ai.
  Note: Raw responses are not saved to the incremental CSV file.

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
  `parallel = TRUE`. Defaults to 1. **Guidance:** Together.ai rate
  limits vary by usage tier. Start with 4 to 8 workers to avoid hitting
  HTTP 429 errors.

- ...:

  Additional Together.ai parameters, such as `temperature`, `top_p`, or
  other provider-specific options. These are forwarded to
  [`together_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/together_compare_pair_live.md).

## Value

A list containing two elements:

- results:

  A tibble with one row per successfully processed pair and columns such
  as `better_id`, `better_sample`, `thoughts`, and `content`.

- failed_pairs:

  A tibble containing the rows from `pairs` that failed to process (due
  to API errors or timeouts), along with an `error_message` column.
  These can be easily re-submitted.

## Details

This function improves upon simple looping by offering:

- **Parallel Processing:** Uses the `future` package to process multiple
  pairs simultaneously.

- **Incremental Saving:** Writes results to a CSV file as they complete.
  If the process is interrupted, re-running the function with the same
  `save_path` will automatically skip pairs that were already
  successfully processed.

- **Error Separation:** Returns valid results and failed pairs
  separately, making it easier to debug or retry specific failures.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires TOGETHER_API_KEY and network access.

data("example_writing_samples", package = "pairwiseLLM")

pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 10, seed = 123) |>
  randomize_pair_order(seed = 456)

td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# 1. Sequential execution with incremental saving
# If interrupted, running this again will resume progress.
res_seq <- submit_together_pairs_live(
  pairs             = pairs,
  model             = "deepseek-ai/DeepSeek-R1",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  save_path         = "results_seq.csv"
)

# 2. Parallel execution (faster)
# Note: On Windows, this opens background R sessions.
res_par <- submit_together_pairs_live(
  pairs             = pairs,
  model             = "deepseek-ai/DeepSeek-R1",
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
