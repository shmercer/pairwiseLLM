# Live Google Gemini comparisons for a tibble of pairs

This is a robust row-wise wrapper around
[`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md).
It takes a tibble of pairs (`ID1` / `text1` / `ID2` / `text2`), submits
each pair to the Google Gemini API, and collects the results.

## Usage

``` r
submit_gemini_pairs_live(
  pairs,
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
  verbose = TRUE,
  status_every = 1L,
  progress = TRUE,
  include_raw = FALSE,
  include_thoughts = FALSE,
  save_path = NULL,
  parallel = FALSE,
  workers = 1,
  ...
)
```

## Arguments

- pairs:

  Tibble/data frame with columns `ID1`, `text1`, `ID2`, `text2`.

- model:

  Gemini model name (e.g. `"gemini-3-pro-preview"`).

- trait_name:

  Trait name.

- trait_description:

  Trait description.

- prompt_template:

  Prompt template string, typically from
  [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).

- api_key:

  Optional Gemini API key.

- thinking_level:

  Default `"low"`; see
  [`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md).

- temperature:

  Optional numeric temperature; forwarded to
  [`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md).
  See Gemini docs; if `NULL` (default), the model uses its own default.

- top_p:

  Optional numeric; forwarded to
  [`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md).

- top_k:

  Optional numeric; forwarded to
  [`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md).

- max_output_tokens:

  Optional integer; forwarded to
  [`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md).

- api_version:

  API version; default `"v1beta"`.

- verbose:

  Logical; print status/timing every `status_every` pairs.

- status_every:

  Integer; how often to print status (default 1 = every pair).

- progress:

  Logical; show a text progress bar.

- include_raw:

  Logical; if `TRUE`, each row of the returned tibble will include a
  `raw_response` list-column with the parsed JSON body. Note: Raw
  responses are not saved to the incremental CSV file.

- include_thoughts:

  Logical; if `TRUE`, requests explicit reasoning output from Gemini and
  stores it in the `thoughts` column of the result, mirroring
  [`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md).

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
  `parallel = TRUE`. Defaults to 1. **Guidance:** Start conservatively
  (e.g., 2-4 workers) to avoid hitting HTTP 429 errors, as Gemini rate
  limits can be strict depending on your tier.

- ...:

  Reserved for future extensions; passed through to
  [`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md)
  (but `thinking_budget` is ignored there).

## Value

A list containing two elements:

- results:

  A tibble with one row per successfully processed pair.

- failed_pairs:

  A tibble containing the rows from `pairs` that failed to process (due
  to API errors or timeouts), along with an `error_message` column.

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

## Examples

``` r
# Requires:
# - GEMINI_API_KEY set in your environment
# - Internet access
# - Billable Gemini API usage
if (FALSE) { # \dontrun{
# Example pair data
pairs <- tibble::tibble(
  ID1   = c("S01", "S03"),
  text1 = c("Text 1", "Text 3"),
  ID2   = c("S02", "S04"),
  text2 = c("Text 2", "Text 4")
)

td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# 1. Sequential execution with incremental saving
res_seq <- submit_gemini_pairs_live(
  pairs             = pairs,
  model             = "gemini-3-pro-preview",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  save_path         = "results_gemini_seq.csv"
)

# 2. Parallel execution (faster)
res_par <- submit_gemini_pairs_live(
  pairs             = pairs,
  model             = "gemini-3-pro-preview",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  save_path         = "results_gemini_par.csv",
  parallel          = TRUE,
  workers           = 4
)

# Inspect results
head(res_par$results)
} # }
```
