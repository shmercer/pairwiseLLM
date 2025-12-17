# Parse Gemini batch JSONL output into a tibble of pairwise results

This reads a JSONL file created by
[`gemini_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_download_batch_results.md)
and converts each line into a row that mirrors the structure used for
live Gemini calls, including a `thoughts` column when the batch was run
with `include_thoughts = TRUE`.

## Usage

``` r
parse_gemini_batch_output(results_path, requests_tbl)
```

## Arguments

- results_path:

  Path to the JSONL file produced by
  [`gemini_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_download_batch_results.md).

- requests_tbl:

  Tibble/data frame with at least columns `custom_id`, `ID1`, `ID2`, and
  (optionally) `request`. If a `request` list-column is present, it is
  used to detect whether `thinkingConfig.includeThoughts` was enabled
  for that pair.

## Value

A tibble with one row per request and columns:

- `custom_id`, `ID1`, `ID2`

- `model`, `object_type`, `status_code`, `result_type`, `error_message`

- `thoughts`, `thought_signature`, `thoughts_token_count`

- `content`, `better_sample`, `better_id`

- `prompt_tokens`, `completion_tokens`, `total_tokens`

## Examples

``` r
#' # This example assumes you have already:
# 1. Built Gemini batch requests with `build_gemini_batch_requests()`
# 2. Submitted and completed a batch job via the Gemini API
# 3. Downloaded the results using `gemini_download_batch_results()`
if (FALSE) { # \dontrun{
# Path to a JSONL file created by `gemini_download_batch_results()`
results_path <- "gemini_batch_results.jsonl"

# Requests table used to build the batch (must contain custom_id, ID1, ID2)
# as returned by `build_gemini_batch_requests()`
requests_tbl <- readRDS("gemini_batch_requests.rds")

# Parse batch output into a tidy tibble of pairwise results
results <- parse_gemini_batch_output(
  results_path = results_path,
  requests_tbl = requests_tbl
)

results
} # }
```
