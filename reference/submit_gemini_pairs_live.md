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
  thinking_level = "low",
  temperature = NULL,
  top_p = NULL,
  top_k = NULL,
  max_output_tokens = NULL,
  service_tier = "standard",
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

  Gemini model name (e.g. `"gemini-3.5-flash-lite"` or
  `"gemini-3-flash-preview"`).

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
  For Gemini 3 Flash models, `"minimal"` is also supported (e.g.,
  `thinking_level = "minimal"` with `model = "gemini-3-flash-preview"`).

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

- service_tier:

  Gemini Developer API service tier forwarded to
  [`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md).
  Use `"standard"` (default) or `NULL` for provider default behavior, or
  `"flex"` / `"priority"` to request the documented Gemini service tier.

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
  `parallel = TRUE`. Defaults to 1. **Guidance:** Use no more than 2
  workers to avoid HTTP 429 errors and respect shared check-farm
  resources.

- ...:

  Reserved for future extensions; passed through to
  [`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md)
  (but `thinking_budget` is ignored there).

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

## See also

[`check_llm_api_keys()`](https://shmercer.github.io/pairwiseLLM/reference/check_llm_api_keys.md),
[`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md)

Other live backends:
[`anthropic_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_compare_pair_live.md),
[`check_llm_api_keys()`](https://shmercer.github.io/pairwiseLLM/reference/check_llm_api_keys.md),
[`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md),
[`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md),
[`ollama_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/ollama_compare_pair_live.md),
[`openai_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/openai_compare_pair_live.md),
[`submit_anthropic_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_anthropic_pairs_live.md),
[`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md),
[`submit_ollama_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_ollama_pairs_live.md),
[`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md),
[`submit_together_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_together_pairs_live.md),
[`submit_vertex_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_vertex_pairs_live.md),
[`together_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/together_compare_pair_live.md),
[`vertex_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/vertex_compare_pair_live.md)

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
  model             = "gemini-3.5-flash-lite",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  save_path         = "results_gemini_seq.csv"
)

# 2. Parallel execution (faster)
res_par <- submit_gemini_pairs_live(
  pairs             = pairs,
  model             = "gemini-3.5-flash-lite",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  save_path         = "results_gemini_par.csv",
  parallel          = TRUE,
  workers           = 2
)

# 3. Gemini 3 Flash example (minimal thinking)
res_flash <- submit_gemini_pairs_live(
  pairs             = pairs,
  model             = "gemini-3-flash-preview",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  thinking_level    = "minimal",
  save_path         = "results_gemini_flash.csv"
)

# Inspect results
head(res_par$results)
} # }
```
