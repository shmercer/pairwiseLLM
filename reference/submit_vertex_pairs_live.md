# Live Vertex AI Gemini comparisons for a tibble of pairs

This is a row-wise wrapper around
[`vertex_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/vertex_compare_pair_live.md).
It takes a tibble of pairs (`ID1` / `text1` / `ID2` / `text2`), submits
each pair to the Vertex AI Gemini API, and collects the results with
optional incremental saving and resume support.

## Usage

``` r
submit_vertex_pairs_live(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  api_key = NULL,
  temperature = NULL,
  top_p = NULL,
  top_k = NULL,
  max_output_tokens = NULL,
  thinking_level = NULL,
  thinking_budget = NULL,
  service_tier = "standard",
  api_version = "v1",
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

  Vertex Gemini model name (for example `"gemini-2.5-flash"`).

- trait_name:

  Trait name.

- trait_description:

  Trait description.

- prompt_template:

  Prompt template string, typically from
  [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).

- api_key:

  Optional Vertex API key.

- temperature:

  Optional numeric temperature; forwarded to
  [`vertex_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/vertex_compare_pair_live.md).

- top_p:

  Optional numeric; forwarded to
  [`vertex_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/vertex_compare_pair_live.md).

- top_k:

  Optional numeric; forwarded to
  [`vertex_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/vertex_compare_pair_live.md).

- max_output_tokens:

  Optional integer; forwarded to
  [`vertex_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/vertex_compare_pair_live.md).

- thinking_level:

  Optional Gemini 3 thinking level; forwarded to
  [`vertex_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/vertex_compare_pair_live.md).

- thinking_budget:

  Optional integer; forwarded to
  [`vertex_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/vertex_compare_pair_live.md).

- service_tier:

  Vertex AI service tier forwarded to
  [`vertex_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/vertex_compare_pair_live.md).

- api_version:

  API version; default `"v1"`.

- verbose:

  Logical; print status/timing every `status_every` pairs.

- status_every:

  Integer; how often to print status (default 1 = every pair).

- progress:

  Logical; show a text progress bar.

- include_raw:

  Logical; if `TRUE`, each row of the returned tibble will include a
  `raw_response` list-column with the parsed JSON body.

- include_thoughts:

  Logical; if `TRUE`, requests explicit reasoning output and stores it
  in the `thoughts` column of the result.

- save_path:

  Character string; optional file path to save results incrementally. If
  the file exists, the function reads it to identify and skip pairs that
  have already been processed (resume mode). Requires the `readr`
  package.

- parallel:

  Logical; if `TRUE`, enables parallel processing using `future.apply`.
  Requires the `future` and `future.apply` packages.

- workers:

  Integer; the number of parallel workers to use if `parallel = TRUE`.
  Defaults to 1.

- ...:

  Reserved for future extensions; passed through to
  [`vertex_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/vertex_compare_pair_live.md).

## Value

A list containing three elements:

- results:

  A tibble with one row per successfully processed pair.

- failed_pairs:

  A tibble containing the rows from `pairs` that failed to process,
  along with an `error_message` column.

- failed_attempts:

  A tibble of attempt-level failures separate from observed outcomes.

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
[`submit_gemini_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_gemini_pairs_live.md),
[`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md),
[`submit_ollama_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_ollama_pairs_live.md),
[`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md),
[`submit_together_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_together_pairs_live.md),
[`together_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/together_compare_pair_live.md),
[`vertex_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/vertex_compare_pair_live.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data("example_writing_samples", package = "pairwiseLLM")
pairs <- make_pairs(example_writing_samples[1:3, ])
td <- trait_description("overall_quality")
out <- submit_vertex_pairs_live(
  pairs = pairs,
  model = "gemini-3.8-flash",
  trait_name = td$name,
  trait_description = td$description,
  thinking_level = "low",
  parallel = FALSE
)
out$failed_pairs
} # }
```
