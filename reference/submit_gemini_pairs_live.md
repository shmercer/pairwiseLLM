# Live Google Gemini comparisons for a tibble of pairs

This is a thin row-wise wrapper around
[`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md).
It takes a tibble of pairs (`ID1` / `text1` / `ID2` / `text2`), submits
each pair to Gemini 3 Pro, and binds the results into a single tibble.

## Usage

``` r
submit_gemini_pairs_live(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  api_key = Sys.getenv("GEMINI_API_KEY"),
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

  Logical; include `raw_response` list-column.

- include_thoughts:

  Logical; if `TRUE`, requests explicit reasoning output from Gemini and
  stores it in the `thoughts` column of the result, mirroring
  [`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md).

- ...:

  Reserved for future extensions; passed through to
  [`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md)
  (but `thinking_budget` is ignored there).

## Value

A tibble of results (one row per pair).

## Details

The output has one row per pair and the same columns as
[`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md),
making it easy to pass into downstream Bradley-Terry / BTM pipelines.
