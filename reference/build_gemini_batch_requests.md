# Build Gemini batch requests from a tibble of pairs

This helper converts a tibble of writing pairs into a set of Gemini
GenerateContent requests suitable for use with the Batch API
(`models/*:batchGenerateContent`).

## Usage

``` r
build_gemini_batch_requests(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  thinking_level = "low",
  custom_id_prefix = "GEM",
  temperature = NULL,
  top_p = NULL,
  top_k = NULL,
  max_output_tokens = NULL,
  service_tier = "standard",
  include_thoughts = FALSE,
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

  Gemini model name, for example `"gemini-3.5-flash-lite"`. This
  parameter is not embedded in each request object (the model is
  provided via the path), but is included here for symmetry with other
  backends and potential validation.

- trait_name:

  Short label for the trait (for example "Overall Quality").

- trait_description:

  Full-text description of the trait or rubric.

- prompt_template:

  Prompt template string, typically from
  [`set_prompt_template`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).
  The template should embed your full instructions, rubric text, and
  `<BETTER_SAMPLE>` tagging convention.

- thinking_level:

  One of `"minimal"`, `"low"`, `"medium"`, or `"high"`.

  This is mapped to Gemini's `thinkingConfig.thinkingLevel`.

  - For Gemini 3 Flash models (for example `"gemini-3-flash-preview"`),
    `"minimal"` is supported and is passed through as `"minimal"`.

  - For models not matched by the package's Gemini 3 Flash-name detector
    (for example `"gemini-3.5-flash-lite"`), `"minimal"` is not
    supported.

  - For backward compatibility with earlier Gemini 3 Pro usage, `"low"`
    maps to `"low"` and both `"medium"` and `"high"` map to `"high"`.
    "Medium" currently behaves like "High".

- custom_id_prefix:

  Prefix for the `custom_id` field. Defaults to `"GEM"` so that IDs take
  the form `"GEM_<ID1>_vs_<ID2>"`.

- temperature:

  Optional numeric temperature. If `NULL`, it is omitted and Gemini uses
  its own default.

- top_p:

  Optional nucleus sampling parameter. If `NULL`, omitted.

- top_k:

  Optional top-k sampling parameter. If `NULL`, omitted.

- max_output_tokens:

  Optional integer. If `NULL`, omitted.

- service_tier:

  Gemini Developer API service tier. Use `"standard"` (default) or
  `NULL` for provider default behavior, or `"flex"` / `"priority"` to
  encode the documented Gemini `serviceTier` request field.

- include_thoughts:

  Logical; if `TRUE`, sets `thinkingConfig.includeThoughts = TRUE` so
  that Gemini returns visible chain-of-thought. For most pairwise
  scoring use cases this should remain `FALSE`.

- ...:

  Reserved for future extensions. Any `thinking_budget` entries are
  ignored (Gemini 3 does not support thinking budgets).

## Value

A tibble with one row per pair and two main columns:

- custom_id:

  Character ID of the form `"<PREFIX>_<ID1>_vs_<ID2>"`.

- request:

  List-column containing the Gemini GenerateContent request object for
  each pair.

## Details

Each pair receives a unique `custom_id` of the form
`"GEM_<ID1>_vs_<ID2>"` and a corresponding request object containing the
prompt and generation configuration.

## See also

[`llm_submit_pairs_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_batch.md),
[`llm_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/llm_download_batch_results.md)

Other batch backends:
[`anthropic_create_batch()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_create_batch.md),
[`anthropic_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_download_batch_results.md),
[`anthropic_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_get_batch.md),
[`anthropic_poll_batch_until_complete()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_poll_batch_until_complete.md),
[`build_anthropic_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_anthropic_batch_requests.md),
[`build_openai_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_openai_batch_requests.md),
[`gemini_create_batch()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_create_batch.md),
[`gemini_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_download_batch_results.md),
[`gemini_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_get_batch.md),
[`gemini_poll_batch_until_complete()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_poll_batch_until_complete.md),
[`llm_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/llm_download_batch_results.md),
[`llm_resume_multi_batches()`](https://shmercer.github.io/pairwiseLLM/reference/llm_resume_multi_batches.md),
[`llm_submit_pairs_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_batch.md),
[`llm_submit_pairs_multi_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_multi_batch.md),
[`openai_create_batch()`](https://shmercer.github.io/pairwiseLLM/reference/openai_create_batch.md),
[`openai_download_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/openai_download_batch_output.md),
[`openai_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/openai_get_batch.md),
[`openai_poll_batch_until_complete()`](https://shmercer.github.io/pairwiseLLM/reference/openai_poll_batch_until_complete.md),
[`openai_upload_batch_file()`](https://shmercer.github.io/pairwiseLLM/reference/openai_upload_batch_file.md),
[`run_anthropic_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_anthropic_batch_pipeline.md),
[`run_gemini_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_gemini_batch_pipeline.md),
[`run_openai_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_openai_batch_pipeline.md),
[`write_openai_batch_file()`](https://shmercer.github.io/pairwiseLLM/reference/write_openai_batch_file.md)

## Examples

``` r
data("example_writing_samples", package = "pairwiseLLM")

pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 3, seed = 123) |>
  randomize_pair_order(seed = 456)

td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# Dated tested Gemini Developer API configuration
reqs <- build_gemini_batch_requests(
  pairs             = pairs,
  model             = "gemini-3.5-flash-lite",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  thinking_level    = "low",
  include_thoughts  = TRUE
)

reqs
#> # A tibble: 3 × 4
#>   custom_id      ID1   ID2   request         
#>   <chr>          <chr> <chr> <list>          
#> 1 GEM_S17_vs_S12 S17   S12   <named list [2]>
#> 2 GEM_S19_vs_S15 S19   S15   <named list [2]>
#> 3 GEM_S01_vs_S15 S01   S15   <named list [2]>

# Gemini 3 Flash example (minimal thinking)
reqs_flash <- build_gemini_batch_requests(
  pairs             = pairs,
  model             = "gemini-3-flash-preview",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  thinking_level    = "minimal",
  include_thoughts  = FALSE
)

reqs_flash
#> # A tibble: 3 × 4
#>   custom_id      ID1   ID2   request         
#>   <chr>          <chr> <chr> <list>          
#> 1 GEM_S17_vs_S12 S17   S12   <named list [2]>
#> 2 GEM_S19_vs_S15 S19   S15   <named list [2]>
#> 3 GEM_S01_vs_S15 S01   S15   <named list [2]>
```
