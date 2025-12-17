# Run a Gemini batch pipeline for pairwise comparisons

This helper ties together the core batch operations:

1.  Build batch requests from a tibble of pairs.

2.  Create a Batch job via
    [`gemini_create_batch`](https://shmercer.github.io/pairwiseLLM/reference/gemini_create_batch.md).

3.  Optionally poll for completion and download results.

4.  Parse the JSONL results into a tibble via
    [`parse_gemini_batch_output`](https://shmercer.github.io/pairwiseLLM/reference/parse_gemini_batch_output.md).

## Usage

``` r
run_gemini_batch_pipeline(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  thinking_level = c("low", "medium", "high"),
  batch_input_path = tempfile(pattern = "gemini-batch-input-", fileext = ".json"),
  batch_output_path = tempfile(pattern = "gemini-batch-output-", fileext = ".jsonl"),
  poll = TRUE,
  interval_seconds = 60,
  timeout_seconds = 86400,
  api_key = Sys.getenv("GEMINI_API_KEY"),
  api_version = "v1beta",
  verbose = TRUE,
  include_thoughts = FALSE,
  ...
)
```

## Arguments

- pairs:

  Tibble/data frame of pairs.

- model:

  Gemini model name, for example `"gemini-3-pro-preview"`.

- trait_name:

  Trait name.

- trait_description:

  Trait description.

- prompt_template:

  Prompt template string.

- thinking_level:

  One of `"low"`, `"medium"`, or `"high"`.

- batch_input_path:

  Path where the batch input JSON should be written.

- batch_output_path:

  Path where the batch output JSONL should be written (only used if
  `poll = TRUE`).

- poll:

  Logical; if `TRUE`, poll the batch until completion and parse results.
  If `FALSE`, only create the batch and write the input file.

- interval_seconds:

  Polling interval when `poll = TRUE`.

- timeout_seconds:

  Maximum total waiting time when `poll = TRUE`.

- api_key:

  Optional Gemini API key.

- api_version:

  API version string.

- verbose:

  Logical; if `TRUE`, prints progress messages.

- include_thoughts:

  Logical; if `TRUE`, sets `thinkingConfig.includeThoughts = TRUE` in
  each request, mirroring
  [`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md).
  Parsed results will include a `thoughts` column when visible thoughts
  are returned by the API (currently batch typically only exposes
  `thoughtSignature` + `thoughtsTokenCount`).

- ...:

  Additional arguments forwarded to
  [`build_gemini_batch_requests`](https://shmercer.github.io/pairwiseLLM/reference/build_gemini_batch_requests.md)
  (for example `temperature`, `top_p`, `top_k`, `max_output_tokens`).

## Value

A list with elements:

- batch_input_path:

  Path to the written batch input JSON.

- batch_output_path:

  Path to the batch output JSONL (or `NULL` when `poll = FALSE`).

- file:

  Reserved for parity with OpenAI/Anthropic; always `NULL` for Gemini
  inline batches.

- batch:

  The created Batch job object.

- results:

  Parsed tibble of results (or `NULL` when `poll = FALSE`).

## Details

The returned list mirrors the structure of
[`run_openai_batch_pipeline`](https://shmercer.github.io/pairwiseLLM/reference/run_openai_batch_pipeline.md)
and
[`run_anthropic_batch_pipeline`](https://shmercer.github.io/pairwiseLLM/reference/run_anthropic_batch_pipeline.md).

## Examples

``` r
# This example requires:
# - A valid Gemini API key (set in GEMINI_API_KEY)
# - Internet access
# - Billable Gemini API usage
if (FALSE) { # \dontrun{
# Example pairwise data
data("example_writing_samples", package = "pairwiseLLM")

pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 5, seed = 123)

td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# Run the full Gemini batch pipeline
res <- run_gemini_batch_pipeline(
  pairs             = pairs,
  model             = "gemini-3-pro-preview",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  thinking_level    = "low",
  poll              = TRUE,
  include_thoughts  = FALSE
)

# Parsed pairwise comparison results
res$results

# Inspect batch metadata
res$batch

# Paths to saved input/output files
res$batch_input_path
res$batch_output_path
} # }
```
