# Run a full OpenAI batch pipeline for pairwise comparisons

This helper wires together the existing pieces:

- [`build_openai_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_openai_batch_requests.md)

- [`write_openai_batch_file()`](https://shmercer.github.io/pairwiseLLM/reference/write_openai_batch_file.md)

- [`openai_upload_batch_file()`](https://shmercer.github.io/pairwiseLLM/reference/openai_upload_batch_file.md)

- [`openai_create_batch()`](https://shmercer.github.io/pairwiseLLM/reference/openai_create_batch.md)

- optionally
  [`openai_poll_batch_until_complete()`](https://shmercer.github.io/pairwiseLLM/reference/openai_poll_batch_until_complete.md)

- optionally
  [`openai_download_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/openai_download_batch_output.md)

- optionally
  [`parse_openai_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/parse_openai_batch_output.md)

## Usage

``` r
run_openai_batch_pipeline(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  include_thoughts = FALSE,
  include_raw = FALSE,
  endpoint = NULL,
  batch_input_path = tempfile("openai_batch_input_", fileext = ".jsonl"),
  batch_output_path = tempfile("openai_batch_output_", fileext = ".jsonl"),
  poll = TRUE,
  interval_seconds = 5,
  timeout_seconds = 600,
  max_attempts = Inf,
  metadata = NULL,
  api_key = Sys.getenv("OPENAI_API_KEY"),
  ...
)
```

## Arguments

- pairs:

  Tibble of pairs with at least `ID1`, `text1`, `ID2`, `text2`.
  Typically produced by
  [`make_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/make_pairs.md),
  [`sample_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/sample_pairs.md),
  and
  [`randomize_pair_order()`](https://shmercer.github.io/pairwiseLLM/reference/randomize_pair_order.md).

- model:

  OpenAI model name (e.g. `"gpt-4.1"`, `"gpt-5.1"`).

- trait_name:

  Trait name to pass to
  [`build_openai_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_openai_batch_requests.md).

- trait_description:

  Trait description to pass to
  [`build_openai_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_openai_batch_requests.md).

- prompt_template:

  Prompt template string, typically from
  [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).

- include_thoughts:

  Logical; if `TRUE` and using `endpoint = "responses"`, requests
  reasoning-style summaries to populate the `thoughts` column in the
  parsed output. When `endpoint` is not supplied,
  `include_thoughts = TRUE` causes the `responses` endpoint to be
  selected automatically.

- include_raw:

  Logical; if `TRUE`, attaches the raw model response as a list-column
  `raw_response` in the parsed results.

- endpoint:

  One of `"chat.completions"` or `"responses"`. If `NULL` (or omitted),
  it is chosen automatically as described above.

- batch_input_path:

  Path to write the batch input `.jsonl` file. Defaults to a temporary
  file.

- batch_output_path:

  Path to write the batch output `.jsonl` file if `poll = TRUE`.
  Defaults to a temporary file.

- poll:

  Logical; if `TRUE`, the function will poll the batch until it reaches
  a terminal status using
  [`openai_poll_batch_until_complete()`](https://shmercer.github.io/pairwiseLLM/reference/openai_poll_batch_until_complete.md)
  and then download and parse the output. If `FALSE`, it stops after
  creating the batch and returns without polling or parsing.

- interval_seconds:

  Polling interval in seconds (used when `poll = TRUE`).

- timeout_seconds:

  Maximum total time in seconds for polling before giving up (used when
  `poll = TRUE`).

- max_attempts:

  Maximum number of polling attempts (primarily useful for testing).

- metadata:

  Optional named list of metadata key–value pairs to pass to
  [`openai_create_batch()`](https://shmercer.github.io/pairwiseLLM/reference/openai_create_batch.md).

- api_key:

  Optional OpenAI API key. Defaults to `Sys.getenv("OPENAI_API_KEY")`.

- ...:

  Additional arguments passed through to
  [`build_openai_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_openai_batch_requests.md),
  e.g. `temperature`, `top_p`, `logprobs`, `reasoning`.

## Value

A list with elements:

- `batch_input_path` – path to the input `.jsonl` file.

- `batch_output_path` – path to the output `.jsonl` file (or `NULL` if
  `poll = FALSE`).

- `file` – File object returned by
  [`openai_upload_batch_file()`](https://shmercer.github.io/pairwiseLLM/reference/openai_upload_batch_file.md).

- `batch` – Batch object; if `poll = TRUE`, this is the final batch
  after polling, otherwise the initial batch returned by
  [`openai_create_batch()`](https://shmercer.github.io/pairwiseLLM/reference/openai_create_batch.md).

- `results` – Parsed tibble from
  [`parse_openai_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/parse_openai_batch_output.md)
  if `poll = TRUE`, otherwise `NULL`.

## Details

It is a convenience wrapper around these smaller functions and is
intended for end-to-end batch runs on a set of pairwise comparisons. For
more control (or testing), you can call the components directly.

When `endpoint` is not specified, it is chosen automatically:

- if `include_thoughts = TRUE`, the `"responses"` endpoint is used and,
  for `"gpt-5.1"`, a default reasoning effort of `"low"` is applied
  (unless overridden via `reasoning`).

- otherwise, `"chat.completions"` is used.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires OPENAI_API_KEY and network access.
library(pairwiseLLM)

data("example_writing_samples", package = "pairwiseLLM")

pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 5, seed = 123) |>
  randomize_pair_order(seed = 456)

td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# 1) Standard chat.completions batch (no thoughts)
pipeline_chat <- run_openai_batch_pipeline(
  pairs             = pairs,
  model             = "gpt-4.1",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  endpoint          = "chat.completions",
  interval_seconds  = 10,
  timeout_seconds   = 600
)

pipeline_chat$batch$status
head(pipeline_chat$results)

# 2) Responses endpoint with reasoning summaries (thoughts) for gpt-5.1
pipeline_resp <- run_openai_batch_pipeline(
  pairs             = pairs,
  model             = "gpt-5.1",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  include_thoughts  = TRUE,   # automatically picks "responses" + reasoning
  interval_seconds  = 10,
  timeout_seconds   = 600
)

pipeline_resp$batch$status
head(pipeline_resp$results)
} # }
```
