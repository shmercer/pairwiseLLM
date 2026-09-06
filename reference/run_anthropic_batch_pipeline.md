# Run an Anthropic batch pipeline for pairwise comparisons

This high-level helper mirrors
[`run_openai_batch_pipeline`](https://shmercer.github.io/pairwiseLLM/reference/run_openai_batch_pipeline.md)
but targets Anthropic's *Message Batches API*. It:

## Usage

``` r
run_anthropic_batch_pipeline(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  reasoning = c("none", "enabled"),
  include_thoughts = FALSE,
  batch_input_path = NULL,
  batch_output_path = NULL,
  poll = TRUE,
  interval_seconds = 60,
  timeout_seconds = 86400,
  api_key = Sys.getenv("ANTHROPIC_API_KEY"),
  anthropic_version = "2023-06-01",
  verbose = TRUE,
  ...
)
```

## Arguments

- pairs:

  Tibble or data frame with at least columns `ID1`, `text1`, `ID2`,
  `text2`.

- model:

  Anthropic model name (for example `"claude-sonnet-4-5"`).

- trait_name:

  Trait name to pass to
  [`build_anthropic_batch_requests`](https://shmercer.github.io/pairwiseLLM/reference/build_anthropic_batch_requests.md).

- trait_description:

  Trait description to pass to
  [`build_anthropic_batch_requests`](https://shmercer.github.io/pairwiseLLM/reference/build_anthropic_batch_requests.md).

- prompt_template:

  Prompt template string, typically from
  [`set_prompt_template`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).

- reasoning:

  Character scalar; one of `"none"` or `"enabled"`. See details above
  for how `include_thoughts` influences this value and how temperature
  defaults are derived.

- include_thoughts:

  Logical; if `TRUE`, requests extended thinking from Claude (by setting
  `reasoning = "enabled"` when necessary) and parses any thinking blocks
  into a `thoughts` column in the batch results.

- batch_input_path:

  Path to write the JSON file containing the `requests` object. Defaults
  to a temporary file with suffix `".json"`.

- batch_output_path:

  Path to write the downloaded `.jsonl` results if `poll = TRUE`.
  Defaults to a temporary file with suffix `".jsonl"`.

- poll:

  Logical; if `TRUE`, the function will poll the batch until it reaches
  `processing_status = "ended"` using
  [`anthropic_poll_batch_until_complete`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_poll_batch_until_complete.md)
  and then download and parse the output. If `FALSE`, it stops after
  creating the batch and returns without polling or parsing.

- interval_seconds:

  Polling interval in seconds (used when `poll = TRUE`).

- timeout_seconds:

  Maximum total time in seconds for polling before giving up (used when
  `poll = TRUE`).

- api_key:

  Optional Anthropic API key. Defaults to
  `Sys.getenv("ANTHROPIC_API_KEY")`.

- anthropic_version:

  Anthropic API version string passed as the `anthropic-version` HTTP
  header. Defaults to `"2023-06-01"`.

- verbose:

  Logical; if `TRUE`, prints progress messages while polling.

- ...:

  Additional Anthropic parameters forwarded to
  [`build_anthropic_batch_requests`](https://shmercer.github.io/pairwiseLLM/reference/build_anthropic_batch_requests.md)
  (for example `max_tokens`, `temperature`, `top_p`,
  `thinking_budget_tokens`).

## Value

A list with elements (aligned with
[`run_openai_batch_pipeline`](https://shmercer.github.io/pairwiseLLM/reference/run_openai_batch_pipeline.md)):

- batch_input_path:

  Path to the JSON file containing the batch `requests` object.

- batch_output_path:

  Path to the downloaded `.jsonl` results file if `poll = TRUE`,
  otherwise `NULL`.

- file:

  Always `NULL` for Anthropic batches (OpenAI uses a File object here).
  Included for structural compatibility.

- batch:

  Message Batch object; if `poll = TRUE`, this is the final batch after
  polling, otherwise the initial batch returned by
  [`anthropic_create_batch`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_create_batch.md).

- results:

  Parsed tibble from
  [`parse_anthropic_batch_output`](https://shmercer.github.io/pairwiseLLM/reference/parse_anthropic_batch_output.md)
  if `poll = TRUE`, otherwise `NULL`.

## Details

1.  Builds Anthropic batch requests from a tibble of pairs using
    [`build_anthropic_batch_requests`](https://shmercer.github.io/pairwiseLLM/reference/build_anthropic_batch_requests.md).

2.  Writes a JSON file containing the `requests` object for
    reproducibility.

3.  Creates a Message Batch via
    [`anthropic_create_batch`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_create_batch.md).

4.  Optionally polls until the batch reaches
    `processing_status = "ended"` using
    [`anthropic_poll_batch_until_complete`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_poll_batch_until_complete.md).

5.  If polling is enabled, downloads the `.jsonl` result file with
    [`anthropic_download_batch_results`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_download_batch_results.md)
    and parses it via
    [`parse_anthropic_batch_output`](https://shmercer.github.io/pairwiseLLM/reference/parse_anthropic_batch_output.md).

It is the Anthropic analogue of
[`run_openai_batch_pipeline`](https://shmercer.github.io/pairwiseLLM/reference/run_openai_batch_pipeline.md)
and returns a list with the same overall structure so that downstream
code can treat the two backends uniformly.

When `include_thoughts = TRUE` and `reasoning` is left at its default of
`"none"`, this function automatically upgrades `reasoning` to
`"enabled"` so that Claude's extended thinking blocks are returned and
parsed into the `thoughts` column by
[`parse_anthropic_batch_output`](https://shmercer.github.io/pairwiseLLM/reference/parse_anthropic_batch_output.md).

**Temperature and reasoning defaults**

Temperature and thinking-mode behaviour are controlled by
[`build_anthropic_batch_requests`](https://shmercer.github.io/pairwiseLLM/reference/build_anthropic_batch_requests.md):

- When `reasoning = "none"` (no extended thinking):

  - Omitted `temperature` and `top_p` values are not sent, so the
    model/provider defaults apply.

  - The default `max_tokens` is `768`, unless you override it via
    `max_tokens` in `...`.

- When `reasoning = "enabled"` (extended thinking enabled):

  - `temperature` **must** be `1`. If you supply a different value in
    `...`,
    [`build_anthropic_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_anthropic_batch_requests.md)
    will throw an error.

  - By default, `max_tokens = 2048` and `thinking_budget_tokens = 1024`,
    subject to the constraint
    `1024 <= thinking_budget_tokens < max_tokens`. Violations of this
    constraint also produce an error.

Therefore, batches without extended thinking use model-default sampling.
When you explicitly use extended thinking (either by setting
`reasoning = "enabled"` or by using `include_thoughts = TRUE`),
Anthropic's requirement of `temperature = 1` is enforced.

## See also

[`llm_submit_pairs_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_batch.md),
[`llm_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/llm_download_batch_results.md)

Other batch backends:
[`anthropic_create_batch()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_create_batch.md),
[`anthropic_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_download_batch_results.md),
[`anthropic_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_get_batch.md),
[`anthropic_poll_batch_until_complete()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_poll_batch_until_complete.md),
[`build_anthropic_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_anthropic_batch_requests.md),
[`build_gemini_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_gemini_batch_requests.md),
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
[`run_gemini_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_gemini_batch_pipeline.md),
[`run_openai_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_openai_batch_pipeline.md),
[`write_openai_batch_file()`](https://shmercer.github.io/pairwiseLLM/reference/write_openai_batch_file.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires ANTHROPIC_API_KEY and network access.
library(pairwiseLLM)

data("example_writing_samples", package = "pairwiseLLM")

pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 5, seed = 123) |>
  randomize_pair_order(seed = 456)

td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# Standard batch without extended thinking
pipeline_none <- run_anthropic_batch_pipeline(
  pairs             = pairs,
  model             = "claude-sonnet-4-5",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  reasoning         = "none",
  include_thoughts  = FALSE,
  interval_seconds  = 60,
  timeout_seconds   = 3600,
  verbose           = TRUE
)

pipeline_none$batch$processing_status
head(pipeline_none$results)

# Batch with extended thinking and thoughts column
pipeline_thoughts <- run_anthropic_batch_pipeline(
  pairs             = pairs,
  model             = "claude-sonnet-4-5",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  include_thoughts  = TRUE,
  interval_seconds  = 60,
  timeout_seconds   = 3600,
  verbose           = TRUE
)

pipeline_thoughts$batch$processing_status
head(pipeline_thoughts$results)
} # }
```
