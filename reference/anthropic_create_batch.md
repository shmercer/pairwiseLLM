# Create an Anthropic Message Batch

This is a thin wrapper around Anthropic's `/v1/messages/batches`
endpoint. It accepts a list of request objects (each with `custom_id`
and `params`) and returns the resulting Message Batch object.

## Usage

``` r
anthropic_create_batch(
  requests,
  api_key = Sys.getenv("ANTHROPIC_API_KEY"),
  anthropic_version = "2023-06-01"
)
```

## Arguments

- requests:

  List of request objects, each of the form
  `list(custom_id = <chr>, params = <list>)`. You can obtain this list
  from the output of
  [`build_anthropic_batch_requests`](https://shmercer.github.io/pairwiseLLM/reference/build_anthropic_batch_requests.md)
  via `split` / `Map`, or use `run_anthropic_batch_pipeline`.

- api_key:

  Optional Anthropic API key. Defaults to
  `Sys.getenv("ANTHROPIC_API_KEY")`.

- anthropic_version:

  Anthropic API version string passed as the `anthropic-version` HTTP
  header. Defaults to `"2023-06-01"`.

## Value

A list representing the Message Batch object returned by Anthropic.
Important fields include `id`, `processing_status`, `request_counts`,
and (after completion) `results_url`.

## Details

Typically you will not call this directly; instead, use
[`run_anthropic_batch_pipeline`](https://shmercer.github.io/pairwiseLLM/reference/run_anthropic_batch_pipeline.md)
which builds requests from a tibble of pairs, creates the batch, polls
for completion, and downloads the results.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires ANTHROPIC_API_KEY and network access.
library(pairwiseLLM)

data("example_writing_samples", package = "pairwiseLLM")

pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 2, seed = 123) |>
  randomize_pair_order(seed = 456)

td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

req_tbl <- build_anthropic_batch_requests(
  pairs             = pairs,
  model             = "claude-sonnet-4-5",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl
)

requests <- lapply(seq_len(nrow(req_tbl)), function(i) {
  list(
    custom_id = req_tbl$custom_id[i],
    params    = req_tbl$params[[i]]
  )
})

batch <- anthropic_create_batch(requests = requests)
batch$id
batch$processing_status
} # }
```
