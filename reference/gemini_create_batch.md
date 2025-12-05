# Create a Gemini Batch job from request objects

This is a thin wrapper around the REST endpoint
`/v1beta/models/<MODEL>:batchGenerateContent`. It accepts a list of
GenerateContent request objects and returns the created Batch job.

## Usage

``` r
gemini_create_batch(
  requests,
  model,
  api_key = Sys.getenv("GEMINI_API_KEY"),
  api_version = "v1beta",
  display_name = NULL
)
```

## Arguments

- requests:

  List of GenerateContent request objects, each of the form
  `list(contents = ..., generationConfig = ...)`. You can obtain this
  list from the output of
  [`build_gemini_batch_requests`](https://shmercer.github.io/pairwiseLLM/reference/build_gemini_batch_requests.md)
  via `batch$request`.

- model:

  Gemini model name, for example `"gemini-3-pro-preview"`.

- api_key:

  Optional Gemini API key. Defaults to `Sys.getenv("GEMINI_API_KEY")`.

- api_version:

  API version string for the path; defaults to `"v1beta"`.

- display_name:

  Optional display name for the batch.

## Value

A list representing the Batch job object returned by Gemini. Important
fields include `name`, `metadata$state`, and (after completion)
`response$inlinedResponses` or `response$responsesFile`.

## Details

Typically you will not call this directly; instead, use
[`run_gemini_batch_pipeline`](https://shmercer.github.io/pairwiseLLM/reference/run_gemini_batch_pipeline.md)
which builds requests from a tibble of pairs, creates the batch, polls
for completion, and parses the results.
