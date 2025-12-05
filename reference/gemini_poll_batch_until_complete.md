# Poll a Gemini Batch job until completion

This helper repeatedly calls
[`gemini_get_batch`](https://shmercer.github.io/pairwiseLLM/reference/gemini_get_batch.md)
until the batch's `metadata$state` enters a terminal state or a time
limit is reached. For the REST API, states have the form
"BATCH_STATE\_\*".

## Usage

``` r
gemini_poll_batch_until_complete(
  batch_name,
  interval_seconds = 60,
  timeout_seconds = 86400,
  api_key = Sys.getenv("GEMINI_API_KEY"),
  api_version = "v1beta",
  verbose = TRUE
)
```

## Arguments

- batch_name:

  Character scalar giving the batch name.

- interval_seconds:

  Polling interval in seconds. Defaults to 60.

- timeout_seconds:

  Maximum total waiting time in seconds. Defaults to 24 hours (86400
  seconds).

- api_key:

  Optional Gemini API key. Defaults to `Sys.getenv("GEMINI_API_KEY")`.

- api_version:

  API version string for the path; defaults to `"v1beta"`.

- verbose:

  Logical; if `TRUE`, prints progress messages.

## Value

The final Batch job object as returned by
[`gemini_get_batch`](https://shmercer.github.io/pairwiseLLM/reference/gemini_get_batch.md).
