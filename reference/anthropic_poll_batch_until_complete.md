# Poll an Anthropic Message Batch until completion

This helper repeatedly calls
[`anthropic_get_batch`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_get_batch.md)
until the batch's `processing_status` becomes `"ended"` or a time limit
is reached. It is analogous to
[`openai_poll_batch_until_complete()`](https://shmercer.github.io/pairwiseLLM/reference/openai_poll_batch_until_complete.md)
but for Anthropic's Message Batches API.

## Usage

``` r
anthropic_poll_batch_until_complete(
  batch_id,
  interval_seconds = 60,
  timeout_seconds = 86400,
  api_key = Sys.getenv("ANTHROPIC_API_KEY"),
  anthropic_version = "2023-06-01",
  verbose = TRUE
)
```

## Arguments

- batch_id:

  Character scalar giving the batch ID.

- interval_seconds:

  Polling interval in seconds. Defaults to 60.

- timeout_seconds:

  Maximum total waiting time in seconds. Defaults to 24 hours (`86400`
  seconds).

- api_key:

  Optional Anthropic API key. Defaults to
  `Sys.getenv("ANTHROPIC_API_KEY")`.

- anthropic_version:

  Anthropic API version string passed as the `anthropic-version` HTTP
  header. Defaults to `"2023-06-01"`.

- verbose:

  Logical; if `TRUE`, prints progress messages.

## Value

The final Message Batch object as returned by
[`anthropic_get_batch`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_get_batch.md)
once `processing_status == "ended"` or the last object retrieved before
timing out.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires ANTHROPIC_API_KEY and network access.
batch <- anthropic_create_batch(requests = my_requests)
final <- anthropic_poll_batch_until_complete(batch$id, interval_seconds = 30)
final$processing_status
} # }
```
