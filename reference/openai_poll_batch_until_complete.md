# Poll an OpenAI batch until it completes or fails

Repeatedly calls
[`openai_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/openai_get_batch.md)
until the batch reaches a terminal status (one of `"completed"`,
`"failed"`, `"cancelled"`, `"expired"`), a timeout is reached, or
`max_attempts` is exceeded.

## Usage

``` r
openai_poll_batch_until_complete(
  batch_id,
  interval_seconds = 5,
  timeout_seconds = 600,
  max_attempts = Inf,
  api_key = NULL,
  verbose = TRUE
)
```

## Arguments

- batch_id:

  The batch ID.

- interval_seconds:

  Number of seconds to wait between polling attempts.

- timeout_seconds:

  Maximum total time to wait in seconds before giving up.

- max_attempts:

  Maximum number of polling attempts. This is mainly useful for testing;
  default is `Inf`.

- api_key:

  Optional OpenAI API key.

- verbose:

  Logical; if `TRUE`, prints status messages to the console.

## Value

The final Batch object (a list) as returned by
[`openai_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/openai_get_batch.md).

## Details

This is a synchronous helper – it will block until one of the conditions
above is met.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires OPENAI_API_KEY and a created batch that may still be running.

batch <- openai_create_batch("file_123", endpoint = "/v1/chat/completions")

final <- openai_poll_batch_until_complete(
  batch_id         = batch$id,
  interval_seconds = 10,
  timeout_seconds  = 3600
)

final$status
} # }
```
