# Retrieve an Anthropic Message Batch by ID

This retrieves the latest state of a Message Batch using its `id`. It
corresponds to a `GET` request on
`/v1/messages/batches/<MESSAGE_BATCH_ID>`.

## Usage

``` r
anthropic_get_batch(
  batch_id,
  api_key = Sys.getenv("ANTHROPIC_API_KEY"),
  anthropic_version = "2023-06-01"
)
```

## Arguments

- batch_id:

  Character scalar giving the batch ID (for example
  `"msgbatch_01HkcTjaV5uDC8jWR4ZsDV8d"`).

- api_key:

  Optional Anthropic API key. Defaults to
  `Sys.getenv("ANTHROPIC_API_KEY")`.

- anthropic_version:

  Anthropic API version string passed as the `anthropic-version` HTTP
  header. Defaults to `"2023-06-01"`.

## Value

A list representing the Message Batch object, including fields such as
`id`, `processing_status`, `request_counts`, and (after completion)
`results_url`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires ANTHROPIC_API_KEY and network access.
# After creating a batch:
batch <- anthropic_create_batch(requests = my_requests)
batch_id <- batch$id

latest <- anthropic_get_batch(batch_id)
latest$processing_status
} # }
```
