# Retrieve an OpenAI batch

Retrieve an OpenAI batch

## Usage

``` r
openai_get_batch(batch_id, api_key = Sys.getenv("OPENAI_API_KEY"))
```

## Arguments

- batch_id:

  The batch ID (e.g. `"batch_abc123"`).

- api_key:

  Optional OpenAI API key.

## Value

A list representing the Batch object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires OPENAI_API_KEY and an existing batch ID.

batch <- openai_get_batch("batch_abc123")
batch$status
} # }
```
