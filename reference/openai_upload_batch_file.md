# Upload a JSONL batch file to OpenAI

Uploads a `.jsonl` file to the OpenAI Files API with purpose `"batch"`,
which can then be used to create a Batch job.

## Usage

``` r
openai_upload_batch_file(
  path,
  purpose = "batch",
  api_key = Sys.getenv("OPENAI_API_KEY")
)
```

## Arguments

- path:

  Path to the local `.jsonl` file to upload.

- purpose:

  File purpose. For the Batch API this should be `"batch"`.

- api_key:

  Optional OpenAI API key. Defaults to `Sys.getenv("OPENAI_API_KEY")`.

## Value

A list representing the File object returned by the API, including `id`,
`filename`, `bytes`, `purpose`, etc.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires OPENAI_API_KEY set in your environment and network access

file_obj <- openai_upload_batch_file("batch_input.jsonl")
file_obj$id
} # }
```
