# Create an OpenAI batch from an uploaded file

Creates and executes a batch based on a previously uploaded input file.

## Usage

``` r
openai_create_batch(
  input_file_id,
  endpoint,
  completion_window = "24h",
  metadata = NULL,
  api_key = Sys.getenv("OPENAI_API_KEY")
)
```

## Arguments

- input_file_id:

  The ID of the uploaded file (with purpose `"batch"`).

- endpoint:

  The endpoint for the batch, e.g. `"/v1/chat/completions"` or
  `"/v1/responses"`.

- completion_window:

  Time frame in which the batch should be processed. Currently only
  `"24h"` is supported by the API.

- metadata:

  Optional named list of metadata key–value pairs.

- api_key:

  Optional OpenAI API key.

## Value

A list representing the Batch object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires OPENAI_API_KEY set in your environment and network access.
# Example: create a batch for a previously uploaded file.

file_obj <- openai_upload_batch_file("batch_input.jsonl")

batch_obj <- openai_create_batch(
  input_file_id = file_obj$id,
  endpoint      = "/v1/chat/completions"
)

batch_obj$status
} # }
```
