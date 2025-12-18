# Download the output file for a completed batch

Given a batch ID, retrieves the batch metadata, extracts the
`output_file_id`, and downloads the corresponding file content to
`path`.

## Usage

``` r
openai_download_batch_output(batch_id, path, api_key = NULL)
```

## Arguments

- batch_id:

  The batch ID (e.g. `"batch_abc123"`).

- path:

  Local file path to write the downloaded `.jsonl` output.

- api_key:

  Optional OpenAI API key.

## Value

Invisibly, the path to the downloaded file.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires OPENAI_API_KEY and a completed batch with an output_file_id.

openai_download_batch_output("batch_abc123", "batch_output.jsonl")

# You can then parse the file
res <- parse_openai_batch_output("batch_output.jsonl")
head(res)
} # }
```
