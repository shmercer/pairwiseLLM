# Download Anthropic Message Batch results (.jsonl)

Once a Message Batch has finished processing (status `"ended"`),
Anthropic exposes a `results_url` field pointing to a `.jsonl` file
containing one JSON object per request result.

## Usage

``` r
anthropic_download_batch_results(
  batch_id,
  output_path,
  api_key = Sys.getenv("ANTHROPIC_API_KEY"),
  anthropic_version = "2023-06-01"
)
```

## Arguments

- batch_id:

  Character scalar giving the batch ID.

- output_path:

  File path where the `.jsonl` results should be written.

- api_key:

  Optional Anthropic API key. Defaults to
  `Sys.getenv("ANTHROPIC_API_KEY")`.

- anthropic_version:

  Anthropic API version string passed as the `anthropic-version` HTTP
  header. Defaults to `"2023-06-01"`.

## Value

Invisibly, the `output_path`.

## Details

This helper downloads that file and writes it to disk. It is the
Anthropic counterpart to
[`openai_download_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/openai_download_batch_output.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires ANTHROPIC_API_KEY and network access.
final <- anthropic_poll_batch_until_complete(batch$id)
jsonl_path <- tempfile(fileext = ".jsonl")
anthropic_download_batch_results(final$id, jsonl_path)
} # }
```
