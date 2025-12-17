# Download Gemini Batch results to a JSONL file

For inline batch requests, Gemini returns results under
`response$inlinedResponses$inlinedResponses`. In the v1beta REST API
this often comes back as a data frame with one row per request and a
`"response"` column, where each `"response"` is itself a data frame of
`GenerateContentResponse` objects.

## Usage

``` r
gemini_download_batch_results(
  batch,
  requests_tbl,
  output_path,
  api_key = Sys.getenv("GEMINI_API_KEY"),
  api_version = "v1beta"
)
```

## Arguments

- batch:

  Either a parsed batch object (as returned by
  [`gemini_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_get_batch.md))
  or a character batch name such as `"batches/123..."`.

- requests_tbl:

  Tibble/data frame with a `custom_id` column in the same order as the
  submitted requests.

- output_path:

  Path to the JSONL file to create.

- api_key:

  Optional Gemini API key (used only when `batch` is a name).

- api_version:

  API version (default `"v1beta"`).

## Value

Invisibly returns `output_path`.

## Details

This helper writes those results to a local `.jsonl` file where each
line is a JSON object of the form:

    {"custom_id": "<GEM_ID1_vs_ID2>",
     "result": {
       "type": "succeeded",
       "response": { ... GenerateContentResponse ... }
     }}

or, when an error occurred:

    {"custom_id": "<GEM_ID1_vs_ID2>",
     "result": {
       "type": "errored",
       "error": { ... }
     }}

## Examples

``` r
# This example requires a Gemini API key and network access.
# It assumes you have already created and run a Gemini batch job.
if (FALSE) { # \dontrun{
# Name of an existing Gemini batch
batch_name <- "batches/123456"

# Requests table used to create the batch (must include custom_id)
requests_tbl <- tibble::tibble(
  custom_id = c("GEM_S01_vs_S02", "GEM_S03_vs_S04")
)

# Download inline batch results to a local JSONL file
out_file <- tempfile(fileext = ".jsonl")

gemini_download_batch_results(
  batch        = batch_name,
  requests_tbl = requests_tbl,
  output_path  = out_file
)

# Inspect the downloaded JSONL
readLines(out_file, warn = FALSE)
} # }
```
