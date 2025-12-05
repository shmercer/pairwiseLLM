# Retrieve a Gemini Batch job by name

This retrieves the latest state of a Batch job using its `name` as
returned by
[`gemini_create_batch`](https://shmercer.github.io/pairwiseLLM/reference/gemini_create_batch.md).

## Usage

``` r
gemini_get_batch(
  batch_name,
  api_key = Sys.getenv("GEMINI_API_KEY"),
  api_version = "v1beta"
)
```

## Arguments

- batch_name:

  Character scalar giving the batch name.

- api_key:

  Optional Gemini API key. Defaults to `Sys.getenv("GEMINI_API_KEY")`.

- api_version:

  API version string for the path; defaults to `"v1beta"`.

## Value

A list representing the Batch job object.

## Details

It corresponds to a GET request on `/v1beta/<BATCH_NAME>`, where
`BATCH_NAME` is a string such as `"batches/123456"`.
