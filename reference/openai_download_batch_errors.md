# Download the error file for an OpenAI batch

Retrieves batch metadata and downloads the raw JSONL file identified by
`error_file_id`. This works even when the batch has no `output_file_id`.
An error is raised if no valid error file ID is available; the message
includes the batch ID and status. HTTP and local file-write errors
propagate. An existing local file is overwritten only after its content
is downloaded.

## Usage

``` r
openai_download_batch_errors(batch_id, path, api_key = NULL)
```

## Arguments

- batch_id:

  The batch ID (e.g. `"batch_abc123"`).

- path:

  Local file path to write the downloaded error `.jsonl` file.

- api_key:

  Optional OpenAI API key. Defaults to `Sys.getenv("OPENAI_API_KEY")`.

## Value

Invisibly, the path to the downloaded file.

## Details

Successful requests are retrieved with
[`openai_download_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/openai_download_batch_output.md).
Reconcile both files against submitted requests by `custom_id`, not line
order. This helper does not parse errors or retry failed comparisons.

## Retrieval retries

Batch metadata and result-file GET requests retry HTTP 408, 429, all 5xx
responses, and transport failures, with at most three total HTTP
attempts per GET. Valid `Retry-After` seconds or HTTP dates take
precedence; otherwise retries use exponential backoff starting at 0.5
seconds plus up to 0.25 seconds of jitter, capped at 30 seconds. Other
HTTP errors fail immediately. Exhaustion raises the original error with
the additional class `pairwiseLLM_batch_retry_exhausted`. These retries
retrieve the same batch; they do not resubmit comparisons or create
scientific failed-attempt rows.

## See also

[`openai_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/openai_get_batch.md),
[`openai_download_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/openai_download_batch_output.md)

Other batch backends:
[`anthropic_create_batch()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_create_batch.md),
[`anthropic_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_download_batch_results.md),
[`anthropic_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_get_batch.md),
[`anthropic_poll_batch_until_complete()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_poll_batch_until_complete.md),
[`build_anthropic_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_anthropic_batch_requests.md),
[`build_gemini_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_gemini_batch_requests.md),
[`build_openai_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_openai_batch_requests.md),
[`gemini_create_batch()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_create_batch.md),
[`gemini_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_download_batch_results.md),
[`gemini_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_get_batch.md),
[`gemini_poll_batch_until_complete()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_poll_batch_until_complete.md),
[`llm_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/llm_download_batch_results.md),
[`llm_resume_multi_batches()`](https://shmercer.github.io/pairwiseLLM/reference/llm_resume_multi_batches.md),
[`llm_submit_pairs_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_batch.md),
[`llm_submit_pairs_multi_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_multi_batch.md),
[`openai_create_batch()`](https://shmercer.github.io/pairwiseLLM/reference/openai_create_batch.md),
[`openai_download_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/openai_download_batch_output.md),
[`openai_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/openai_get_batch.md),
[`openai_poll_batch_until_complete()`](https://shmercer.github.io/pairwiseLLM/reference/openai_poll_batch_until_complete.md),
[`openai_upload_batch_file()`](https://shmercer.github.io/pairwiseLLM/reference/openai_upload_batch_file.md),
[`run_anthropic_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_anthropic_batch_pipeline.md),
[`run_gemini_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_gemini_batch_pipeline.md),
[`run_openai_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_openai_batch_pipeline.md),
[`write_openai_batch_file()`](https://shmercer.github.io/pairwiseLLM/reference/write_openai_batch_file.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires OPENAI_API_KEY and a batch with an error_file_id.
openai_download_batch_errors("batch_abc123", "batch_errors.jsonl")
errors <- lapply(readLines("batch_errors.jsonl"), jsonlite::fromJSON)
} # }
```
