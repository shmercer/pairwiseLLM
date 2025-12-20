# Example OpenAI Batch output (JSONL lines)

A small character vector containing three example lines from an OpenAI
Batch API output file in JSONL format. Each element is a single JSON
object representing the result for one batch request.

## Usage

``` r
data("example_openai_batch_output")
```

## Format

A character vector of length 3, where each element is a single JSON line
(JSONL).

## Details

The structure follows the current Batch API output schema, with fields
such as `id`, `custom_id`, and a nested `response` object containing
`status_code`, `request_id`, and a `body` that resembles a regular chat
completion response. One line illustrates a successful comparison where
`<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>` is returned, one illustrates a
case where SAMPLE_2 is preferred, and one illustrates an error case with
a non-200 status.

This dataset is designed for use in examples and tests of batch output
parsing functions. Typical usage is to write the lines to a temporary
file and then read/parse them as a JSONL batch file.

## Examples

``` r
data("example_openai_batch_output")

# Inspect the first line
cat(example_openai_batch_output[1], "\n")
#> {"id": "batch_req_aaa111", "custom_id": "EXP_S01_vs_S02", "response":
#>   {"status_code": 200, "request_id": "req_111aaa", "body":
#>   {"id": "chatcmpl-111aaa", "object": "chat.completion", "created":
#>   1753322001, "model": "o3-2025-04-16", "choices": [{"index": 0, "message":
#>   {"role": "assistant", "content": "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>",
#>   "refusal": null, "annotations": []}, "finish_reason": "stop"}], "usage":
#>   {"prompt_tokens": 440, "completion_tokens": 95, "total_tokens": 535,
#>   "prompt_tokens_details": {"cached_tokens": 0, "audio_tokens": 0},
#>   "completion_tokens_details": {"reasoning_tokens": 64, "audio_tokens": 0,
#>   "accepted_prediction_tokens": 0, "rejected_prediction_tokens": 0}},
#>   "system_fingerprint": null}}, "error": null} 

# Write to a temporary .jsonl file for parsing
tmp <- tempfile(fileext = ".jsonl")
writeLines(example_openai_batch_output, con = tmp)
tmp
#> [1] "/tmp/RtmpsEG8pu/file1e3315c24993.jsonl"
```
