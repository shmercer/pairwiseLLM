# Parse Anthropic Message Batch output into a tibble

This function parses a `.jsonl` file produced by
[`anthropic_download_batch_results`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_download_batch_results.md).
Each line in the file is a JSON object with at least:

## Usage

``` r
parse_anthropic_batch_output(
  jsonl_path,
  tag_prefix = "<BETTER_SAMPLE>",
  tag_suffix = "</BETTER_SAMPLE>"
)
```

## Arguments

- jsonl_path:

  Path to a `.jsonl` file produced by
  [`anthropic_download_batch_results`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_download_batch_results.md).

- tag_prefix:

  Prefix for the better-sample tag. Defaults to `"<BETTER_SAMPLE>"`.

- tag_suffix:

  Suffix for the better-sample tag. Defaults to `"</BETTER_SAMPLE>"`.

## Value

A tibble with one row per result. The columns mirror
[`anthropic_compare_pair_live`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_compare_pair_live.md)
with batch-specific additions:

- custom_id:

  Batch custom ID (for example `"ANTH_S01_vs_S02"`).

- ID1, ID2:

  Sample IDs recovered from `custom_id`.

- model:

  Model name reported by Anthropic.

- object_type:

  Anthropic object type (for example `"message"`).

- status_code:

  HTTP-style status code (200 for succeeded results, `NA` otherwise).

- result_type:

  One of `"succeeded"`, `"errored"`, `"canceled"`, `"expired"`.

- error_message:

  Error message for non-succeeded results, otherwise NA.

- thoughts:

  Extended thinking text returned by Claude when reasoning is enabled
  (for example when `reasoning = "enabled"`), otherwise NA.

- content:

  Concatenated assistant text for succeeded results.

- better_sample:

  "SAMPLE_1", "SAMPLE_2", or NA.

- better_id:

  ID1 if SAMPLE_1 is chosen, ID2 if SAMPLE_2 is chosen, otherwise NA.

- prompt_tokens:

  Prompt / input token count (if reported).

- completion_tokens:

  Completion / output token count (if reported).

- total_tokens:

  Total token count (reported or computed upstream).

## Details

    {
      "custom_id": "ANTH_S01_vs_S02",
      "result": {
        "type": "succeeded" | "errored" | "canceled" | "expired",
        "message": { ... }  # when type == "succeeded"
        "error":   { ... }  # when type == "errored" (optional)
      }
    }

Results may be returned in any order. This function uses the `custom_id`
field to recover `ID1` and `ID2` and then applies the same parsing logic
as
[`anthropic_compare_pair_live`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_compare_pair_live.md),
including extraction of extended thinking blocks (when enabled) into a
separate `thoughts` column.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires ANTHROPIC_API_KEY and a completed batch.
library(pairwiseLLM)

# Suppose you have already run run_anthropic_batch_pipeline() with poll =
# TRUE:
# pipeline <- run_anthropic_batch_pipeline(...)
# jsonl_path <- pipeline$batch_output_path

# You can parse the results .jsonl file directly:
# tbl <- parse_anthropic_batch_output(jsonl_path)
# dplyr::count(tbl, result_type)

# For illustration only (do not run without a real path):
# tbl <- parse_anthropic_batch_output("anthropic-results.jsonl")
} # }
```
