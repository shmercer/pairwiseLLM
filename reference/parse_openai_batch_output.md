# Parse an OpenAI Batch output JSONL file

This function reads an OpenAI Batch API output file (JSONL) and extracts
pairwise comparison results for use with Bradley–Terry models. It
supports both the Chat Completions endpoint (where
`object = "chat.completion"`) and the Responses endpoint (where
`object = "response"`), including GPT-5.1 with reasoning.

## Usage

``` r
parse_openai_batch_output(
  path,
  tag_prefix = "<BETTER_SAMPLE>",
  tag_suffix = "</BETTER_SAMPLE>"
)
```

## Arguments

- path:

  Path to a JSONL output file downloaded from the OpenAI Batch API.

- tag_prefix:

  Character string marking the start of the better-sample tag. Defaults
  to `"<BETTER_SAMPLE>"`.

- tag_suffix:

  Character string marking the end of the better-sample tag. Defaults to
  `"</BETTER_SAMPLE>"`.

## Value

A tibble with one row per successfully parsed comparison and columns:

- custom_id:

  The `custom_id` from the batch request.

- ID1, ID2:

  Sample IDs inferred from `custom_id`.

- model:

  The model name reported by the API.

- object_type:

  The OpenAI response object type (e.g., `"chat.completion"` or
  `"response"`).

- status_code:

  HTTP-style status code from the batch output.

- error_message:

  Error message, if present; otherwise `NA`.

- thoughts:

  Reasoning / thinking summary text when available (for Responses with
  reasoning); otherwise `NA`.

- content:

  The raw assistant visible content string (the LLM's output), used to
  locate the `<BETTER_SAMPLE>` tag. For Responses with reasoning this
  does not include reasoning summaries, which are kept in `thoughts`.

- better_sample:

  Either `"SAMPLE_1"`, `"SAMPLE_2"`, or `NA` if the tag was not found.

- better_id:

  `ID1` if `SAMPLE_1` was chosen, `ID2` if `SAMPLE_2` was chosen, or
  `NA`.

- prompt_tokens:

  Prompt/input token count (if reported).

- completion_tokens:

  Completion/output token count (if reported).

- total_tokens:

  Total tokens (if reported).

- prompt_cached_tokens:

  Cached prompt tokens (if reported via
  `input_tokens_details$cached_tokens`); otherwise `NA`.

- reasoning_tokens:

  Reasoning tokens (if reported via
  `output_tokens_details$reasoning_tokens`); otherwise `NA`.

## Details

For each line, the function:

- extracts `custom_id` and parses `ID1` and `ID2` from the pattern
  `"<prefix>ID1_vs_ID2"`,

- pulls the raw LLM content containing the
  `<BETTER_SAMPLE>...</BETTER_SAMPLE>` tag,

- determines whether `SAMPLE_1` or `SAMPLE_2` was selected and maps that
  to `better_id`,

- collects model name and token usage statistics (including reasoning
  tokens for GPT-5.1 Responses),

- when using the Responses endpoint with reasoning, separates reasoning
  summaries into the `thoughts` column and visible assistant output into
  `content`.

The returned data frame is suitable as input for
[`build_bt_data`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md).
