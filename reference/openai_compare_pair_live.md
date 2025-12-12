# Live OpenAI comparison for a single pair of samples

This function sends a single pairwise comparison prompt to the OpenAI
API and parses the result into a small tibble. It is the live /
on-demand analogue of
[`build_openai_batch_requests`](https://shmercer.github.io/pairwiseLLM/reference/build_openai_batch_requests.md)
plus
[`parse_openai_batch_output`](https://shmercer.github.io/pairwiseLLM/reference/parse_openai_batch_output.md).

## Usage

``` r
openai_compare_pair_live(
  ID1,
  text1,
  ID2,
  text2,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  endpoint = c("chat.completions", "responses"),
  tag_prefix = "<BETTER_SAMPLE>",
  tag_suffix = "</BETTER_SAMPLE>",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  include_raw = FALSE,
  ...
)
```

## Arguments

- ID1:

  Character ID for the first sample.

- text1:

  Character string containing the first sample's text.

- ID2:

  Character ID for the second sample.

- text2:

  Character string containing the second sample's text.

- model:

  OpenAI model name (e.g. "gpt-4.1", "gpt-5.2-2025-12-11").

- trait_name:

  Short label for the trait (e.g. "Overall Quality").

- trait_description:

  Full-text definition of the trait.

- prompt_template:

  Prompt template string.

- endpoint:

  Which OpenAI endpoint to use: `"chat.completions"` or `"responses"`.

- tag_prefix:

  Prefix for the better-sample tag.

- tag_suffix:

  Suffix for the better-sample tag.

- api_key:

  Optional OpenAI API key.

- include_raw:

  Logical; if TRUE, adds a `raw_response` column.

- ...:

  Additional OpenAI parameters, for example `temperature`, `top_p`,
  `logprobs`, `reasoning`, and (optionally) `include_thoughts`. The same
  validation rules for gpt-5 models are applied as in
  [`build_openai_batch_requests`](https://shmercer.github.io/pairwiseLLM/reference/build_openai_batch_requests.md).
  When using the Responses endpoint with reasoning models, you can
  request reasoning summaries in the `thoughts` column by setting
  `endpoint = "responses"`, a non-"none" reasoning effort, and
  `include_thoughts = TRUE`.

## Value

A tibble with one row and columns:

- custom_id:

  ID string of the form `"LIVE_<ID1>_vs_<ID2>"`.

- ID1, ID2:

  The sample IDs you supplied.

- model:

  Model name reported by the API.

- object_type:

  OpenAI object type (for example "chat.completion" or "response").

- status_code:

  HTTP-style status code (200 if successful).

- error_message:

  Error message if something goes wrong; otherwise NA.

- thoughts:

  Reasoning / thinking summary text when available, otherwise NA.

- content:

  Concatenated text from the assistant's visible output. For the
  Responses endpoint this is taken from the `type = "message"` output
  items and does not include reasoning summaries.

- better_sample:

  "SAMPLE_1", "SAMPLE_2", or NA.

- better_id:

  ID1 if SAMPLE_1 is chosen, ID2 if SAMPLE_2 is chosen, otherwise NA.

- prompt_tokens:

  Prompt / input token count (if reported).

- completion_tokens:

  Completion / output token count (if reported).

- total_tokens:

  Total token count (if reported).

- raw_response:

  (Optional) list-column containing the parsed JSON body.

## Details

It supports both the Chat Completions endpoint ("/v1/chat/completions")
and the Responses endpoint ("/v1/responses", for example gpt-5.1 with
reasoning), using the same prompt template and model / parameter rules
as the batch pipeline.

For the Responses endpoint, the function collects:

- Reasoning / "thoughts" text (if available) into the `thoughts` column.

- Visible assistant output into the `content` column.

**Temperature Defaults:** If `temperature` is not provided in `...`:

- It defaults to `0` (deterministic) for standard models or when
  reasoning is disabled.

- It remains `NULL` when reasoning is enabled, as the API does not
  support temperature in that mode.

## Examples

``` r
if (FALSE) { # \dontrun{
# 1. Standard comparison using GPT-4.1
res <- openai_compare_pair_live(
  ID1 = "A", text1 = "Text A...",
  ID2 = "B", text2 = "Text B...",
  model = "gpt-4.1",
  trait_name = "clarity",
  trait_description = "Which text is clearer?",
  temperature = 0
)

# 2. Reasoning comparison using GPT-5.2 (date-stamped)
res_reasoning <- openai_compare_pair_live(
  ID1 = "A", text1 = "Text A...",
  ID2 = "B", text2 = "Text B...",
  model = "gpt-5.2-2025-12-11",
  trait_name = "clarity",
  trait_description = "Which text is clearer?",
  endpoint = "responses",
  include_thoughts = TRUE,
  reasoning = "high"
)
print(res_reasoning$thoughts)
} # }
```
