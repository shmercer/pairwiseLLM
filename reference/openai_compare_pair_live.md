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

  OpenAI model name (for example "gpt-4.1", "gpt-5.1").

- trait_name:

  Short label for the trait (for example "Overall Quality").

- trait_description:

  Full-text definition of the trait.

- prompt_template:

  Prompt template string, typically from
  [`set_prompt_template`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).

- endpoint:

  Which OpenAI endpoint to use. One of `"chat.completions"` or
  `"responses"`.

- tag_prefix:

  Prefix for the better-sample tag. Defaults to `"<BETTER_SAMPLE>"`.

- tag_suffix:

  Suffix for the better-sample tag. Defaults to `"</BETTER_SAMPLE>"`.

- api_key:

  Optional OpenAI API key. Defaults to `Sys.getenv("OPENAI_API_KEY")`.

- include_raw:

  Logical; if TRUE, adds a list-column `raw_response` containing the
  parsed JSON body returned by OpenAI (or NULL on parse failure). This
  is useful for debugging parsing problems.

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
  Reasoning summaries are typically provided on the `type = "reasoning"`
  output item under `summary`.

- Visible assistant output into the `content` column, taken from the
  `type = "message"` output item's `content[[*]]$text`.

Reasoning text is not prefixed into `content`; instead it is kept
separate in `thoughts` for consistency with Anthropic and Gemini.
