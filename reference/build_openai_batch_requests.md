# Build OpenAI batch JSONL lines for paired comparisons

This helper constructs one JSON object per pair of writing samples,
suitable for use with the OpenAI batch API. It supports both
`/v1/chat/completions` and `/v1/responses` endpoints.

## Usage

``` r
build_openai_batch_requests(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  endpoint = c("chat.completions", "responses"),
  temperature = NULL,
  top_p = NULL,
  logprobs = NULL,
  reasoning = NULL,
  include_thoughts = FALSE,
  request_id_prefix = "EXP"
)
```

## Arguments

- pairs:

  A data frame or tibble with columns `ID1`, `text1`, `ID2`, and
  `text2`.

- model:

  Character scalar giving the OpenAI model name. Supports standard names
  (e.g. `"gpt-4.1"`, `"gpt-5.6-sol"`) and date-stamped versions (e.g.
  `"gpt-5.4-2026-01-15"`).

- trait_name:

  Short label for the trait (e.g., "Overall Quality").

- trait_description:

  Full-text definition of the trait.

- prompt_template:

  Character template containing the placeholders `{TRAIT_NAME}`,
  `{TRAIT_DESCRIPTION}`, `{SAMPLE_1}`, and `{SAMPLE_2}`. Defaults to
  [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).

- endpoint:

  Which OpenAI endpoint to target. One of `"chat.completions"` (default)
  or `"responses"`.

- temperature:

  Optional temperature parameter. If `NULL`, it is omitted so the
  model/provider default applies. Must be `NULL` for reasoning modes
  that do not support it.

- top_p:

  Optional top-p parameter. If `NULL`, it is omitted so the
  model/provider default applies.

- logprobs:

  Optional logprobs parameter.

- reasoning:

  Optional reasoning effort for GPT-5 series when using the
  `/v1/responses` endpoint. For `"gpt-5"` and `"gpt-5-mini"`, `"none"`
  is normalized to `"minimal"`. For later GPT-5.x reasoning models, use
  model-supported efforts such as `"none"`, `"low"`, `"medium"`,
  `"high"`, `"xhigh"`, or `"max"`.

- include_thoughts:

  Logical; if TRUE and using `responses` endpoint with reasoning,
  requests a summary. Defaults `reasoning` to `"low"` for GPT-5 series
  models if not specified.

- request_id_prefix:

  String prefix for `custom_id`; the full ID takes the form
  `"<prefix>_<ID1>_vs_<ID2>"`.

## Value

A tibble with one row per pair and columns:

- `custom_id`: ID string used by the batch API.

- `method`: HTTP method (`"POST"`).

- `url`: Endpoint path (`"/v1/chat/completions"` or `"/v1/responses"`).

- `body`: List column containing the request body.

## See also

[`llm_submit_pairs_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_batch.md),
[`llm_resume_multi_batches()`](https://shmercer.github.io/pairwiseLLM/reference/llm_resume_multi_batches.md)

Other batch backends:
[`anthropic_create_batch()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_create_batch.md),
[`anthropic_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_download_batch_results.md),
[`anthropic_get_batch()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_get_batch.md),
[`anthropic_poll_batch_until_complete()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_poll_batch_until_complete.md),
[`build_anthropic_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_anthropic_batch_requests.md),
[`build_gemini_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_gemini_batch_requests.md),
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
data("example_writing_samples", package = "pairwiseLLM")

pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 3, seed = 123) |>
  randomize_pair_order(seed = 456)

td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# 1. Basic chat.completions batch with no thoughts
batch_tbl_chat <- build_openai_batch_requests(
  pairs             = pairs,
  model             = "gpt-4.1",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  endpoint          = "chat.completions"
)

# 2. GPT-5.6 Sol Responses Batch with Reasoning
batch_tbl_resp <- build_openai_batch_requests(
  pairs = pairs,
  model = "gpt-5.6-sol",
  trait_name = td$name,
  trait_description = td$description,
  prompt_template = tmpl,
  endpoint = "responses",
  include_thoughts = TRUE, # implies reasoning="low" if not set
  reasoning = "medium"
)

batch_tbl_chat
#> # A tibble: 3 × 4
#>   custom_id      method url                  body            
#>   <chr>          <chr>  <chr>                <list>          
#> 1 EXP_S17_vs_S12 POST   /v1/chat/completions <named list [2]>
#> 2 EXP_S19_vs_S15 POST   /v1/chat/completions <named list [2]>
#> 3 EXP_S01_vs_S15 POST   /v1/chat/completions <named list [2]>
batch_tbl_resp
#> # A tibble: 3 × 4
#>   custom_id      method url           body            
#>   <chr>          <chr>  <chr>         <list>          
#> 1 EXP_S17_vs_S12 POST   /v1/responses <named list [3]>
#> 2 EXP_S19_vs_S15 POST   /v1/responses <named list [3]>
#> 3 EXP_S01_vs_S15 POST   /v1/responses <named list [3]>
```
