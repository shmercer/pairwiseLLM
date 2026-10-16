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
  endpoint = c("chat.completions", "responses", "decisions"),
  tag_prefix = "<BETTER_SAMPLE>",
  tag_suffix = "</BETTER_SAMPLE>",
  api_key = NULL,
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

  OpenAI model name (e.g. "gpt-4.1", "gpt-5.6-sol"). Decisions currently
  accepts only `"gpt-6-luna"`; other model IDs fail before HTTP.

- trait_name:

  Short label for the trait (e.g. "Overall Quality").

- trait_description:

  Full-text definition of the trait.

- prompt_template:

  Generative prompt template string. For Decisions, omit this argument
  or use `NULL`; explicitly supplied non-NULL templates fail. Use
  `decision_template` in `...` for custom decision instructions.

- endpoint:

  Which OpenAI endpoint to use: `"chat.completions"` (the unchanged
  default), `"responses"`, or `"decisions"`.

- tag_prefix:

  Prefix for the better-sample tag. Must be omitted for Decisions.

- tag_suffix:

  Suffix for the better-sample tag. Must be omitted for Decisions.

- api_key:

  Optional OpenAI API key.

- include_raw:

  Logical; if TRUE, adds a `raw_response` column.

- ...:

  Additional OpenAI parameters, for example `temperature`, `top_p`,
  `logprobs`, `reasoning`, `service_tier`, `max_output_tokens`, `store`,
  `pair_uid`, and (optionally) `include_thoughts`. `max_output_tokens`
  must be a positive integer and is supported only by the Responses
  endpoint. When `pair_uid` is supplied, it is used verbatim as
  `custom_id`. `store` accepts `NULL` or one non-missing logical value
  for either generative endpoint. `TRUE`/`FALSE` are sent unchanged;
  omission or `NULL` leaves the field absent and preserves the OpenAI
  default. For either generative endpoint, `service_tier = "standard"`
  or `"default"` sends `"default"`; explicit `"auto"`, `"flex"`, and
  `"priority"` are sent unchanged. Omission or `NULL` leaves the field
  absent, allowing OpenAI's project-configured `"auto"` behavior.
  Model/tier availability is validated by OpenAI. For Responses,
  `store = FALSE` disables response storage for later retrieval. This is
  not a general data-retention guarantee. The same validation rules for
  gpt-5 models are applied as in
  [`build_openai_batch_requests`](https://shmercer.github.io/pairwiseLLM/reference/build_openai_batch_requests.md).
  When using the Responses endpoint with reasoning models, you can
  request reasoning summaries in the `thoughts` column by setting
  `endpoint = "responses"`, a non-"none" reasoning effort, and
  `include_thoughts = TRUE`. For Decisions, only `pair_uid`,
  `decision_template`, and `prompt_cache_options` are supported in
  `...`; see below. Duplicate, unnamed, unsupported, or misspelled
  settings fail before credentials or HTTP.

## Value

A tibble with one row and columns:

- custom_id:

  Stable ID for the pair (`pair_uid` if supplied via `...`; otherwise
  `"LIVE_<ID1>_vs_<ID2>"`).

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

  (Optional) list-column containing the parsed JSON body. Failed
  Decisions always retain their response, including invalid JSON text,
  regardless of `include_raw`.

Decisions additionally returns the columns documented in
[decision_results](https://shmercer.github.io/pairwiseLLM/reference/decision_results.md),
including integer `winner_pos`, `pair_uid`, both probabilities,
`p_chosen`, independent `decision_confidence`, full
`probabilities_json`, scientific provenance/fingerprint,
requested/resolved model, and provider `x-request-id`. `object_type` is
`"decision"`; `thoughts` and `content` are `NA`. `prompt_cached_tokens`,
`prompt_cache_write_tokens`, and `reasoning_tokens` retain reported
usage details, including zero; unavailable counts are `NA`.
`request_controls_json` records submitted `prompt_cache_options` (or
`null`) and `effective_prompt_cache_options` with effective mode and
TTL. These transport settings are separate from the scientific
fingerprint. `retry_failures` is a list-column of transient attempt
diagnostics, consumed by the existing normalizer for `failed_attempts`.

## Details

It supports the Chat Completions endpoint ("/v1/chat/completions") and
the Responses endpoint ("/v1/responses", for example gpt-5.6 with
reasoning), using the same prompt template and model / parameter rules
as the batch pipeline. The opt-in Decisions endpoint (`"/v1/decisions"`)
instead uses one structured binary choice question; see the Decisions
section below.

For the Responses endpoint, the function collects:

- Reasoning / "thoughts" text (if available) into the `thoughts` column.

- Visible assistant output into the `content` column.

**Sampling defaults:** If `temperature` or `top_p` is not provided in
`...`, the corresponding field is omitted so the model/provider default
applies. Reasoning modes that do not support sampling parameters
continue to require them to be `NULL`.

## Decisions

Decisions is single-pair, single-trait and binary only. `input` is the
exact shared JSON-string evidence built by
[decision_request](https://shmercer.github.io/pairwiseLLM/reference/decision_request.md);
`questions` contains one named `choice` question, `writing_trait`, with
the ordered `SAMPLE_1` and `SAMPLE_2` values and symmetric descriptions.
Omission of the generative prompt does not evaluate
[`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).
The default instructions are the exact approved `decision_cj_v1`. A
custom `decision_template` must contain `{TRAIT_NAME}` and
`{TRAIT_DESCRIPTION}`, and cannot contain sample placeholders or
generative output tags. The complete trait definition is inserted
verbatim and fingerprinted. No instructions request generated
probabilities, reasoning traces, or XML.

The selected `choice` is preserved, never replaced with an argmax. Both
probabilities must be finite numbers in `[0,1]` summing to 1 within
`1e-6`; exact 0, 0.5 and 1 are kept. Independent confidence is required
in `[0,1]`. A selection below the alternative by more than `1e-8` is a
diagnostic failure, retaining the original choice and probabilities.
Malformed JSON/answers, refusals, and incomplete distributions return
classified failed rows; they are never valid binary evidence or
automatically retried. See
[decision_results](https://shmercer.github.io/pairwiseLLM/reference/decision_results.md)
for error codes and failure retention. HTTP errors/timeouts use the
existing transport/retry conditions and attempt metadata; no generative
fallback occurs.

`prompt_cache_options` is `NULL` or a nonempty named list with only
`mode` (`"implicit"`, `"explicit"`, or `NULL`) and/or `ttl` (`"30m"`).
Omitted values use implicit mode and a 30-minute minimum TTL. Explicit
mode disables the implicit breakpoint; this helper adds no explicit
breakpoints, so explicit mode disables prompt caching for this request.
Cache entries can persist longer than the TTL. This is caching, not
request storage or a general data-retention guarantee. Account/project
data controls still apply. `store`, service/processing tiers, cache
keys, breakpoints, and Batch cache policy controls are unsupported here.
All supplied generative controls, including `temperature`, `top_p`,
`logprobs`, `reasoning`, `max_output_tokens`, and `include_thoughts`,
fail even when `NULL` or `FALSE`.

Before request construction, the scientific body is checked
independently against provenance, exact ordered text/evidence hashes,
instructions, question, and choices. The serialized JSON is checked
again, attached as exact UTF-8 bytes, and verified before every HTTP
attempt, including retries. Inconsistent requests abort with
`pairwiseLLM_decision_request_error` and sample-free diagnostics before
HTTP; no automatic repair is performed. A fingerprint checks
identity/integrity and is not an authenticated signature.

Requests contain the complete sample texts. Provenance excludes samples
but includes full trait/custom instruction text, which can itself be
sensitive. Failed raw responses can also contain sensitive content; do
not log publicly. These probabilities do not establish validated
educational assessment accuracy and do not change binary BT/adaptive
evidence. Use
[`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md)
or
[`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md)
for live bulk execution; managed Batch remains unsupported for
Decisions. A live smoke run on 2026-10-16 exercised `gpt-6-luna` on 20
synthetic samples, 60 primary pairs and 10 reverse audits; see the
model-compatibility vignette for the tested commit and limits.
Probabilities are not established calibrated probabilities of assessment
correctness. API availability remains provider/account dependent. The
schema and controls follow the [OpenAI Decisions
reference](https://developers.openai.com/api/reference/resources/decisions/methods/create).

## See also

[`check_llm_api_keys()`](https://shmercer.github.io/pairwiseLLM/reference/check_llm_api_keys.md),
[`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md)

Other live backends:
[`anthropic_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_compare_pair_live.md),
[`check_llm_api_keys()`](https://shmercer.github.io/pairwiseLLM/reference/check_llm_api_keys.md),
[`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md),
[`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md),
[`ollama_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/ollama_compare_pair_live.md),
[`submit_anthropic_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_anthropic_pairs_live.md),
[`submit_gemini_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_gemini_pairs_live.md),
[`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md),
[`submit_ollama_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_ollama_pairs_live.md),
[`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md),
[`submit_together_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_together_pairs_live.md),
[`submit_typesafe_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_typesafe_pairs_live.md),
[`submit_vertex_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_vertex_pairs_live.md),
[`together_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/together_compare_pair_live.md),
[`typesafe_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/typesafe_compare_pair_live.md),
[`vertex_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/vertex_compare_pair_live.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires API key set and internet access

# 1. Standard comparison using GPT-4.1
res <- openai_compare_pair_live(
  ID1 = "A", text1 = "Text A...",
  ID2 = "B", text2 = "Text B...",
  model = "gpt-4.1",
  trait_name = "clarity",
  trait_description = "Which text is clearer?"
)

# 2. Reasoning comparison using GPT-5.6 Sol
res_reasoning <- openai_compare_pair_live(
  ID1 = "A", text1 = "Text A...",
  ID2 = "B", text2 = "Text B...",
  model = "gpt-5.6-sol",
  trait_name = "clarity",
  trait_description = "Which text is clearer?",
  endpoint = "responses",
  include_thoughts = TRUE,
  reasoning = "high",
  service_tier = "flex"
)
print(res_reasoning$thoughts)

# 3. One decision with both choice probabilities (also incurs API costs)
res_decision <- openai_compare_pair_live(
  "A", "Synthetic sample A.", "B", "Synthetic sample B.",
  model = "gpt-6-luna", trait_name = "Organization",
  trait_description = "Logical sequencing and connections between ideas.",
  endpoint = "decisions", pair_uid = "example-forward",
  prompt_cache_options = list(mode = "explicit")
)
res_decision[c("better_id", "p_sample_1", "p_sample_2", "decision_confidence")]
} # }
```
