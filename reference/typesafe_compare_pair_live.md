# Live TypeSafe Jev comparison for a single pair of samples

Compare two writing samples on one trait using TypeSafe's binary Choice
API, `POST https://api.typesafe.ai/v1/systemone`. The default is the
exact approved `decision_cj_v1` instructions, with the complete trait
description preserved. This opt-in helper uses the same scientific
request, integrity validator, response parser, and binary winner
semantics as OpenAI Decisions.

## Usage

``` r
typesafe_compare_pair_live(
  ID1,
  text1,
  ID2,
  text2,
  model = "jev-1.13.0",
  trait_name,
  trait_description,
  prompt_template = NULL,
  api_key = NULL,
  include_raw = FALSE,
  ...
)
```

## Arguments

- ID1, ID2:

  Distinct nonempty character scalar sample IDs, in presentation order.
  Reversing the presentation reverses the sample-to-position mapping.

- text1, text2:

  Nonmissing character scalar sample texts. Empty strings are allowed.
  Whitespace, Unicode characters, and line endings are preserved.

- model:

  Requested model identifier. Defaults to the explicit version
  `"jev-1.13.0"`. Other `jev-<major>.<minor>.<patch>` identifiers and
  the documented aliases `"jev-latest"` and `"jev-preview"` are
  accepted. Identifiers are checked for syntax, not availability;
  TypeSafe determines whether the account can use them. No model-list
  preflight, version allowlist, substitution, or automatic fallback is
  used. Prefer versioned identifiers for reproducible research: aliases
  can resolve differently over time.

- trait_name:

  Nonempty character scalar trait label.

- trait_description:

  Nonempty character scalar containing the full trait definition,
  including construct boundaries; inserted verbatim.

- prompt_template:

  Must be omitted or `NULL`. Generative templates are rejected. Use
  `decision_template` in `...` for custom decision instructions.

- api_key:

  Optional character scalar TypeSafe key. `NULL` or `""` uses
  `TYPESAFE_API_KEY` through the shared credential resolver. Explicit
  keys take precedence; missing, malformed, whitespace-containing, or
  control-character keys fail without printing their values. Persist
  credentials in `.Renviron` and restart R, or use
  `Sys.setenv(TYPESAFE_API_KEY = "YOUR_KEY_HERE")` for the current
  session.
  [`check_llm_api_keys()`](https://shmercer.github.io/pairwiseLLM/reference/check_llm_api_keys.md)
  reports presence only.

- include_raw:

  Logical scalar. Retain successful parsed responses when `TRUE`. Failed
  decision responses are always retained for diagnosis.

- ...:

  Only `pair_uid` and `decision_template` are supported, and must be
  uniquely named. `pair_uid` identifies the scheduled pair occurrence;
  its default is `"LIVE_<ID1>_vs_<ID2>"`. Custom `decision_template`
  instructions must contain `{TRAIT_NAME}` and `{TRAIT_DESCRIPTION}`,
  and must not contain sample placeholders or generative output tags.
  Custom instructions are fingerprinted. Sampling, reasoning, token
  limits, caching, storage, tiers, arbitrary URLs/headers, and
  long-context/image controls are unsupported, including explicitly
  supplied `NULL` settings. Unknown settings fail before credential
  resolution or HTTP.

## Value

A one-row tibble with the shared
[decision_results](https://shmercer.github.io/pairwiseLLM/reference/decision_results.md)
columns:

- ID1, ID2, pair_uid, custom_id:

  Presentation and scheduled pair identity.

- better_sample, better_id, winner_pos:

  Explicit selected sample label, corresponding ID, and integer
  presentation position.

- p_sample_1, p_sample_2, p_chosen, probabilities_json:

  Both provider probabilities, the selected probability, and the full
  binary map.

- decision_confidence:

  Independent provider-reported confidence; never inferred from or
  substituted for the selected probability.

- requested_model, resolved_model, model:

  Requested identifier, provider-reported identifier (`NA` when absent),
  and resolved identifier when supplied, otherwise the requested
  identifier. Missing resolution metadata does not establish which model
  weights served an alias.

- prompt_tokens, completion_tokens, total_tokens:

  Reported input, output, and total usage; unavailable counts are `NA`,
  not inferred. Shared cached/write/reasoning token columns are also
  retained if supplied.

- endpoint, decision_question, decision_type, decision_template_version,
  request_fingerprint, request_provenance_json:

  Scientific identity. Provenance includes provider, full
  trait/instructions/criteria, ordered IDs, and exact text hashes.

- provider_request_id:

  Optional `x-request-id` header, otherwise `NA`.

- object_type, status_code, error_message:

  `"decision"`, HTTP status, and sample-free decision failure detail
  (`NA` on success).

- decision_error_code, decision_error_detail:

  Shared response-validation failure classification and detail; `NA` on
  success.

- thoughts, content:

  `NA`; this endpoint does not generate assessment prose.

- retry_failures:

  List-column of preceding transient attempt records.

- raw_response:

  Optional parsed response, always present on decision failure,
  including malformed output and inconsistent probabilities.

## Details

The wire request contains `model`, `state` (a JSON string with exactly
the ordered `SAMPLE_1` and `SAMPLE_2` texts), and a `questions` map with
one `writing_trait` entry. Its type is `choice`, with rendered
instructions and symmetric `SAMPLE_1`/`SAMPLE_2` criteria descriptions.
Responses use an `answers` map with `choice`, a complete `probabilities`
map, and `confidence`.

The shared integrity validator checks the complete scientific body
against recorded provenance and its SHA-256 fingerprint before provider
operations. The serialized JSON is checked again, attached as raw UTF-8
bytes, and checked with the URL/content type before every HTTP attempt.
Altered requests fail with `pairwiseLLM_decision_request_error`, without
repair or HTTP. This is an integrity/reproducibility guard, not an
authenticated signature.

Exactly two finite probabilities in `[0,1]`, summing to 1 within `1e-6`,
and finite confidence in `[0,1]` are required. Values are never
renormalized, clipped, rounded, complemented, or fabricated. Exact 0,
0.5, and 1 survive. The explicit choice is retained; if the alternative
exceeds its probability by more than `1e-8`, the result fails with
`choice_probability_mismatch`. Missing confidence, malformed/unknown
answer types, incomplete distributions, and invalid usage are classified
by the shared parser. TypeSafe has no documented refusal variant;
unknown types are malformed. These response failures are not retried and
are excluded by the shared normalizer before modeling. Only successful
normalized results are model evidence.

HTTP 408, 429, all 5xx statuses (including 529), and timeouts receive up
to three total attempts with exponential delays of 0.5 and 1 second.
Requests retain identical validated bytes. Other transport failures and
permanent HTTP errors, including authentication, unavailable models, and
schema errors, abort. Conditions have sample-free/key-free messages and
`retry_failures`; HTTP conditions also carry `status_code`. Backoff uses
the package's shared retry helper; it does not currently interpret
`Retry-After` headers.

Only the standard text endpoint is supported: no SDK is required, and no
compression, long-context/image opt-in, managed Batch, bulk execution,
or resumption is implemented here. The provider enforces body/context
limits. This feature has provider-free validation, not an authorized
live availability check. Model probabilities do not establish validated
educational assessment accuracy and do not become BT/adaptive weights,
fractional wins, or judgments.

Sample text is sent to TypeSafe. Keys are omitted from diagnostics.
Request bodies and raw responses can contain private text; sample-free
provenance still includes possibly sensitive custom instructions and
trait definitions. Avoid logging them publicly. This helper provides no
data-retention control.

## See also

[`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md),
[`check_llm_api_keys()`](https://shmercer.github.io/pairwiseLLM/reference/check_llm_api_keys.md),
[decision_request](https://shmercer.github.io/pairwiseLLM/reference/decision_request.md),
[decision_results](https://shmercer.github.io/pairwiseLLM/reference/decision_results.md),
[`openai_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/openai_compare_pair_live.md),
[TypeSafe API reference](https://docs.typesafe.ai/api), [TypeSafe
models](https://docs.typesafe.ai/models)

Other live backends:
[`anthropic_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_compare_pair_live.md),
[`check_llm_api_keys()`](https://shmercer.github.io/pairwiseLLM/reference/check_llm_api_keys.md),
[`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md),
[`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md),
[`ollama_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/ollama_compare_pair_live.md),
[`openai_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/openai_compare_pair_live.md),
[`submit_anthropic_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_anthropic_pairs_live.md),
[`submit_gemini_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_gemini_pairs_live.md),
[`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md),
[`submit_ollama_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_ollama_pairs_live.md),
[`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md),
[`submit_together_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_together_pairs_live.md),
[`submit_vertex_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_vertex_pairs_live.md),
[`together_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/together_compare_pair_live.md),
[`vertex_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/vertex_compare_pair_live.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires TYPESAFE_API_KEY and incurs provider usage charges.
result <- typesafe_compare_pair_live(
  "A", "Synthetic sample A.", "B", "Synthetic sample B.",
  trait_name = "Organization",
  trait_description = "Logical sequencing and connections between ideas."
)
result$better_id
result$p_sample_1

# The same backend through the public dispatcher:
llm_compare_pair(
  "A", "Synthetic sample A.", "B", "Synthetic sample B.",
  model = "jev-1.13.0", backend = "typesafe",
  trait_name = "Organization",
  trait_description = "Logical sequencing and connections between ideas."
)
} # }
```
