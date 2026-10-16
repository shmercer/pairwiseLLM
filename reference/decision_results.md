# Parse offline provider decisions and preserve binary probability evidence

Convert one provider response to a row accepted by the shared
normalizer. This is an internal, pure converter, with no HTTP or
automatic retries.

## Usage

``` r
.parse_decision_response(
  response,
  request,
  provider_request_id = NULL,
  include_raw = FALSE
)
```

## Arguments

- response:

  JSON character scalar or an unsimplified parsed JSON object (as
  returned by `jsonlite::fromJSON(..., simplifyVector = FALSE)`).
  Strings are parsed literally; filenames and URLs are never read.
  OpenAI requires an `answers` array containing exactly the requested
  named answer; TypeSafe requires an `answers` map with exactly the
  requested key. Unexpected nesting, duplicate keys/options, extra
  answers, and non-choice answers fail.

- request:

  An unmodified request from
  [decision_request](https://shmercer.github.io/pairwiseLLM/reference/decision_request.md).

- provider_request_id:

  Optional provider-assigned request ID, distinct from the scheduled
  pair UID. `NULL` records `NA`.

- include_raw:

  Logical scalar; retain raw successful responses when true. Failed
  decision responses are always retained, including mismatches.

## Value

A one-row tibble retaining `ID1`, `ID2`, actual `pair_uid`/`custom_id`,
`better_sample`, `better_id`, integer `winner_pos`, `model`, and usage
(`prompt_tokens`, `completion_tokens`, `total_tokens`; `NA` when
unavailable). `model` is the resolved model when supplied, otherwise the
requested model; `requested_model` and `resolved_model` distinguish them
without claiming resolution when the provider omits it. Additive scalar
columns are:

- p_sample_1, p_sample_2:

  Probabilities for the presented IDs, never sorted IDs. Reversed
  presentations therefore reverse their ID mapping.

- p_chosen:

  Probability for the explicitly returned choice.

- decision_confidence:

  Independent provider confidence, never inferred from `p_chosen` or the
  distribution. Both current wire schemas require it.

- probabilities_json:

  Complete binary map in SAMPLE_1/SAMPLE_2 order, preserving numeric
  values. For malformed containers, the original probability
  representation is kept when serializable.

- decision_question, decision_type, decision_template_version, endpoint,
  request_fingerprint:

  Scientific request identity.

- request_provenance_json:

  Sample-free provenance from the constructor; includes the full trait
  definition, instructions, choice definitions, and text hashes. It can
  still contain sensitive custom instruction text.

- provider_request_id, trait_name:

  Transport identity and focal trait for diagnostics, alongside
  presentation IDs and ordered keys.

- decision_error_code, decision_error_detail:

  `NA` on success, otherwise a fixed, sample-free reason. The shared
  normalizer moves failures to `failed_pairs` and `failed_attempts`
  before its generic winner filter.

## Details

Exactly two finite numeric probabilities in `[0,1]` are required,
summing to 1 within absolute tolerance `1e-6`. Values are never
renormalized, rounded, clipped, complemented, or fabricated. Exact 0,
0.5, and 1 are retained. Confidence must be a finite numeric scalar in
`[0,1]`; missing confidence fails. Historical generative rows receive
`NA` decision fields in the normalizer.

The returned choice is authoritative: no argmax substitution is made. If
the alternative exceeds its probability by more than `1e-8`, the row
fails with `choice_probability_mismatch`. Both probabilities, original
selection, and raw response remain available for later
provider/trait/order analysis. Equal probabilities retain the explicit
selection. Mismatches must not be retried without an explicit future
policy. Other failure codes are `invalid_choice`,
`invalid_probabilities`, `invalid_confidence`, `malformed_decision`,
`decision_refusal`, and `decision_identity_mismatch` (during
normalization). OpenAI's explicit `type = "refusal"` is distinct from
malformed output. The current TypeSafe schema has no refusal variant;
unknown answer types there remain malformed rather than being guessed
from prose.

Only normalized successful `results` are modeling evidence; retained
failed choices are diagnostics. Existing BT/Bayesian/adaptive code is
unchanged and does not consume probabilities as weights, fractional
wins, or extra outcomes. Raw responses may contain sensitive provider
content. Do not log them publicly.

## See also

[decision_request](https://shmercer.github.io/pairwiseLLM/reference/decision_request.md),
[`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md)

## Examples

``` r
request <- pairwiseLLM:::.build_decision_request(
  "A", "Synthetic sample A.", "B", "Synthetic sample B.",
  "Organization", "Logical sequencing of ideas.", "example", "typesafe"
)
response <- list(
  model = "jev-1.13.0",
  answers = list(writing_trait = list(
    type = "choice", choice = "SAMPLE_2", confidence = 0.4,
    probabilities = list(SAMPLE_1 = 0.3, SAMPLE_2 = 0.7)
  ))
)
pairwiseLLM:::.parse_decision_response(response, request)$p_chosen
#> [1] 0.7
```
