# Construct an offline comparative-judgment decision request

Internal foundation for single-trait binary decision endpoints. This
helper constructs a request without HTTP, credentials, retries,
persistence, or model availability checks. Existing live/batch
dispatchers do not call it yet.

## Usage

``` r
.build_decision_request(
  ID1,
  text1,
  ID2,
  text2,
  trait_name,
  trait_description,
  pair_uid,
  provider = c("openai", "typesafe"),
  model = NULL,
  decision_template = NULL,
  prompt_template = NULL
)
```

## Arguments

- ID1, ID2:

  Distinct, nonempty character scalar sample IDs, in presentation order.
  IDs may recur across different pairs; each scheduled occurrence must
  have a distinct `pair_uid` when results are normalized.

- text1, text2:

  Nonmissing character scalar sample texts. Empty text is allowed. Exact
  characters, including whitespace and line endings, are kept.

- trait_name, trait_description:

  Nonempty character scalars. The complete description, including
  construct boundaries, is inserted verbatim. Inserted content is never
  interpreted again as template syntax.

- pair_uid:

  Nonempty character scalar identifying the actual scheduled pair
  occurrence. It is separate from any provider request ID.

- provider:

  Either `"openai"` or `"typesafe"`.

- model:

  Requested model ID. `NULL` selects `"gpt-6-luna"` for OpenAI or pinned
  `"jev-1.13.0"` for TypeSafe. Explicit alternatives are recorded, not
  checked for account availability. Aliases do not guarantee immutable
  weights.

- decision_template:

  `NULL` selects the exact approved `decision_cj_v1` instructions. A
  custom character scalar must contain `{TRAIT_NAME}` and
  `{TRAIT_DESCRIPTION}` and must not embed sample placeholders or the
  generative `<BETTER_SAMPLE>` output tag. Custom text is fingerprinted.

- prompt_template:

  Must be `NULL`. An explicitly supplied generative template is
  rejected. Future decision endpoint wrappers must distinguish
  omitted/defaulted generative arguments (use `decision_cj_v1`) from
  explicitly supplied generative templates (error), and forward custom
  instructions using `decision_template`. Historical
  [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md)
  behavior is unchanged.

## Value

A list with `body` (provider wire representation), `pair_uid`,
`provenance`, and `request_fingerprint`. The fixed question
`writing_trait` has type `choice` and exactly `SAMPLE_1`/`SAMPLE_2`
options. Symmetric option descriptions differ only by sample label.
Shared evidence is one JSON string with those two keys; JSON quoting
gives explicit boundaries even when sample text contains labels, quotes,
or newlines. OpenAI uses `/v1/decisions`, `input` and a
`questions`/`choices` array; TypeSafe uses `/v1/systemone`, `state` and
a `questions`/`criteria` map. Samples are never placed in instructions.

## Details

The built-in instruction value is UTF-8, LF-separated, without a
trailing newline; its SHA-256 is
`06fdf52ff1b9b9128f7e52189053226ad1f15f4ea4a54266ea55a78399ef22af`.

Fingerprint scheme `decision-v1-sha256` hashes UTF-8 compact JSON, with
fixed field/choice order and unboxed scalars. Provenance includes
contract version, provider, endpoint, requested model, question/type,
ordered IDs, SHA-256 hashes of both exact UTF-8 texts and the evidence
rendering, complete trait definition, template version/text, rendered
instructions, and both choice definitions. No Unicode normalization,
trimming, or line-ending conversion is performed. Pair UID, provider
request ID, response/resolved model, and timestamps are not scientific
fingerprint inputs. Future resumption must verify both pair UID and
fingerprint, and inspect resolved-model metadata when aliases are used;
this helper does not implement resumption or guarantee provider
reproducibility.

The body contains private sample text and must not be logged publicly.
Provenance contains text hashes, not sample contents, but custom
trait/template text can itself be sensitive. These model probabilities
do not establish validated educational assessment accuracy. No
statistical model uses them as fractional wins, weights, or additional
judgments.

## See also

[decision_results](https://shmercer.github.io/pairwiseLLM/reference/decision_results.md),
[`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md)

## Examples

``` r
# Entirely offline; this does not submit a request.
request <- pairwiseLLM:::.build_decision_request(
  "A", "A short synthetic sample.", "B", "Another synthetic sample.",
  "Organization", "Logical sequencing and connections between ideas.",
  pair_uid = "example-forward", provider = "openai"
)
request$request_fingerprint
#> [1] "decision-v1-sha256-4412231ad2743e1363c16a7a608a950af10a3080dea4bfc18620e5f3fdceea82"
request$body$questions[[1]]$type
#> [1] "choice"
```
