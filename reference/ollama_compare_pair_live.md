# Live Ollama comparison for a single pair of samples

`ollama_compare_pair_live()` sends a single pairwise comparison prompt
to a local Ollama server and parses the result into the standard
pairwiseLLM tibble format.

## Usage

``` r
ollama_compare_pair_live(
  ID1,
  text1,
  ID2,
  text2,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  host = getOption("pairwiseLLM.ollama_host", "http://127.0.0.1:11434"),
  tag_prefix = "<BETTER_SAMPLE>",
  tag_suffix = "</BETTER_SAMPLE>",
  think = FALSE,
  num_ctx = 8192L,
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

  Ollama model name (for example `"mistral-small3.2:24b"`,
  `"qwen3:32b"`, `"gemma3:27b"`).

- trait_name:

  Short label for the trait (for example `"Overall Quality"`).

- trait_description:

  Full-text definition of the trait.

- prompt_template:

  Prompt template string, typically from
  [`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).

- host:

  Base URL of the Ollama server. Defaults to the option
  `getOption("pairwiseLLM.ollama_host", "http://127.0.0.1:11434")`.

- tag_prefix:

  Prefix for the better-sample tag. Defaults to `"<BETTER_SAMPLE>"`.

- tag_suffix:

  Suffix for the better-sample tag. Defaults to `"</BETTER_SAMPLE>"`.

- think:

  Logical; if `TRUE` and the model is a Qwen model (name starts with
  `"qwen"`), the temperature is set to `0.6`. Otherwise the temperature
  is `0`. The `think` argument does not itself modify the HTTP request
  body; it is used only for choosing the temperature, but the function
  will parse a `thinking` field from the response whenever one is
  present.

- num_ctx:

  Integer; context window to use via `options$num_ctx`. The default is
  `8192L`.

- include_raw:

  Logical; if `TRUE`, adds a list-column `raw_response` containing the
  parsed JSON body returned by Ollama (or `NULL` on parse failure). This
  is useful for debugging.

- ...:

  Reserved for future extensions.

## Value

A tibble with one row and columns:

- `custom_id` – ID string of the form `"LIVE_<ID1>_vs_<ID2>"`.

- `ID1`, `ID2` – the sample IDs supplied to the function.

- `model` – model name reported by the API (or the requested model).

- `object_type` – backend object type (for example `"ollama.generate"`).

- `status_code` – HTTP-style status code (`200` if successful).

- `error_message` – error message if something goes wrong; otherwise
  `NA`.

- `thoughts` – reasoning / thinking text when a `thinking` field is
  returned by Ollama; otherwise `NA`.

- `content` – visible response text from the model (from the `response`
  field).

- `better_sample` – `"SAMPLE_1"`, `"SAMPLE_2"`, or `NA`, based on tags
  found in `content`.

- `better_id` – `ID1` if `"SAMPLE_1"` is chosen, `ID2` if `"SAMPLE_2"`
  is chosen, otherwise `NA`.

- `prompt_tokens` – prompt / input token count (if reported).

- `completion_tokens` – completion / output token count (if reported).

- `total_tokens` – total token count (if reported).

- `raw_response` – optional list-column containing the parsed JSON body
  (present only when `include_raw = TRUE`).

## Details

The function targets the `/api/generate` endpoint on a running Ollama
instance and expects a single non-streaming response. Model names should
match those available in your Ollama installation (for example
`"mistral-small3.2:24b"`, `"qwen3:32b"`, `"gemma3:27b"`).

Temperature and context length are controlled as follows:

- By default, `temperature = 0` for all models.

- For Qwen models (model names beginning with `"qwen"`) and
  `think = TRUE`, `temperature` is set to `0.6`.

- The context window is set via `options$num_ctx`, which defaults to
  `8192L` but may be overridden via the `num_ctx` argument.

If the Ollama response includes a `thinking` field (as described in the
Ollama API), that string is stored in the `thoughts` column of the
returned tibble; otherwise `thoughts` is `NA`. This allows pairwiseLLM
to consume Ollama's native thinking output in a way that is consistent
with other backends that expose explicit reasoning traces.

The Ollama backend is intended to be compatible with the existing
OpenAI, Anthropic, and Gemini backends, so the returned tibble can be
used directly with downstream helpers such as
[`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md)
and
[`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md).

In typical workflows, users will call
[`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md)
with `backend = "ollama"` rather than using `ollama_compare_pair_live()`
directly. The direct helper is exported so that advanced users can work
with Ollama in a more explicit and backend-specific way.

The function assumes that:

- An Ollama server is running and reachable at `host`.

- The requested `model` has already been pulled, for example via
  `ollama pull mistral-small3.2:24b` on the command line.

When the Ollama response includes a `thinking` field (as documented in
the Ollama API), that string is copied into the `thoughts` column of the
returned tibble; otherwise `thoughts` is `NA`. This parsed thinking
output can be logged, inspected, or analyzed alongside the visible
comparison decisions.

## See also

- [`submit_ollama_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_ollama_pairs_live.md)
  for single-backend, row-wise comparisons.

- [`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md)
  for backend-agnostic single-pair comparisons.

- [`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md)
  for backend-agnostic comparisons over tibbles of pairs.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires a running Ollama server and locally available models.

data("example_writing_samples", package = "pairwiseLLM")

td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

ID1 <- example_writing_samples$ID[1]
ID2 <- example_writing_samples$ID[2]
text1 <- example_writing_samples$text[1]
text2 <- example_writing_samples$text[2]

# Make sure an Ollama server is running

# mistral example
res_mistral <- ollama_compare_pair_live(
  ID1               = ID1,
  text1             = text1,
  ID2               = ID2,
  text2             = text2,
  model             = "mistral-small3.2:24b",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl
)

res_mistral$better_id

# qwen example with reasoning
res_qwen_think <- ollama_compare_pair_live(
  ID1               = ID1,
  text1             = text1,
  ID2               = ID2,
  text2             = text2,
  model             = "qwen3:32b",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  think             = TRUE,
  include_raw       = TRUE
)

res_qwen_think$better_id
res_qwen_think$thoughts
} # }
```
