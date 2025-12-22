# Estimate LLM token usage and cost for a set of pairwise comparisons

Estimate total token usage and cost for running a large set of pairwise
comparisons by:

- running a small pilot on `n_test` pairs (live calls) to observe
  `prompt_tokens` and `completion_tokens`, and

- using the pilot to calibrate a prompt-bytes-to-input-token model for
  the remaining pairs, and

- prorating output tokens for the remaining pairs from the pilot
  distribution.

## Usage

``` r
estimate_llm_pairs_cost(
  pairs,
  model,
  trait_name,
  trait_description,
  prompt_template = set_prompt_template(),
  backend = c("openai", "anthropic", "gemini", "together"),
  endpoint = c("chat.completions", "responses"),
  mode = c("live", "batch"),
  n_test = 25,
  test_strategy = c("stratified_prompt_bytes", "random", "first"),
  seed = NULL,
  cost_per_million_input,
  cost_per_million_output,
  batch_discount = 1,
  budget_quantile = 0.9,
  return_test_results = TRUE,
  return_remaining_pairs = TRUE,
  ...
)
```

## Arguments

- pairs:

  Tibble or data frame with at least columns `ID1`, `text1`, `ID2`,
  `text2`. Typically created by
  [`make_pairs`](https://shmercer.github.io/pairwiseLLM/reference/make_pairs.md),
  [`sample_pairs`](https://shmercer.github.io/pairwiseLLM/reference/sample_pairs.md),
  and
  [`randomize_pair_order`](https://shmercer.github.io/pairwiseLLM/reference/randomize_pair_order.md).

- model:

  Model name to use for the pilot run (and for the target job).

- trait_name:

  Short label for the trait (for example "Overall Quality").

- trait_description:

  Full-text description of the trait or rubric.

- prompt_template:

  Prompt template string, typically from
  [`set_prompt_template`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md).

- backend:

  Backend for the pilot run; one of `"openai"`, `"anthropic"`,
  `"gemini"`, or `"together"`.

- endpoint:

  OpenAI endpoint; one of `"chat.completions"` or `"responses"`. Ignored
  for other backends.

- mode:

  Target execution mode for the full job; one of `"live"` or `"batch"`.
  The pilot is always run live. If `mode = "batch"`, `batch_discount` is
  applied to the estimated cost for the remaining (non-pilot) pairs.

- n_test:

  Number of pilot pairs to run live. Defaults to 25 or fewer if fewer
  pairs are supplied.

- test_strategy:

  Strategy for selecting pilot pairs: `"stratified_prompt_bytes"`
  (default), `"random"`, or `"first"`.

- seed:

  Optional integer seed used for pilot sampling when `test_strategy` is
  not `"first"`.

- cost_per_million_input:

  Cost per one million input tokens (prompt tokens), in your currency of
  choice.

- cost_per_million_output:

  Cost per one million output tokens (completion tokens).
  Reasoning/thinking tokens are treated as output.

- batch_discount:

  Numeric scalar multiplier applied to the estimated cost for the
  remaining pairs when `mode = "batch"`. For example, if batch pricing
  is 50 percent of live pricing, use `batch_discount = 0.5`.

- budget_quantile:

  Quantile used for the "budget" output-token estimate for remaining
  pairs. Defaults to `0.9` (p90).

- return_test_results:

  Logical; if `TRUE`, include pilot results in the returned object so
  you can reuse them and avoid paying twice.

- return_remaining_pairs:

  Logical; if `TRUE`, include the remaining pairs (excluding pilot
  pairs) in the returned object.

- ...:

  Additional arguments forwarded to
  [`submit_llm_pairs`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md)
  for the pilot run (for example `api_key`, `reasoning`,
  `include_thoughts`, `max_tokens`, etc.).

## Value

An object of class `"pairwiseLLM_cost_estimate"`, a list with:

- summary:

  A one-row tibble with expected and budget token and cost estimates
  (and pilot usage).

- calibration:

  A list describing the input-token calibration (coefficients and fit
  diagnostics).

- test_pairs:

  The pilot pair subset.

- pilot:

  Pilot results (when `return_test_results = TRUE`).

- remaining_pairs:

  Remaining pairs (when `return_remaining_pairs = TRUE`).

## Details

The estimator does not require a provider tokenizer. Input tokens are
estimated from the byte length of the fully constructed prompt and
calibrated on the pilot's observed `prompt_tokens`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires an API key and internet access.
data("example_writing_samples", package = "pairwiseLLM")

pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 50, seed = 123)

td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

est <- estimate_llm_pairs_cost(
  pairs = pairs,
  backend = "openai",
  model = "gpt-4.1",
  endpoint = "chat.completions",
  trait_name = td$name,
  trait_description = td$description,
  prompt_template = tmpl,
  mode = "batch",
  batch_discount = 0.5,
  n_test = 10,
  cost_per_million_input = 0.15,
  cost_per_million_output = 0.60
)

est
est$summary

# Reuse pilot results and run only remaining pairs:
remaining <- est$remaining_pairs
} # }
```
