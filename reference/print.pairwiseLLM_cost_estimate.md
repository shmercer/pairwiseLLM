# Print a pairwiseLLM cost estimate

Prints a compact, human-readable summary of an object returned by
[`estimate_llm_pairs_cost`](https://shmercer.github.io/pairwiseLLM/reference/estimate_llm_pairs_cost.md).
The print method reports the backend, model, pilot/remaining pair
counts, estimated token totals, and both the expected and budget cost
estimates.

## Usage

``` r
# S3 method for class 'pairwiseLLM_cost_estimate'
print(x, ...)
```

## Arguments

- x:

  An object of class `"pairwiseLLM_cost_estimate"`, typically returned
  by
  [`estimate_llm_pairs_cost`](https://shmercer.github.io/pairwiseLLM/reference/estimate_llm_pairs_cost.md).

- ...:

  Unused. Included for method compatibility.

## Value

`x`, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
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
} # }
```
