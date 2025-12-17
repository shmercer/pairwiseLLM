# Extract results from a pairwiseLLM batch object

Helper to extract the parsed results tibble from a batch object returned
by
[`llm_submit_pairs_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_batch.md).
This is a thin wrapper around the `results` element returned by
backend-specific batch pipelines and is designed to be
forward-compatible with future, more asynchronous batch workflows.

## Usage

``` r
llm_download_batch_results(x, ...)
```

## Arguments

- x:

  An object returned by
  [`llm_submit_pairs_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_batch.md)
  (class `"pairwiseLLM_batch"`), or a compatible list that contains a
  `results` element.

- ...:

  Reserved for future use; currently ignored.

## Value

A tibble containing batch comparison results in the standard pairwiseLLM
schema.

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires running a provider batch job first (API key + internet + cost).

batch <- llm_submit_pairs_batch(
  pairs             = tibble::tibble(
    ID1   = "S01",
    text1 = "Text 1",
    ID2   = "S02",
    text2 = "Text 2"
  ),
  backend           = "openai",
  model             = "gpt-4.1",
  trait_name        = trait_description("overall_quality")$name,
  trait_description = trait_description("overall_quality")$description,
  prompt_template   = set_prompt_template()
)

res <- llm_download_batch_results(batch)
res
} # }
```
