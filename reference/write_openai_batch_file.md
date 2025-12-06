# Write an OpenAI batch table to a JSONL file

This helper takes the output of
[`build_openai_batch_requests`](https://shmercer.github.io/pairwiseLLM/reference/build_openai_batch_requests.md)
(or a compatible table) and writes one JSON object per line, in the
format expected by the OpenAI batch API.

## Usage

``` r
write_openai_batch_file(batch_tbl, path)
```

## Arguments

- batch_tbl:

  A data frame or tibble, typically the result of
  [`build_openai_batch_requests`](https://shmercer.github.io/pairwiseLLM/reference/build_openai_batch_requests.md).

- path:

  File path where the JSONL file should be written.

## Value

Invisibly returns `path`.

## Details

The input can either:

- Already contain a character column `jsonl` (one JSON string per row),
  in which case that column is used directly, or

- Contain the columns `custom_id`, `method`, `url`, and `body`, in which
  case the JSON strings are constructed automatically.

## Examples

``` r
if (FALSE) { # \dontrun{
data("example_writing_samples")
pairs_all <- make_pairs(example_writing_samples)
pairs_small <- sample_pairs(pairs_all, n_pairs = 5, seed = 1)

td <- trait_description("overall_quality")
tmpl <- set_prompt_template()

batch_tbl <- build_openai_batch_requests(
  pairs             = pairs_small,
  model             = "gpt-4.1",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl
)

write_openai_batch_file(batch_tbl, "batch_forward.jsonl")
} # }
```
