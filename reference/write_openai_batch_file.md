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
# Construct a minimal batch request tibble
requests <- tibble::tibble(
  custom_id = c("req1", "req2"),
  method = "POST",
  url = "/v1/chat/completions",
  body = list(
    list(
      model = "gpt-4o-mini",
      messages = list(
        list(role = "user", content = "Hello")
      )
    ),
    list(
      model = "gpt-4o-mini",
      messages = list(
        list(role = "user", content = "Goodbye")
      )
    )
  )
)

# Write to a temporary JSONL file
path <- tempfile(fileext = ".jsonl")
write_openai_batch_file(requests, path)

# Inspect the file contents
readLines(path)
#> [1] "{\"custom_id\":\"req1\",\"method\":\"POST\",\"url\":\"/v1/chat/completions\",\"body\":{\"model\":\"gpt-4o-mini\",\"messages\":[{\"role\":\"user\",\"content\":\"Hello\"}]}}"  
#> [2] "{\"custom_id\":\"req2\",\"method\":\"POST\",\"url\":\"/v1/chat/completions\",\"body\":{\"model\":\"gpt-4o-mini\",\"messages\":[{\"role\":\"user\",\"content\":\"Goodbye\"}]}}"
```
