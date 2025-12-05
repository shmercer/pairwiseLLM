# Internal: parse a Gemini GenerateContentResponse into the standard tibble row

For batch responses, Gemini 3 Pro currently typically returns:

- `candidates[[1]]$content$parts[[1]]$text` = final answer

- `candidates[[1]]$content$parts[[1]]$thoughtSignature` = opaque
  signature

- `usageMetadata$thoughtsTokenCount` = hidden reasoning tokens

## Usage

``` r
.parse_gemini_pair_response(
  custom_id,
  ID1,
  ID2,
  response,
  include_thoughts = FALSE
)
```

## Details

When `include_thoughts = TRUE` and \>= 2 parts are present, we mirror
the live behavior: first part = `thoughts`, remaining parts = `content`.
When only one part is present, we treat it as `content` and leave
`thoughts` as NA (batch isn't returning visible thoughts text).
