# Internal: Together.ai API key helper

This is a thin wrapper around `.get_api_key()` for the Together.ai
backend. It looks for a `TOGETHER_API_KEY` environment variable by
default and can be overridden explicitly via the `api_key` argument.

## Usage

``` r
.together_api_key(api_key = NULL)
```

## Arguments

- api_key:

  Optional character scalar. If `NULL` or an empty string, the helper
  falls back to `Sys.getenv("TOGETHER_API_KEY")`.
