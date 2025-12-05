# Internal: Google Gemini API key helper

This is a thin wrapper around `.get_api_key()` for the Google Gemini
backend. It looks for a `GEMINI_API_KEY` environment variable by default
and can be overridden explicitly via the `api_key` argument.

## Usage

``` r
.gemini_api_key(api_key = NULL)
```

## Arguments

- api_key:

  Optional character scalar. If `NULL` or an empty string, the helper
  falls back to `Sys.getenv("GEMINI_API_KEY")`.
