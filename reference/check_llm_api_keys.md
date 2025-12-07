# Check configured API keys for LLM backends

This function inspects the current R session for configured API keys
used by pairwiseLLM. It checks for known environment variables such as
`OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, and `GEMINI_API_KEY`, and returns
a small tibble summarising which keys are available.

## Usage

``` r
check_llm_api_keys(verbose = TRUE)
```

## Arguments

- verbose:

  Logical; if `TRUE` (default), prints a human-readable summary to the
  console describing which keys are set and how to configure missing
  ones.

## Value

A tibble (data frame) with one row per backend and columns:

- backend:

  Short backend identifier, e.g. `"openai"`, `"anthropic"`, `"gemini"`,
  `"together"`.

- service:

  Human-readable service name, e.g. `"OpenAI"`, `"Anthropic"`,
  `"Google Gemini"`, `"Together.ai"`.

- env_var:

  Name of the environment variable that is checked.

- has_key:

  Logical flag indicating whether the key is set and non-empty.

## Details

It does **not** print or return the key values themselves - only whether
each key is present. This makes it safe to run in logs, scripts, and
shared environments.

## Examples

``` r
if (FALSE) { # \dontrun{
# In an interactive session, quickly check which keys are configured:
check_llm_api_keys()

# In non-interactive scripts, you can disable messages and just use the
# result:
status <- check_llm_api_keys(verbose = FALSE)
status
} # }
```
