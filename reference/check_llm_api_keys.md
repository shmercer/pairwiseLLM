# Check configured API keys for LLM backends

This function inspects the current R session for configured API keys
used by pairwiseLLM. It checks for known environment variables such as
`OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, `GEMINI_API_KEY`, and
`VERTEX_API_KEY`, and returns a small tibble summarising which keys are
available.

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
  `"vertex"`, `"together"`.

- service:

  Human-readable service name, e.g. `"OpenAI"`, `"Anthropic"`,
  `"Google Gemini"`, `"Vertex AI Gemini API"`, `"Together.ai"`.

- env_var:

  Name of the environment variable that is checked.

- has_key:

  Logical flag indicating whether the key is set and non-empty.

## Details

It does **not** print or return the key values themselves - only whether
each key is present. This makes it safe to run in logs, scripts, and
shared environments.

## See also

[`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md),
[`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md)

Other live backends:
[`anthropic_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/anthropic_compare_pair_live.md),
[`gemini_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/gemini_compare_pair_live.md),
[`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md),
[`ollama_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/ollama_compare_pair_live.md),
[`openai_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/openai_compare_pair_live.md),
[`submit_anthropic_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_anthropic_pairs_live.md),
[`submit_gemini_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_gemini_pairs_live.md),
[`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md),
[`submit_ollama_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_ollama_pairs_live.md),
[`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md),
[`submit_together_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_together_pairs_live.md),
[`submit_vertex_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_vertex_pairs_live.md),
[`together_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/together_compare_pair_live.md),
[`vertex_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/vertex_compare_pair_live.md)

## Examples

``` r
# In an interactive session, quickly check which keys are configured:
check_llm_api_keys()
#> No LLM API keys are currently set for known backends:
#>   - OpenAI:         OPENAI_API_KEY
#>   - Anthropic:      ANTHROPIC_API_KEY
#>   - Google Gemini:  GEMINI_API_KEY
#>   - Vertex AI:      VERTEX_API_KEY
#>   - Together.ai:    TOGETHER_API_KEY
#> 
#> Use `usethis::edit_r_environ()` to add the keys persistently, e.g.:
#>   OPENAI_API_KEY    = "YOUR_OPENAI_KEY_HERE"
#>   ANTHROPIC_API_KEY = "YOUR_ANTHROPIC_KEY_HERE"
#>   GEMINI_API_KEY    = "YOUR_GEMINI_KEY_HERE"
#>   VERTEX_API_KEY    = "YOUR_VERTEX_KEY_HERE"
#>   TOGETHER_API_KEY  = "YOUR_TOGETHER_KEY_HERE"
#> # A tibble: 5 × 4
#>   backend   service              env_var           has_key
#>   <chr>     <chr>                <chr>             <lgl>  
#> 1 openai    OpenAI               OPENAI_API_KEY    FALSE  
#> 2 anthropic Anthropic            ANTHROPIC_API_KEY FALSE  
#> 3 gemini    Google Gemini        GEMINI_API_KEY    FALSE  
#> 4 vertex    Vertex AI Gemini API VERTEX_API_KEY    FALSE  
#> 5 together  Together.ai          TOGETHER_API_KEY  FALSE  

# In non-interactive scripts, you can disable messages and just use the
# result:
status <- check_llm_api_keys(verbose = FALSE)
status
#> # A tibble: 5 × 4
#>   backend   service              env_var           has_key
#>   <chr>     <chr>                <chr>             <lgl>  
#> 1 openai    OpenAI               OPENAI_API_KEY    FALSE  
#> 2 anthropic Anthropic            ANTHROPIC_API_KEY FALSE  
#> 3 gemini    Google Gemini        GEMINI_API_KEY    FALSE  
#> 4 vertex    Vertex AI Gemini API VERTEX_API_KEY    FALSE  
#> 5 together  Together.ai          TOGETHER_API_KEY  FALSE  
```
