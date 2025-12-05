# Getting Started with pairwiseLLM

## 1. Introduction

`pairwiseLLM` provides a unified workflow for generating and analyzing
**pairwise comparisons of writing quality** using modern LLM APIs
(OpenAI, Anthropic, Gemini, and soon local models).

A typical workflow:

1.  Select writing samples  
2.  Construct pairwise comparison sets  
3.  Submit comparisons to an LLM (live or batch API)  
4.  Parse model outputs  
5.  Fit Bradley–Terry or Elo models to obtain latent writing-quality
    scores

For prompt evaluation and positional-bias diagnostics, see the companion
vignette:

👉
[`vignette("prompt-template-positional-bias")`](https://shmercer.github.io/pairwiseLLM/articles/prompt-template-positional-bias.md)

------------------------------------------------------------------------

## 2. Setting API Keys

`pairwiseLLM` reads provider keys **only from environment variables**,
never from R options or global variables.

| Provider  | Environment Variable |
|-----------|----------------------|
| OpenAI    | `OPENAI_API_KEY`     |
| Anthropic | `ANTHROPIC_API_KEY`  |
| Gemini    | `GEMINI_API_KEY`     |

You should put these in your `~/.Renviron`:

    OPENAI_API_KEY=sk-...
    ANTHROPIC_API_KEY=...
    GEMINI_API_KEY=...

Check which keys are available:

``` r
library(pairwiseLLM)
check_llm_api_keys()
#> No LLM API keys are currently set for known backends:
#>   - OpenAI:         OPENAI_API_KEY
#>   - Anthropic:      ANTHROPIC_API_KEY
#>   - Google Gemini:  GEMINI_API_KEY
#> 
#> Use `usethis::edit_r_environ()` to add the keys persistently, e.g.:
#>   OPENAI_API_KEY   = "YOUR_OPENAI_KEY_HERE"
#>   ANTHROPIC_API_KEY = "YOUR_ANTHROPIC_KEY_HERE"
#>   GEMINI_API_KEY    = "YOUR_GEMINI_KEY_HERE"
#> # A tibble: 3 × 4
#>   backend   service       env_var           has_key
#>   <chr>     <chr>         <chr>             <lgl>  
#> 1 openai    OpenAI        OPENAI_API_KEY    FALSE  
#> 2 anthropic Anthropic     ANTHROPIC_API_KEY FALSE  
#> 3 gemini    Google Gemini GEMINI_API_KEY    FALSE
```

------------------------------------------------------------------------

## 3. Example Writing Data

The package ships with 20 authentic student writing samples:

``` r
data("example_writing_samples", package = "pairwiseLLM")
dplyr::slice_head(example_writing_samples, n = 3)
#> # A tibble: 3 × 3
#>   ID    text                                                       quality_score
#>   <chr> <chr>                                                              <int>
#> 1 S01   Writing assessment is hard. People write different things…             1
#> 2 S02   It is hard to grade writing. Some are long and some are s…             2
#> 3 S03   Assessing writing is difficult because everyone writes di…             3
```

Each sample has:

- `id`  
- `text`

------------------------------------------------------------------------

## 4. Constructing Pairwise Comparisons

Create all unordered pairs:

``` r
pairs <- example_writing_samples |>
  make_pairs()

dplyr::slice_head(pairs, n = 5)
#> # A tibble: 5 × 4
#>   ID1   text1                                                        ID2   text2
#>   <chr> <chr>                                                        <chr> <chr>
#> 1 S01   Writing assessment is hard. People write different things. … S02   It i…
#> 2 S01   Writing assessment is hard. People write different things. … S03   Asse…
#> 3 S01   Writing assessment is hard. People write different things. … S04   Grad…
#> 4 S01   Writing assessment is hard. People write different things. … S05   Writ…
#> 5 S01   Writing assessment is hard. People write different things. … S06   It i…
```

Sample a subset of pairs:

``` r
pairs_small <- sample_pairs(pairs, n_pairs = 10, seed = 123)
```

Randomize SAMPLE_1 / SAMPLE_2 order:

``` r
pairs_small <- randomize_pair_order(pairs_small, seed = 99)
```

------------------------------------------------------------------------

## 5. Traits and Prompt Templates

### 5.1 Using a built-in trait

``` r
td <- trait_description("overall_quality")
td
#> $name
#> [1] "Overall Quality"
#> 
#> $description
#> [1] "Overall quality of the writing, considering how well ideas are expressed, how clearly the writing is organized, and how effective the language and conventions are."
```

Or define your own:

``` r
td_custom <- trait_description(
  custom_name = "Clarity",
  custom_description = "How clearly and effectively ideas are expressed."
)
```

### 5.2 Using or customizing prompt templates

Load default prompt:

``` r
tmpl <- set_prompt_template()
cat(substr(tmpl, 1, 300))
#> You are a debate adjudicator. Your task is to weigh the comparative strengths of two writing samples regarding a specific trait.
#> 
#> TRAIT: {TRAIT_NAME}
#> DEFINITION: {TRAIT_DESCRIPTION}
#> 
#> SAMPLES:
#> 
#> === SAMPLE_1 ===
#> {SAMPLE_1}
#> 
#> === SAMPLE_2 ===
#> {SAMPLE_2}
#> 
#> EVALUATION PROCESS (Mental Simulation):
#> 
#> 1.  **Ad
```

Placeholders required:

- `{TRAIT_NAME}`
- `{TRAIT_DESCRIPTION}`
- `{SAMPLE_1}`
- `{SAMPLE_2}`

Load a template from file:

``` r
set_prompt_template(file = "my_template.txt")
```

------------------------------------------------------------------------

## 6. Live Pairwise Comparisons

The unified wrapper works for **OpenAI, Anthropic, and Gemini**.

``` r
res_live <- submit_llm_pairs(
  pairs             = pairs_small,
  backend           = "openai",   # also "anthropic" or "gemini"
  model             = "gpt-4o",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl
)
```

Preview results:

``` r
dplyr::slice_head(res_live, 5)
```

Each row includes:

- `pair_id`
- `sample1_id`, `sample2_id`
- parsed `<BETTER_SAMPLE>` tag → `better_sample` and `better_id`
- (optionally) raw model output

------------------------------------------------------------------------

## 7. Preparing Data for BT or Elo Modeling

Convert LLM output to a 3-column BT dataset:

``` r
bt_data <- build_bt_data(res_live)
dplyr::slice_head(bt_data, 5)
```

------------------------------------------------------------------------

## 8. Bradley–Terry Modeling

Fit model:

``` r
bt_fit <- fit_bt_model(bt_data)
```

Summarize results:

``` r
summarize_bt_fit(bt_fit)
```

The output includes:

- latent θ ability scores  
- SEs  
- reliability (sirt engine)

------------------------------------------------------------------------

## 9. Elo Modeling (Optional)

Requires **EloChoice** (in Suggests):

``` r
elo_fit <- fit_elo_model(bt_data, runs = 5)
elo_fit
```

Outputs:

- Elo ratings for each sample  
- unweighted and weighted reliability  
- trial counts

------------------------------------------------------------------------

## 10. Batch APIs (Large Jobs)

### 10.1 Submit a batch

``` r
batch <- llm_submit_pairs_batch(
  provider           = "openai",
  model              = "gpt-4o",
  pairs              = pairs_small,
  trait_name         = td$name,
  trait_description  = td$description,
  prompt_template    = tmpl
)
```

### 10.2 Download results

``` r
res_batch <- llm_download_batch_results(batch)
head(res_batch)
```

------------------------------------------------------------------------

## 11. Backend-Specific Tools

Most users use the unified interface, but backend helpers are available.

#### 11.1 OpenAI

- [`submit_openai_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_openai_pairs_live.md)
- [`build_openai_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_openai_batch_requests.md)
- [`run_openai_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_openai_batch_pipeline.md)
- [`parse_openai_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/parse_openai_batch_output.md)

#### 11.2 Anthropic

- [`submit_anthropic_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_anthropic_pairs_live.md)
- [`build_anthropic_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_anthropic_batch_requests.md)
- [`run_anthropic_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_anthropic_batch_pipeline.md)
- [`parse_anthropic_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/parse_anthropic_batch_output.md)

#### 11.3 Google Gemini

- [`submit_gemini_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_gemini_pairs_live.md)
- [`build_gemini_batch_requests()`](https://shmercer.github.io/pairwiseLLM/reference/build_gemini_batch_requests.md)
- [`run_gemini_batch_pipeline()`](https://shmercer.github.io/pairwiseLLM/reference/run_gemini_batch_pipeline.md)
- [`parse_gemini_batch_output()`](https://shmercer.github.io/pairwiseLLM/reference/parse_gemini_batch_output.md)

------------------------------------------------------------------------

## 12. Troubleshooting

#### Missing API keys

``` r
check_llm_api_keys()
#> No LLM API keys are currently set for known backends:
#>   - OpenAI:         OPENAI_API_KEY
#>   - Anthropic:      ANTHROPIC_API_KEY
#>   - Google Gemini:  GEMINI_API_KEY
#> 
#> Use `usethis::edit_r_environ()` to add the keys persistently, e.g.:
#>   OPENAI_API_KEY   = "YOUR_OPENAI_KEY_HERE"
#>   ANTHROPIC_API_KEY = "YOUR_ANTHROPIC_KEY_HERE"
#>   GEMINI_API_KEY    = "YOUR_GEMINI_KEY_HERE"
#> # A tibble: 3 × 4
#>   backend   service       env_var           has_key
#>   <chr>     <chr>         <chr>             <lgl>  
#> 1 openai    OpenAI        OPENAI_API_KEY    FALSE  
#> 2 anthropic Anthropic     ANTHROPIC_API_KEY FALSE  
#> 3 gemini    Google Gemini GEMINI_API_KEY    FALSE
```

#### Getting chain-of-thought leakage

Use the default template or set `include_thoughts = FALSE`.

#### Timeouts

Use batch APIs for \>40 pairs.

#### Positional bias

Use `test_positional_bias()` or see the bias-testing vignette.

------------------------------------------------------------------------

## 13. Further Reading

To evaluate positional bias across models and templates:

``` r
vignette("prompt-template-positional-bias")
```

To explore modeling:

``` r
?fit_bt_model
?fit_elo_model
```

------------------------------------------------------------------------

## 14. Citation

> Mercer, S. H. (2025). *pairwiseLLM: Pairwise comparisons of writing
> quality with LLMs* (R package).  
> GitHub: <https://github.com/shmercer/pairwiseLLM>
