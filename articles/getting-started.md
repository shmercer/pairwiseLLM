# Getting Started with pairwiseLLM

## 1. Introduction

`pairwiseLLM` provides a unified workflow for generating and analyzing
**pairwise comparisons of writing quality** using LLM APIs (OpenAI,
Anthropic, Gemini, Together), and local models via Ollama..

A typical workflow:

1.  Select writing samples  
2.  Construct pairwise comparison sets  
3.  Submit comparisons to an LLM (live or batch API)  
4.  Parse model outputs  
5.  Fit Bradley–Terry or Elo models to obtain latent writing-quality
    scores

For prompt evaluation and positional-bias diagnostics, see:

- [`vignette("prompt-template-bias")`](https://shmercer.github.io/pairwiseLLM/articles/prompt-template-bias.html)

For advanced batch processing workflows, see:

- [`vignette("advanced-batch-workflows")`](https://shmercer.github.io/pairwiseLLM/articles/advanced-batch-workflows.html)

------------------------------------------------------------------------

## 2. Setting API Keys

`pairwiseLLM` reads provider keys **only from environment variables**,
never from R options or global variables.

| Provider                                    | Environment Variable |
|---------------------------------------------|----------------------|
| [OpenAI](https://openai.com/api/)           | OPENAI_API_KEY       |
| [Anthropic](https://console.anthropic.com/) | ANTHROPIC_API_KEY    |
| [Gemini](https://aistudio.google.com/)      | GEMINI_API_KEY       |
| [Together](https://www.together.ai/)        | TOGETHER_API_KEY     |

You should put these in your `~/.Renviron`:

    OPENAI_API_KEY="sk-..."
    ANTHROPIC_API_KEY="..."
    GEMINI_API_KEY="..."
    TOGETHER_API_KEY="..."

Check which keys are available:

    library(pairwiseLLM)

    check_llm_api_keys()
    #> All known LLM API keys are set: OPENAI_API_KEY, ANTHROPIC_API_KEY, GEMINI_API_KEY, TOGETHER_API_KEY.
    #> # A tibble: 4 × 4
    #>   backend   service        env_var           has_key
    #> 1 openai    OpenAI         OPENAI_API_KEY    TRUE
    #> 2 anthropic Anthropic      ANTHROPIC_API_KEY TRUE
    #> 3 gemini    Google Gemini  GEMINI_API_KEY    TRUE
    #> 4 together  Together.ai    TOGETHER_API_KEY  TRUE

[Ollama](https://ollama.com/) runs locally and does not require an API
key, just that the Ollama server is running.

------------------------------------------------------------------------

## 3. Example Writing Data

The package ships with 20 simulated student writing samples with clear
differences in quality:

``` r
data("example_writing_samples", package = "pairwiseLLM")
dplyr::slice_head(example_writing_samples, n = 3)
#> # A tibble: 3 × 3
#>   ID    text                                                       quality_score
#>   <chr> <chr>                                                              <int>
#> 1 S01   "Writing assessment is hard. People write different thing…             1
#> 2 S02   "It is hard to grade writing. Some are long and some are …             2
#> 3 S03   "Assessing writing is difficult because everyone writes d…             3
```

Each sample has:

- `ID`  
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
#> 1 S01   "Writing assessment is hard. People write different things.… S02   "It …
#> 2 S01   "Writing assessment is hard. People write different things.… S03   "Ass…
#> 3 S01   "Writing assessment is hard. People write different things.… S04   "Gra…
#> 4 S01   "Writing assessment is hard. People write different things.… S05   "Wri…
#> 5 S01   "Writing assessment is hard. People write different things.… S06   "It …
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
#> [1] "Overall quality of the writing, considering how well ideas are expressed,\n      how clearly the writing is organized, and how effective the language and\n      conventions are."
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

Placeholders required in custom prompt templates:

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

The unified wrapper works for **OpenAI, Anthropic, Gemini, Together, and
Ollama.**

It supports **parallel processing** and **incremental output file
saving** (resume capability) for **all** supported backends. The
function returns a list containing:

- `$results`: observed outcomes only (canonical schema)
- `$failed_pairs`: scheduled pairs with no observed outcome
- `$failed_attempts`: attempt-level failures (retries, timeouts, parse
  errors, invalid winners)

``` r
# Example using parallel processing and incremental saving
res_list <- submit_llm_pairs(
  pairs             = pairs_small,
  backend           = "openai", # also "anthropic", "gemini", "together"
  model             = "gpt-4o",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  # New features:
  parallel          = TRUE,
  workers           = 4,
  save_path         = "live_results.csv"
)
```

Preview results:

``` r
# Successes are in the $results tibble
dplyr::slice_head(res_list$results, 5)

# Failures (if any) are in $failed_pairs
if (nrow(res_list$failed_pairs) > 0) {
  print(res_list$failed_pairs)
}

# Attempt-level failures (if any) are in $failed_attempts
if (nrow(res_list$failed_attempts) > 0) {
  print(res_list$failed_attempts)
}
```

Each row in `$results` includes: - `custom_id` (uses `pair_uid` if
supplied; otherwise defaults to `LIVE_<ID1>_vs_<ID2>`) - `ID1`, `ID2` -
parsed `<BETTER_SAMPLE>` tag → `better_sample` and `better_id` -
canonical aliases/keys: `A_id`, `B_id`, `winner_pos`, `ordered_key`,
`unordered_key`, `pair_uid`, `received_at`, `backend`, `model` -
thoughts (reasoning text, if available) and content (final answer)

------------------------------------------------------------------------

## 7. Preparing Data for BT or Elo Modeling

Convert the LLM output (specifically the `$results` tibble for
[`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md)
output) to a 3-column BT dataset:

``` r
# res_list: output list from submit_llm_pairs()
# We extract the $results tibble for modeling
bt_data <- build_bt_data(res_list$results)
dplyr::slice_head(bt_data, 5)
```

and/or a dataset for Elo modeling:

``` r
# res_list: output from submit_llm_pairs()
elo_data <- build_elo_data(res_list$results)
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
- reliability (when using `sirt` engine)

------------------------------------------------------------------------

## 9. Elo Modeling

``` r
elo_fit <- fit_elo_model(elo_data, runs = 5)
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
  backend            = "openai",
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

### 10.3 Multi‑Batch Jobs

In addition to the standard batch helpers, you can split a large job
into multiple segments using
[`llm_submit_pairs_multi_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_multi_batch.md)
and then poll all of them with
[`llm_resume_multi_batches()`](https://shmercer.github.io/pairwiseLLM/reference/llm_resume_multi_batches.md).
This is particularly useful when you have many pairs or want to ensure
that you can resume if the session ends.

``` r
# Generate a small set of pairs
pairs_small <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 10, seed = 4321) |>
  randomize_pair_order(seed = 8765)

td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

# Split into two batches and include reasoning/chain-of-thought
multi_job <- llm_submit_pairs_multi_batch(
  pairs             = pairs_small,
  backend           = "openai",
  model             = "gpt-5.1",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  n_segments        = 2,
  output_dir        = "myjob",
  write_registry    = TRUE,
  include_thoughts  = TRUE
)

# Poll and merge results.  Combined results are written to
# "myjob/combined_results.csv" or the directory you specify.
res <- llm_resume_multi_batches(
  jobs               = multi_job$jobs,
  interval_seconds   = 30,
  write_combined_csv = TRUE
)

head(res$combined)
```

### 10.4 Estimating cost before you run

For large jobs, it is often useful to estimate token usage and cost
before launching a live run or submitting a batch. `pairwiseLLM`
includes
[`estimate_llm_pairs_cost()`](https://shmercer.github.io/pairwiseLLM/reference/estimate_llm_pairs_cost.md),
which runs a small **pilot** (paid live calls) and then estimates the
rest of the job by calibrating input tokens from prompt byte length.

The output includes both:

- **Expected cost** (using mean output tokens from the pilot)
- **Budget cost** (using a high quantile of pilot output tokens,
  controlled by `budget_quantile`)

If you are running a discounted batch workflow, set `mode = "batch"` and
supply a `batch_discount` multiplier.

``` r
# Create a moderate set of pairs
pairs_big <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 200, seed = 123) |>
  randomize_pair_order(seed = 456)

td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

est <- estimate_llm_pairs_cost(
  pairs = pairs_big,
  backend = "anthropic",                # "openai", "anthropic", "gemini", "together"
  model = "claude-sonnet-4-5",
  trait_name = td$name,
  trait_description = td$description,
  prompt_template = tmpl,
  mode = "batch",
  batch_discount = 0.5,                 # set to 1 for no discount
  n_test = 10,                          # paid pilot calls (live)
  budget_quantile = 0.9,                # p90 output tokens
  cost_per_million_input = 3.0,         # fill in your provider pricing
  cost_per_million_output = 15.0
)

est$summary
```

Avoid paying twice: reuse pilot results

The estimator returns both the pilot output and the pairs not included
in the pilot (remaining_pairs). Use remaining_pairs to submit only the
remaining work after you are satisfied with the estimate:

``` r
remaining_pairs <- est$remaining_pairs

# Example: submit only the remaining pairs as a batch

batch <- llm_submit_pairs_batch(
          backend = "anthropic",
          model = "claude-sonnet-4-5",
          pairs = remaining_pairs,
          trait_name = td$name,
          trait_description = td$description,
          prompt_template = tmpl)

results <- llm_download_batch_results(batch)
```

Notes:

- The estimator does not require a provider tokenizer; it uses prompt
  byte length calibrated on the pilot.
- Ollama is not supported in the estimator (local models do not incur
  token costs).
- Reasoning/thinking tokens are treated as output tokens for pricing.

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

#### 11.4 Together.ai (live only)

- [`together_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/together_compare_pair_live.md)
- [`submit_together_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_together_pairs_live.md)

#### 11.5 Ollama (local, live only)

- [`ollama_compare_pair_live()`](https://shmercer.github.io/pairwiseLLM/reference/ollama_compare_pair_live.md)
- [`submit_ollama_pairs_live()`](https://shmercer.github.io/pairwiseLLM/reference/submit_ollama_pairs_live.md)

------------------------------------------------------------------------

## 12. Troubleshooting

#### Missing API keys

``` r
check_llm_api_keys()
#> No LLM API keys are currently set for known backends:
#>   - OpenAI:         OPENAI_API_KEY
#>   - Anthropic:      ANTHROPIC_API_KEY
#>   - Google Gemini:  GEMINI_API_KEY
#>   - Together.ai:    TOGETHER_API_KEY
#> 
#> Use `usethis::edit_r_environ()` to add the keys persistently, e.g.:
#>   OPENAI_API_KEY    = "YOUR_OPENAI_KEY_HERE"
#>   ANTHROPIC_API_KEY = "YOUR_ANTHROPIC_KEY_HERE"
#>   GEMINI_API_KEY    = "YOUR_GEMINI_KEY_HERE"
#>   TOGETHER_API_KEY  = "YOUR_TOGETHER_KEY_HERE"
#> # A tibble: 4 × 4
#>   backend   service       env_var           has_key
#>   <chr>     <chr>         <chr>             <lgl>  
#> 1 openai    OpenAI        OPENAI_API_KEY    FALSE  
#> 2 anthropic Anthropic     ANTHROPIC_API_KEY FALSE  
#> 3 gemini    Google Gemini GEMINI_API_KEY    FALSE  
#> 4 together  Together.ai   TOGETHER_API_KEY  FALSE
```

#### Timeouts

Use batch APIs for \>40 pairs. Split a large job into multiple segments
using
[`llm_submit_pairs_multi_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_multi_batch.md)
and then poll/download all of them with
[`llm_resume_multi_batches()`](https://shmercer.github.io/pairwiseLLM/reference/llm_resume_multi_batches.md)

#### Positional bias

Use
[`compute_reverse_consistency()`](https://shmercer.github.io/pairwiseLLM/reference/compute_reverse_consistency.md) +
[`check_positional_bias()`](https://shmercer.github.io/pairwiseLLM/reference/check_positional_bias.md)
(see
[vignette(“prompt-template-bias”)](https://shmercer.github.io/pairwiseLLM/articles/prompt-template-bias.html)
for a full example).

------------------------------------------------------------------------

## 13. Citation

> Mercer, S. (2025). *Getting started with pairwiseLLM* \[R package
> vignette\]. In *pairwiseLLM: Pairwise comparison tools for large
> language model-based writing evaluation*.
> <https://doi.org/10.32614/CRAN.package.pairwiseLLM>
