pairwiseLLM: Pairwise comparisons of writing quality with LLMs
================

<!-- badges: start -->

[![R-CMD-check](https://github.com/shmercer/pairwiseLLM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/shmercer/pairwiseLLM/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/shmercer/pairwiseLLM/branch/main/graph/badge.svg)](https://app.codecov.io/gh/shmercer/pairwiseLLM)
[![CRAN
status](https://www.r-pkg.org/badges/version/pairwiseLLM)](https://CRAN.R-project.org/package=pairwiseLLM)
<!-- badges: end -->

`pairwiseLLM` is an R package for performing **pairwise comparisons of
writing quality** using large language models (LLMs).

It provides a unified interface for comparing writing samples across
OpenAI, Anthropic, Gemini, and (planned) local models via **Ollama**,
with support for both **live** and **batch** pipelines.

The package is designed to simplify generating measures of writing
quality from pairwise comparisons with LLMs.

## Key features

### Multi-provider support (live)

- **OpenAI** — Chat Completions + Responses API  
- **Anthropic** — Messages API with extended thinking  
- **Gemini** — Gemini 3 Pro  
- **Ollama (planned)** — Local, offline models (e.g., llama3, mistral,
  qwen)

All live backends use the **same unified schema**.

### Batch processing

Fully implemented for:

| Provider  | Batch support | Functions                          |
|-----------|---------------|------------------------------------|
| OpenAI    | ✅ Yes        | `run_openai_batch_pipeline()`      |
| Gemini    | ✅ Yes        | `run_gemini_batch_pipeline()`      |
| Anthropic | ✅ Yes        | `run_anthropic_batch_pipeline()`   |
| Ollama    | ❌ No         | Not applicable (live-only planned) |

A **unified batch front-end** is planned:

- `llm_submit_pairs_batch()`
- `llm_download_batch_results()`

### Bias-minimized prompting system

`pairwiseLLM` uses XML-like structured prompts that minimize
first-position bias:

- `<SAMPLE_1>` … `</SAMPLE_1>`
- `<SAMPLE_2>` … `</SAMPLE_2>`
- `<BETTER_SAMPLE>` … `</BETTER_SAMPLE>`

The current default template was crafted to minimize positional bias and
has been **empirically tested** on:

- OpenAI GPT-5.1
- Anthropic Claude 4.5 Sonnet
- Gemini 3 Pro

### Pair generation & positional bias tools

Included helper functions:

- `make_pairs()` — generate all possible ordered pairs  
- `sample_pairs()` — select a subset of pairs  
- `reverse_pairs()` — generate reversed-order pairs  
- `randomize_pair_order()` — randomize sample positions  
- `test_positional_bias()` — quantify bias toward the first-presented
  sample

These allow you to empirically test whether any model systematically
prefers the first or second sample.

### Unified output schema

All providers return the same tibble structure, with columns such as:

- `custom_id` — e.g. `"OPENAI_S01_vs_S02"` or `"ANTH_S01_vs_S02"`  
- `ID1`, `ID2` — sample IDs  
- `model` — model used (e.g. `"gpt-5.1"`, `"claude-sonnet-4-5"`)  
- `object_type` — provider object type (e.g. `"message"`)  
- `status_code` — HTTP-style status (e.g. `200`, or `NA` for
  non-succeeded batch lines)  
- `result_type` — `"succeeded"`, `"errored"`, `"canceled"`, `"expired"`
  (batch only)  
- `error_message` — normalized error message if present  
- `thoughts` — extracted reasoning / thinking (when provided)  
- `content` — visible assistant text  
- `better_sample` — `"SAMPLE_1"`, `"SAMPLE_2"`, or `NA`  
- `better_id` — `ID1` or `ID2`, matching `better_sample`  
- `prompt_tokens`, `completion_tokens`, `total_tokens`  
- `raw_response` — optional list-column with parsed JSON

This makes it easy to plug results into Bradley–Terry, Elo, or
rubric-based scoring models.

## Installation

At present, `pairwiseLLM` is not yet on CRAN. You can install the
development version from GitHub:

``` r
# install.packages("pak")
pak::pak("shmercer/pairwiseLLM")
```

Set provider API keys via environment variables (e.g. in `~/.Renviron`):

``` bash
export OPENAI_API_KEY="your-openai-key"
export ANTHROPIC_API_KEY="your-anthropic-key"
export GEMINI_API_KEY="your-gemini-key"
```

Restart R after editing `~/.Renviron`.

## Quickstart

### Load package and example data

`pairwiseLLM` ships with example writing samples:

``` r
library(pairwiseLLM)

data("example_writing_samples", package = "pairwiseLLM")
head(example_writing_samples)
```

Assume `example_writing_samples` has at least an `ID` and `text` column.

### Create pairs of writing samples

We can create randomized pairs for testing:

``` r
pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 3, seed = 123) |>
  randomize_pair_order(seed = 999)

pairs
```

### Live comparison (single pair)

Here we show a single comparison using OpenAI. This requires
`OPENAI_API_KEY` and network access.

``` r
td   <- trait_description("overall_quality")
tmpl <- set_prompt_template()

res <- llm_compare_pair(
  ID1              = pairs$ID1[1],
  text1            = pairs$text1[1],
  ID2              = pairs$ID2[1],
  text2            = pairs$text2[1],
  backend          = "openai",
  model            = "gpt-5.1",
  trait_name       = td$name,
  trait_description= td$description,
  prompt_template  = tmpl
)

res
```

### Live comparison (many pairs)

Using Anthropic as the backend:

``` r
live_tbl <- submit_llm_pairs(
  pairs             = pairs,
  backend           = "anthropic",
  model             = "claude-4-5-sonnet",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  reasoning         = "none"  # or "enabled" for extended thinking
)

live_tbl
```

### Batch example (OpenAI)

Batch pipelines are useful for large-scale scoring:

``` r
pipeline <- run_openai_batch_pipeline(
  pairs             = pairs,
  model             = "gpt-5.1",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl,
  reasoning         = "none",
  interval_seconds  = 30,
  timeout_seconds   = 3600,
  verbose           = TRUE
)

batch_results <- pipeline$results
batch_results
```

Similar pipelines exist for:

- `run_anthropic_batch_pipeline()`
- `run_gemini_batch_pipeline()`

A unified batch front-end (`llm_submit_pairs_batch()`,
`llm_download_batch_results()`) is planned so that users can submit
batches without worrying about provider-specific details.

### Positional bias testing

`pairwiseLLM` includes utilities for probing positional bias. Assuming
`live_tbl` is a tibble of live results containing `better_sample` and
randomized ordering:

``` r
bias_summary <- test_positional_bias(live_tbl)
bias_summary
```

You can also explicitly generate reversed-order pairs:

``` r
rev_pairs <- reverse_pairs(pairs)
rev_pairs
```

These can be re-submitted to the same model to see how often the
preferred sample changes when the positions are swapped.

## Local models via Ollama (planned)

Ollama integration will allow you to use local models without sending
data to external providers. The planned design is:

``` r
submit_llm_pairs(
  pairs   = pairs,
  backend = "ollama",
  model   = "llama3"
)
```

Features:

- No external API keys required  
- Fully local inference (depends on your Ollama installation)  
- Same unified tibble schema  
- Ideal for large-scale offline experiments and sensitive text

## Architecture overview

### Live backends

Live backends are implemented in separate files:

- `R/openai_live.R`
- `R/anthropic_live.R`
- `R/gemini_live.R`

A dispatcher in `R/llm_backends.R` exposes:

- `llm_compare_pair()` — compare a single pair  
- `submit_llm_pairs()` — compare many pairs in one call

All backends:

- Accept a tibble/data frame with `ID1`, `text1`, `ID2`, `text2`  
- Use provider-specific prompt templates & response parsing  
- Return a unified tibble with standard columns

### Batch backends

Each provider has its own batch pipeline:

- **OpenAI**
  - `build_openai_batch_requests()`
  - `run_openai_batch_pipeline()`
  - `parse_openai_batch_output()`
- **Anthropic**
  - `build_anthropic_batch_requests()`
  - `anthropic_create_batch()`, `anthropic_get_batch()`
  - `anthropic_poll_batch_until_complete()`
  - `anthropic_download_batch_results()`
  - `parse_anthropic_batch_output()`
  - `run_anthropic_batch_pipeline()`
- **Gemini**
  - `build_gemini_batch_requests()`
  - `gemini_create_batch()`
  - `gemini_poll_batch_until_complete()`
  - `gemini_download_batch_results()`
  - `parse_gemini_batch_output()`
  - `run_gemini_batch_pipeline()`

The planned unified batch front-end will sit on top of these and provide
a backend-agnostic API.

## Roadmap

### Immediate

- Implement unified batch front-end:
  - `llm_submit_pairs_batch()`
  - `llm_download_batch_results()`
- Tighten error classes and input validation across public APIs

### Medium-term

- Vignettes:
  - Introduction to `pairwiseLLM`
  - Live comparisons across providers
  - Batch processing workflows
  - Bradley–Terry and Elo modeling pipelines using LLM comparisons  
- Implement Ollama backend for local models

### CRAN readiness

- Replace live tests with HTTP mocks (e.g., `httptest2` or `vcr`)  
- Wrap all network tests with `skip_on_cran()` and environment-based
  flags  
- Add:
  - `NEWS.md`
  - `cran-comments.md`
  - `CITATION` file  
- Achieve ≥ 80% test coverage on core modules  
- Run `styler::style_pkg()` and lightweight `lintr` checks

## Contributing

Contributions, issues, and pull requests are welcome!

- Repository: <https://github.com/shmercer/pairwiseLLM>
- If you file an issue, please include:
  - R session info (`sessionInfo()` or `sessioninfo::session_info()`)
  - Backend and model used
  - A minimal reproducible example when possible

## License

`pairwiseLLM` is released under the MIT License. See the `LICENSE` file
for details.

## Citation

A formal `CITATION` file will be added prior to the first CRAN release.

In the meantime, if you use `pairwiseLLM` in published work, you can
cite it as:

> Mercer, S. (2025). *pairwiseLLM: Pairwise comparisons of writing
> quality with LLMs* (R package). GitHub:
> <https://github.com/shmercer/pairwiseLLM>
