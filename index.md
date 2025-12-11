pairwiseLLM: Pairwise Comparison Tools for Large Language Model-Based
Writing Evaluation ================

`pairwiseLLM` provides a unified, extensible framework for generating,
submitting, and modeling **pairwise comparisons of writing quality**
using large language models (LLMs).

It includes:

- Unified **live** and **batch** APIs across OpenAI, Anthropic, and
  Gemini  
- A prompt template registry with **tested templates** designed to
  reduce positional bias  
- Positional-bias diagnostics (forward vs reverse design)  
- Bradley–Terry (BT) and Elo modeling  
- Consistent data structures for all providers

------------------------------------------------------------------------

## Vignettes

Several vignettes are available to demonstrate functionality.

For basic function usage, see:

- [`vignette("getting-started")`](https://shmercer.github.io/pairwiseLLM/articles/getting-started.html)

For advanced batch processing workflows, see:

- [`vignette("advanced-batch-workflows")`](https://shmercer.github.io/pairwiseLLM/articles/advanced-batch-workflows.html)

For information on prompt evaluation and positional-bias diagnostics,
see:

- [`vignette("prompt-template-bias")`](https://shmercer.github.io/pairwiseLLM/articles/prompt-template-bias.html)

------------------------------------------------------------------------

## Supported Models

The following models are confirmed to work for pairwise comparisons:

| Provider                                             | Model                         | Reasoning Mode? |
|------------------------------------------------------|-------------------------------|-----------------|
| **[OpenAI](https://openai.com/api/)**                | gpt-5.1                       | ✅ Yes          |
| **[OpenAI](https://openai.com/api/)**                | gpt-4o                        | ❌ No           |
| **[OpenAI](https://openai.com/api/)**                | gpt-4.1                       | ❌ No           |
| **[Anthropic](https://console.anthropic.com/)**      | claude-sonnet-4-5             | ✅ Yes          |
| **[Anthropic](https://console.anthropic.com/)**      | claude-haiku-4-5              | ✅ Yes          |
| **[Anthropic](https://console.anthropic.com/)**      | claude-opus-4-5               | ✅ Yes          |
| **[Google/Gemini](https://aistudio.google.com/)**    | gemini-3-pro-preview          | ✅ Yes          |
| **[DeepSeek-AI](https://www.deepseek.com/en)₁**      | DeepSeek-R1                   | ✅ Yes          |
| **[DeepSeek-AI](https://www.deepseek.com/en)₁**      | DeepSeek-V3                   | ❌ No           |
| **[Moonshot-AI](https://www.moonshot.ai/)₁**         | Kimi-K2-Instruct-0905         | ❌ No           |
| **[Qwen](https://qwen.ai/home)₁**                    | Qwen3-235B-A22B-Instruct-2507 | ❌ No           |
| **[Qwen](https://qwen.ai/home)₂**                    | qwen3:32b                     | ✅ Yes          |
| **[Google](https://deepmind.google/models/gemma/)₂** | gemma3:27b                    | ❌ No           |
| **[Mistral](https://mistral.ai/)₂**                  | mistral-small3.2:24b          | ❌ No           |

₁ via the [together.ai](https://www.together.ai/) API

₂ via [Ollama](https://ollama.com/) on a local machine

Batch APIs are currently available for OpenAI, Anthropic, and Gemini
only. Models accessed via Together.ai and Ollama are supported for live
comparisons via
[`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md)
/
[`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md).

| Backend   | Live | Batch |
|-----------|------|-------|
| openai    | ✅   | ✅    |
| anthropic | ✅   | ✅    |
| gemini    | ✅   | ✅    |
| together  | ✅   | ❌    |
| ollama    | ✅   | ❌    |

------------------------------------------------------------------------

## Installation

Once the package is available on CRAN, install with:

``` r
install.packages("pairwiseLLM")
```

To install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("shmercer/pairwiseLLM")
```

Load the package:

``` r
library(pairwiseLLM)
```

------------------------------------------------------------------------

## Core Concepts

At a high level, `pairwiseLLM` workflows follow this structure:

1.  **Writing samples** – e.g., essays, constructed responses, short
    answers.  
2.  **Trait** – a rating dimension such as “overall quality” or
    “organization”.  
3.  **Pairs** – pairs of samples to be compared for that trait.  
4.  **Prompt template** – instructions + placeholders for
    `{TRAIT_NAME}`, `{TRAIT_DESCRIPTION}`, `{SAMPLE_1}`, `{SAMPLE_2}`.  
5.  **Backend** – which provider/model to use (OpenAI, Anthropic,
    Gemini, Together, Ollama).  
6.  **Modeling** – convert pairwise results to latent scores via BT or
    Elo.

The package provides helpers for each step.

------------------------------------------------------------------------

## Live Comparisons

Use the unified API:

- [`llm_compare_pair()`](https://shmercer.github.io/pairwiseLLM/reference/llm_compare_pair.md)
  — compare one pair  
- [`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md)
  — compare many pairs at once

Example:

``` r
data("example_writing_samples")

pairs <- example_writing_samples |>
  make_pairs() |>
  sample_pairs(5, seed = 123) |>
  randomize_pair_order()

td <- trait_description("overall_quality")
tmpl <- get_prompt_template("default")

res <- submit_llm_pairs(
  pairs             = pairs,
  backend           = "openai",
  model             = "gpt-4o",
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl
)
```

------------------------------------------------------------------------

## Batch Comparisons

Large-scale runs use:

- [`llm_submit_pairs_batch()`](https://shmercer.github.io/pairwiseLLM/reference/llm_submit_pairs_batch.md)  
- [`llm_download_batch_results()`](https://shmercer.github.io/pairwiseLLM/reference/llm_download_batch_results.md)

Example:

``` r
batch <- llm_submit_pairs_batch(
  backend           = "anthropic",
  model             = "claude-sonnet-4-5",
  pairs             = pairs,
  trait_name        = td$name,
  trait_description = td$description,
  prompt_template   = tmpl
)

results <- llm_download_batch_results(batch)
```

------------------------------------------------------------------------

## API Keys

`pairwiseLLM` reads keys **only from environment variables**.  
Keys are **never printed**, **never stored**, and **never written** to
disk.

You can verify which providers are available using:

``` r
check_llm_api_keys()
```

This returns a tibble showing whether R can see the required keys for:

- OpenAI  
- Anthropic  
- Google Gemini
- Together.ai

### Setting API Keys

You may set keys **temporarily** for the current R session:

``` r
Sys.setenv(OPENAI_API_KEY = "your-key-here")
Sys.setenv(ANTHROPIC_API_KEY = "your-key-here")
Sys.setenv(GEMINI_API_KEY = "your-key-here")
Sys.setenv(TOGETHER_API_KEY = "your-key-here")
```

…but for normal use and for reproducible analyses, it is **strongly
recommended**  
to store them in your `~/.Renviron` file.

### Recommended method: Adding keys to `~/.Renviron`

Open your `.Renviron` file:

``` r
usethis::edit_r_environ()
```

Add the following lines:

``` R
OPENAI_API_KEY="your-openai-key"
ANTHROPIC_API_KEY="your-anthropic-key"
GEMINI_API_KEY="your-gemini-key"
TOGETHER_API_KEY="your-together-key"
```

Save the file, then restart R.

You can confirm that R now sees the keys:

``` r
check_llm_api_keys()
```

------------------------------------------------------------------------

## Prompt Templates & Registry

`pairwiseLLM` includes:

- A **default template** tested for positional bias  
- Support for **multiple templates stored by name**  
- User-defined templates via
  [`register_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/register_prompt_template.md)

### View available templates

``` r
list_prompt_templates()
#> [1] "default" "test1"   "test2"   "test3"   "test4"   "test5"
```

### Show the default template (truncated)

``` r
tmpl <- get_prompt_template("default")
cat(substr(tmpl, 1, 400), "...\n")
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
#> 1.  **Advocate for SAMPLE_1**: Mentally list the single strongest point of evidence that makes SAMPLE_1 the  ...
```

### Register your own template

``` r
register_prompt_template("my_template", "
Compare two essays for {TRAIT_NAME}…

{TRAIT_NAME} is defined as {TRAIT_DESCRIPTION}.

SAMPLE 1:
{SAMPLE_1}

SAMPLE 2:
{SAMPLE_2}

<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE> or
<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>
")
```

Use it in a submission:

``` r
tmpl <- get_prompt_template("my_template")
```

------------------------------------------------------------------------

## Trait Descriptions

Traits define what “quality” means.

``` r
trait_description("overall_quality")
#> $name
#> [1] "Overall Quality"
#> 
#> $description
#> [1] "Overall quality of the writing, considering how well ideas are expressed,\n      how clearly the writing is organized, and how effective the language and\n      conventions are."
```

You can also provide custom traits:

``` r
trait_description(
  custom_name        = "Clarity",
  custom_description = "How understandable, coherent, and well structured the ideas are."
)
```

------------------------------------------------------------------------

## Positional Bias Testing

LLMs often show a first-position or second-position bias.  
`pairwiseLLM` includes explicit tools for testing this.

### Typical workflow

``` r
pairs_fwd <- make_pairs(example_writing_samples)
pairs_rev <- sample_reverse_pairs(pairs_fwd, reverse_pct = 1.0)
```

Submit:

``` r
res_fwd <- submit_llm_pairs(pairs_fwd, model = "gpt-4o", backend = "openai", ...)
res_rev <- submit_llm_pairs(pairs_rev, model = "gpt-4o", backend = "openai", ...)
```

Compute bias:

``` r
cons <- compute_reverse_consistency(res_fwd, res_rev)
bias <- check_positional_bias(cons)

cons$summary
bias$summary
```

### Positional-bias tested templates

Five included templates have been tested across different backend
providers. Complete details are presented in a vignette:
[`vignette("prompt-template-bias")`](https://shmercer.github.io/pairwiseLLM/articles/prompt-template-bias.html)

------------------------------------------------------------------------

## Bradley–Terry & Elo Modeling

### Bradley–Terry (BT)

``` r
bt_data <- build_bt_data(res)
bt_fit <- fit_bt_model(bt_data)
summarize_bt_fit(bt_fit)
```

### Elo Modeling

``` r
# res: output from submit_llm_pairs() / llm_submit_pairs_batch()
elo_data <- build_elo_data(res)
elo_fit  <- fit_elo_model(elo_data, runs = 5)

elo_fit$elo
elo_fit$reliability
elo_fit$reliability_weighted
```

------------------------------------------------------------------------

## Live vs Batch Summary

| Workflow  | Use Case                  | Functions                                              |
|-----------|---------------------------|--------------------------------------------------------|
| **Live**  | small or interactive runs | `submit_llm_pairs`, `llm_compare_pair`                 |
| **Batch** | large jobs, cost control  | `llm_submit_pairs_batch`, `llm_download_batch_results` |

------------------------------------------------------------------------

## Contributing

Contributions to **pairwiseLLM** are very welcome!

- Bug reports (with reproducible examples when possible)
- Feature requests, ideas, and discussion
- Pull requests improving:
  - functionality
  - documentation
  - examples / vignettes
  - test coverage
- Backend integrations (e.g., additional LLM providers or local
  inference engines)
- Modeling extensions

## Reporting issues

If you encounter a problem:

1.  Run:

    ``` r
    devtools::session_info()
    ```

2.  Include:

    - reproducible code
    - the error message
    - the model/backend involved
    - your operating system

3.  Open an issue at:  
    <https://github.com/shmercer/pairwiseLLM/issues>

------------------------------------------------------------------------

## License

MIT License. See `LICENSE`.

------------------------------------------------------------------------

## Package Author and Maintainer

- **Sterett H. Mercer** – *University of British Columbia*  
  UBC Faculty Profile: <https://ecps.educ.ubc.ca/sterett-h-mercer/>  
  ResearchGate: <https://www.researchgate.net/profile/Sterett_Mercer/>  
  Google Scholar:
  <https://scholar.google.ca/citations?user=YJg4svsAAAAJ&hl=en>

------------------------------------------------------------------------

## Citation

> Mercer, S. H. (2025). pairwiseLLM: Pairwise writing quality
> comparisons with large language models (Version 1.0.0) \[R package;
> Computer software\]. <https://github.com/shmercer/pairwiseLLM>
