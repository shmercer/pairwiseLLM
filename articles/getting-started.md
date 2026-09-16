# Getting Started with pairwiseLLM

## What you will do

`pairwiseLLM` turns judgments about **which of two writing samples is
better** into relative writing-quality scores. The aspect being judged,
such as overall quality or organization, is called a **trait**.
Comparing samples in this way is also called **comparative judgment
(CJ)**.

This guide assumes you can run R code and work with a data frame. First,
you will analyze bundled synthetic comparisons without an API key or
provider charges. Then you will prepare samples and see how to collect
new comparisons using an LLM. All code through “Prepare a judging
prompt” runs locally; cloud requests later in the guide are shown but
are not executed when this page builds.

## Install the tools for this example

Use **R 4.4 or later**. Install the package and the optional `sirt`
modeling engine once, then load pairwiseLLM in each new R session:

``` r

install.packages(c("pairwiseLLM", "sirt"))
library(pairwiseLLM)
```

You do not need CmdStan, Python, or an LLM account for this first
example. Bayesian and adaptive analyses have additional setup described
in their guides.

## Your first result: scores from bundled comparisons

The package includes 190 comparison outcomes covering every unordered
pair of 20 synthetic writing samples. These outcomes were generated for
demonstration; they are not new LLM judgments or evidence of assessment
validity.

``` r

data("example_writing_pairs", package = "pairwiseLLM")
head(example_writing_pairs, 4)
#> # A tibble: 4 × 3
#>   ID1   ID2   better_id
#>   <chr> <chr> <chr>    
#> 1 S01   S02   S02      
#> 2 S01   S03   S03      
#> 3 S01   S04   S04      
#> 4 S01   S05   S01
```

`ID1` and `ID2` identify the two samples. `better_id` identifies the
winner.
[`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md)
converts these winners into the format needed by a Bradley–Terry (BT)
model. The model estimates relative scores from the pattern of wins and
losses.

``` r

bt_data <- build_bt_data(example_writing_pairs)
fit <- fit_bt_model(bt_data, engine = "sirt", verbose = FALSE)
scores <- fit$theta |> dplyr::arrange(dplyr::desc(theta))
head(scores, 5)
#> # A tibble: 5 × 3
#>   ID    theta    se
#>   <chr> <dbl> <dbl>
#> 1 S18   2.88  1.16 
#> 2 S13   1.91  0.794
#> 3 S20   1.73  0.985
#> 4 S15   1.12  0.842
#> 5 S14   0.921 0.836
```

If `sirt` is missing, run the installation command above before this
block. Choosing the engine explicitly makes this example consistent
across machines;
[`fit_bt_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_bt_model.md)
also supports an automatic choice and `BradleyTerry2`.

### Read the scores

| Column | Meaning | How to use it |
|----|----|----|
| `ID` | The sample identifier | Match the result to your original text. |
| `theta` | Estimated relative writing quality | Higher values indicate stronger writing on the assessed trait. |
| `se` | Standard error of the estimate | Larger values mean greater model uncertainty. |

The displayed rows are ordered by score. The full `scores` table has 20
rows. A difference between two estimates is not automatically a
meaningful difference: consider their uncertainty and the quality of the
judgments.

Zero is a location on a relative scale, not a pass mark. These values
are not percentages or rubric grades, and independently fitted cohorts
do not share a common scale. For rubric categories, use the [rubric
calibration
guide](https://shmercer.github.io/pairwiseLLM/articles/rubric-calibration.md),
which requires completed **Bayesian** CJ results; the frequentist fit
above is not an accepted input.

### Save an analysis

``` r

write.csv(scores, "writing_scores.csv", row.names = FALSE)
saveRDS(fit, "writing_bt_fit.rds")
# In a later session: fit <- readRDS("writing_bt_fit.rds")
```

The CSV is convenient for viewing scores in a spreadsheet. The RDS
preserves the fitted R object. These paths are relative to your current
working directory.

## Prepare your own writing samples

Start with one row per sample, a unique, non-missing `ID`, and a `text`
column. Keep IDs stable so that saved results continue to refer to the
same samples. Do not include names or other identifying information
merely to label rows.

``` r

my_samples <- data.frame(
  ID = c("essay_01", "essay_02", "essay_03"),
  text = c(
    "Writing helps us explain our ideas to others.",
    "We write to communicate, remember, and develop an argument.",
    "Writing is useful. I use it at school."
  )
)
samples <- read_samples_df(my_samples)
```

For a spreadsheet saved as CSV, map its columns to the package’s names:

``` r

my_data <- read.csv("writing.csv", colClasses = "character")
samples <- read_samples_df(my_data, id_col = "student_code", text_col = "response")
```

These three short texts illustrate the data format only. For the
collection example below, use the bundled 20 samples so every object is
defined:

``` r

data("example_writing_samples", package = "pairwiseLLM")
samples <- read_samples_df(example_writing_samples[, c("ID", "text")])
all_pairs <- make_pairs(samples)
nrow(all_pairs)
#> [1] 190

pairs_small <- all_pairs |>
  sample_pairs(n_pairs = 10, seed = 123) |>
  randomize_pair_order(seed = 99)
```

Pair construction makes no provider calls. Twenty samples produce
`choose(20, 2) = 190` unordered pairs; the total grows quickly as you
add samples. Randomizing presentation helps distribute which sample
appears first.

**The ten-pair subset is a collection demonstration, not a complete
ranking study.** Random sampling does not guarantee that every sample
appears or that all samples are connected through comparisons. The
offline fit above uses the full bundled comparison set. Plan coverage
and diagnostics before fitting your own collected data; [adaptive
pairing](https://shmercer.github.io/pairwiseLLM/articles/adaptive-pairing.md)
is an alternative that selects comparisons as evidence accumulates.

## Prepare a judging prompt

A prompt combines the trait, the judging instructions, and the two
texts. Start with a built-in trait and template:

``` r

td <- trait_description("overall_quality")
tmpl <- get_prompt_template("default")
td$name
#> [1] "Overall Quality"
```

You can inspect exactly what the model will receive before paying for a
call:

``` r

prompt <- build_prompt(
  template = tmpl,
  trait_name = td$name,
  trait_desc = td$description,
  text1 = pairs_small$text1[[1]],
  text2 = pairs_small$text2[[1]]
)
cat(substr(prompt, 1, 350))
#> You are a debate adjudicator. Your task is to weigh the comparative strengths of two writing samples regarding a specific trait.
#> 
#> TRAIT: Overall Quality
#> DEFINITION: Overall quality of the writing, considering how well ideas are expressed,
#> how clearly the writing is organized, and how effective the language and
#> conventions are.
#> 
#> SAMPLES:
#> 
#> === SAMPLE
```

This shows only the beginning; use `cat(prompt)` to read the full prompt
locally. See [Data Schemas and Prompt
Management](https://shmercer.github.io/pairwiseLLM/articles/data-and-prompts.md)
for custom traits, file templates, named registration, and required
placeholders.

## Collect a small set of new judgments

The remaining submission examples make **cloud requests that may incur
charges** when you run them. They send the prompt and sample text to
your chosen provider. Review its privacy and retention terms before
submitting sensitive data.

### Configure one provider

A **backend** selects the service; `model` selects a model offered by
that service. This example uses OpenAI. Use an identifier available to
your account; example identifiers are not a guarantee of current
availability. See [Backends and Tested Model
Configurations](https://shmercer.github.io/pairwiseLLM/articles/model-compatibility.md)
for the provider/key table and official catalogs.

For the current R session:

``` r

Sys.setenv(OPENAI_API_KEY = "your-key-here")
check_llm_api_keys()
```

`has_key = TRUE` means R can see a nonempty value; it does not test
whether the key is valid or the account can access a model. Missing keys
for other providers are expected. Never put a real key in a shared
script or report.

For persistent local setup, put `OPENAI_API_KEY="your-key-here"` in your
`~/.Renviron` file and restart R. If you use `usethis`,
`usethis::edit_r_environ()` opens that file. This is a file you choose
to store; the package does not save credentials for you. Ollama uses a
local server and does not require a provider API key.

### Submit the pairs

``` r

res_list <- submit_llm_pairs(
  pairs = pairs_small,
  backend = "openai",
  model = "gpt-4o",
  trait_name = td$name,
  trait_description = td$description,
  prompt_template = tmpl,
  parallel = FALSE,
  save_path = "live_results.csv"
)
```

The run processes pairs sequentially and saves completed results
incrementally. Use a distinct file for each study/model/trait
configuration. To continue the same interrupted run, keep its inputs and
saved file and repeat the call; matching saved pairs are skipped.
Parallel execution is optional and requires `future` and `future.apply`;
leave it off for a first run.

### Check what succeeded

``` r

head(res_list$results)
res_list$failed_pairs
res_list$failed_attempts
```

| Component | What it tells you |
|----|----|
| `results` | Comparisons with a valid winner. `better_id` identifies the winning sample. |
| `failed_pairs` | Scheduled pairs with no valid outcome. |
| `failed_attempts` | Attempt-level problems, including failures followed by successful retries. |

An HTTP success alone is not a valid judgment. Inspect unresolved pairs
before analysis; missing winners are not ties. The [recovery
guide](https://shmercer.github.io/pairwiseLLM/articles/provider-controls-and-recovery.md)
explains retrying failures and keeping successful work.

Once you have collected an adequate comparison design and inspected
failures, prepare its successful outcomes with
`build_bt_data(res_list$results)` and use the same fit-and-inspect steps
as the offline example. Do not interpret the small submission
demonstration as an adequate design for all 20 samples.

## Choose the next workflow

| Your goal | Guide or function |
|----|----|
| More control over data and judging instructions | [Data and prompts](https://shmercer.github.io/pairwiseLLM/articles/data-and-prompts.md) |
| Estimate token costs with a paid pilot; configure or troubleshoot providers | [Provider controls and recovery](https://shmercer.github.io/pairwiseLLM/articles/provider-controls-and-recovery.md) |
| Submit fixed pairs for later retrieval, including resumable large jobs | [Batch workflows](https://shmercer.github.io/pairwiseLLM/articles/advanced-batch-workflows.md) |
| Fit Elo ratings instead of BT scores | [`build_elo_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_elo_data.md) and [`fit_elo_model()`](https://shmercer.github.io/pairwiseLLM/reference/fit_elo_model.md) (optional `EloChoice`) |
| Estimate Bayesian scores and inspect convergence | [Bayesian BTL](https://shmercer.github.io/pairwiseLLM/articles/bayesian-btl.md) (requires CmdStan) |
| Let the next pair depend on earlier judgments | [Adaptive pairing](https://shmercer.github.io/pairwiseLLM/articles/adaptive-pairing.md) (requires CmdStan) |
| Convert completed Bayesian scores to ordered rubric levels | [Rubric calibration](https://shmercer.github.io/pairwiseLLM/articles/rubric-calibration.md) |
| Compare separately evaluated cohorts on a shared scale | [Adaptive linking](https://shmercer.github.io/pairwiseLLM/articles/adaptive-linking.md) |
| Check whether presentation order affects judgments | [Prompt bias testing](https://shmercer.github.io/pairwiseLLM/articles/prompt-template-bias.md) |

Live processing is useful for interactive work. Batch processing can
suit larger fixed-pair jobs when you can wait for completion; support
and pricing depend on the provider. There is no universal pair-count
threshold for choosing between them.

## Common first-run problems

| Problem | First check |
|----|----|
| The offline model says `sirt` is missing | Run `install.packages("sirt")` once, then rerun the fit. |
| R cannot see my key | Restart R after editing `.Renviron`, then use [`check_llm_api_keys()`](https://shmercer.github.io/pairwiseLLM/reference/check_llm_api_keys.md). |
| The provider rejects the model or a setting | Check account access and the provider’s current catalog; start with minimal controls. |
| Some comparisons are missing | Inspect `failed_pairs` and `failed_attempts`; keep successful results. |
| Scores look unlike familiar grades | BT scores are relative estimates; rubric scoring is a separate Bayesian workflow. |
| A batch is still pending | Poll or resume the existing job; do not submit it again merely because it is unfinished. |

For a reproducible issue report, include your code with credentials
removed, the error, provider/model, operating system, and
[`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html) output.

## Citation

> Mercer, S. H. (2026). *Getting started with pairwiseLLM* \[R package
> vignette\]. Comprehensive R Archive Network.
> <https://doi.org/10.32614/CRAN.package.pairwiseLLM>
