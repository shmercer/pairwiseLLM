# Prompt Template Positional Bias Testing

## 1. Motivation

`pairwiseLLM` uses large language models (LLMs) to compare pairs of
writing samples and decide which sample is better on a given trait (for
example, *Overall Quality*).

If a **prompt template** systematically nudges the model toward the
first or second position, then scores derived from these comparisons may
be biased. This vignette documents how we:

- Designed and tested several prompt templates for **positional bias**  
- Quantified both **reverse-order consistency** and **preference for
  SAMPLE_1**  
- Selected templates that appear robust across multiple providers and
  reasoning configurations

The vignette also shows how to:

- Retrieve the tested templates from the package  
- Inspect their full text  
- Access summary statistics from the experiments

For basic function usage, see:

- [`vignette("getting-started")`](https://shmercer.github.io/pairwiseLLM/articles/getting-started.html)

For advanced batch processing workflows, see:

- [`vignette("advanced-batch-workflows")`](https://shmercer.github.io/pairwiseLLM/articles/advanced-batch-workflows.html)

------------------------------------------------------------------------

## 2. Testing Process Summary

At a high level, the testing pipeline works as follows:

1.  **Trait and samples**

    - Choose a trait (here: `"overall_quality"`) and obtain its
      description with
      [`trait_description()`](https://shmercer.github.io/pairwiseLLM/reference/trait_description.md).
    - Use `example_writing_samples` or your own dataset of writing
      samples.

2.  **Generate forward and reverse pairs**

    - Use
      [`make_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/make_pairs.md)
      to generate all ordered pairs.
    - Use
      [`alternate_pair_order()`](https://shmercer.github.io/pairwiseLLM/reference/alternate_pair_order.md)
      to build a deterministic “forward” set.
    - Use
      [`sample_reverse_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/sample_reverse_pairs.md)
      with `reverse_pct = 1` to build a fully “reversed” set, where
      SAMPLE_1 and SAMPLE_2 are swapped for all pairs.

3.  **Prompt templates**

    - Define multiple templates (e.g., `"test1"`–`"test5"`) and register
      them in the template registry.
    - Each template is a text file shipped with the package and accessed
      via `get_prompt_template("testX")`.

4.  **Batch calls to LLM providers**

    - For each combination of:

      - Template (`test1`–`test5`)
      - Backend (Anthropic, Gemini, OpenAI, TogetherAI)
      - Model (e.g., `claude-sonnet-4-5`, `gpt-4o`,
        `gemini-3-pro-preview`)
      - Thinking configuration (`"no_thinking"` vs `"with_thinking"`,
        where applicable)
      - Direction (`forward` vs `reverse`)

    - Submit the forward and reverse pairs. You can do this using the
      package’s Batch API helpers (for large-scale jobs) or the live API
      wrapper
      [`submit_llm_pairs()`](https://shmercer.github.io/pairwiseLLM/reference/submit_llm_pairs.md)
      with `parallel = TRUE` (for faster turnaround on smaller test
      sets).

    - Store responses as CSVs, including the model’s `<BETTER_SAMPLE>`
      decision and derived `better_id`.

5.  **Reverse-order consistency**

    - For each (template, provider, model, thinking), compare:

      - The model’s decisions for a pair in the forward set
      - The decisions for the same pair in the reverse set (where
        positions are swapped)

    - Use
      [`compute_reverse_consistency()`](https://shmercer.github.io/pairwiseLLM/reference/compute_reverse_consistency.md)
      to compute:

      - `prop_consistent`: proportion of comparisons where reversing the
        order yields the same **underlying winner**.

6.  **Positional bias statistics**

    - Use
      [`check_positional_bias()`](https://shmercer.github.io/pairwiseLLM/reference/check_positional_bias.md)
      on the reverse-consistency results to quantify:

      - `prop_pos1`: proportion of all comparisons where SAMPLE_1 is
        chosen as better.
      - `p_sample1_overall`: p-value from a binomial test of whether the
        probability of choosing SAMPLE_1 differs from 0.5.

7.  **Summarize and interpret**

    - Aggregate the results across templates and models into a summary
      table.
    - Look for templates with:
      - High `prop_consistent` (close to 1).
      - `prop_pos1` close to 0.5.
      - Non-significant positional bias (`p_sample1_overall` not \<
        .05).

In the sections below we show how to retrieve the templates, how they
are intended to be used, and how to examine the summary statistics for
the experiment.

------------------------------------------------------------------------

## 3. Trait descriptions and custom traits

In the tests, we evaluated samples for overall quality.

``` r
td <- trait_description("overall_quality")
td
#> $name
#> [1] "Overall Quality"
#> 
#> $description
#> [1] "Overall quality of the writing, considering how well ideas are expressed,\n      how clearly the writing is organized, and how effective the language and\n      conventions are."
```

In *pairwiseLLM*, every pairwise comparison evaluates writing samples on
a **trait** — a specific dimension of writing quality, such as:

- **Overall Quality**
- **Organization**
- **Development**
- **Language**

The trait determines *what the model should focus on* when choosing
which sample is better. Each trait has:

- a **short name** (e.g., `"overall_quality"`)
- a **human-readable name** (e.g., `"Overall Quality"`)
- a **textual description** used inside prompts

The function that supplies these definitions is:

``` r
trait_description(name, custom_name = NULL, custom_description = NULL)
```

------------------------------------------------------------------------

### 3.1 Built-in traits

The package includes some predefined traits accessible by name:

``` r
trait_description("overall_quality")
trait_description("organization")
```

Calling a built-in trait returns a list with:

``` r
$list
$name         # human-friendly name
$description  # the textual rubric used in prompts
```

Example:

``` r
td <- trait_description("organization")
td$name
td$description
```

This description is inserted into your chosen prompt template wherever
`{TRAIT_DESCRIPTION}` appears.

------------------------------------------------------------------------

### 3.2 Setting a different built-in trait

To switch evaluations to another trait, simply pass its ID:

``` r
td <- trait_description("organization")

prompt <- build_prompt(
  template   = get_prompt_template("test1"),
  trait_name = td$name,
  trait_desc = td$description,
  text1      = sample1,
  text2      = sample2
)
```

This will automatically update all trait-specific wording in the prompt.

------------------------------------------------------------------------

### 3.3 Creating a custom trait

If your study requires a new writing dimension, you can define your own
trait directly in the call:

``` r
td <- trait_description(
  custom_name        = "Clarity",
  custom_description = "Clarity refers to how easily a reader can understand the writer's ideas, wording, and structure."
)

td$name
#> [1] "Clarity"

td$description
#> [1] "Clarity refers to how easily ..."
```

No built-in name needs to be supplied when using custom text:

``` r
prompt <- build_prompt(
  template   = get_prompt_template("test2"),
  trait_name = td$name,
  trait_desc = td$description,
  text1      = sample1,
  text2      = sample2
)
```

------------------------------------------------------------------------

### 3.4 Why traits matter for positional bias testing

Traits determine the **criterion of comparison**, and different traits
may produce different sensitivity patterns in LLM behavior. For example:

- “Overall Quality” may yield more stable results than “Development”
- Short, concise trait definitions may reduce positional bias
- Custom traits allow experimentation with alternative rubric wordings

Because positional bias interacts with how the model interprets the
trait, *every trait–template combination* can be evaluated using the
same workflow described earlier in this vignette.

------------------------------------------------------------------------

## 4. Example data used in tests

The positional-bias experiments in this vignette use the
`example_writing_samples` dataset that ships with the package.

Each row represents a student writing sample and includes:

- an identifying ID,
- a `text` field containing the full written response.

Below we print the 20 writing samples included in the file.  
This dataset provides a reproducible testing base; in real applications,
you would use your own writing samples.

``` r
data("example_writing_samples", package = "pairwiseLLM")

# Inspect the structure
glimpse(example_writing_samples)
#> Rows: 20
#> Columns: 3
#> $ ID            <chr> "S01", "S02", "S03", "S04", "S05", "S06", "S07", "S08", …
#> $ text          <chr> "Writing assessment is hard. People write different thin…
#> $ quality_score <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1…

# Print the 20 samples (full text)
example_writing_samples |>
  kable(
    caption = "20 example writing samples included with pairwiseLLM."
  )
```

| ID                                         | text                                                                      | quality_score |
|:-------------------------------------------|:--------------------------------------------------------------------------|--------------:|
| S01                                        | Writing assessment is hard. People write different things. It is          |               |
| confusing.                                 | 1                                                                         |               |
| S02                                        | It is hard to grade writing. Some are long and some are short. I do not   |               |
| know which is best.                        | 2                                                                         |               |
| S03                                        | Assessing writing is difficult because everyone writes differently and it |               |
| can be hard to decide what is good or bad. | 3                                                                         |               |
| S04                                        | Grading essays is tough work. You have to read a lot. Sometimes the       |               |

20 example writing samples included with pairwiseLLM.

    handwriting is bad or the grammar is wrong, and that makes it hard to give
    a score.                                                                                                                                                                                                                                           |             4|

\|S05 \|Writing assessment is challenging because teachers must judge
ideas, organization, grammar, and style all at once. Different raters
may focus on different things. \| 5\| \|S06 \|It is difficult to assess
writing because it is subjective. One teacher might like a creative
style while another teacher wants a strict structure. This makes the
scores unfair sometimes. \| 6\| \|S07 \|Writing assessment is difficult
because writing is a complex skill. Raters must consider ideas,
organization, style, and conventions, and these features do not always
align. \| 7\| \|S08 \|A paper with strong ideas might have weak grammar,
while another has flawless sentences but no clear argument. Deciding
which one deserves a higher score is a major challenge in assessment. \|
8\| \|S09 \|Assessing writing is difficult because the construct is
multidimensional. Even with detailed rubrics, raters interpret criteria
differently, and their judgments can be influenced by fatigue or
expectations. \| 9\| \|S10 \|The difficulty in writing assessment lies
in consistency. Because raters bring their own background knowledge and
preferences to the task, achieving high inter-rater reliability requires
extensive training and calibration. \| 10\| \|S11 \|Writing assessment
is difficult because we are trying to compress a rich, multi-dimensional
performance into a single score. Raters must weigh content,
organization, style, and mechanics, while also dealing with time
pressure. \| 11\| \|S12 \|Evaluating writing is challenging because no
rubric can fully capture what makes a text effective for a particular
audience. Two essays might receive the same score for completely
different reasons, obscuring the feedback loop. \| 12\| \|S13 \|Writing
assessment is difficult because it is context-dependent. A style that
works for a narrative is inappropriate for a report. Raters must
constantly adjust their internal standard based on the specific purpose
of the prompt. \| 13\| \|S14 \|The challenge of writing assessment is
distinguishing between surface-level errors and deep structural flaws.
Raters often over-penalize mechanical mistakes while missing more
significant issues in logic or argumentation due to cognitive load. \|
14\| \|S15 \|Writing assessment is difficult because it sits at the
intersection of measurement and interpretation. Raters must translate
complex judgments about ideas, voice, and language into discrete rubric
categories, often losing nuance in the process. \| 15\| \|S16
\|Assessing writing is inherently difficult because it requires
balancing consistency with sensitivity. A rubric describes general
qualities, but individual texts vary in genre and voice. Raters must
decide if an unconventional choice is a mistake or a stylistic
innovation. \| 16\| \|S17 \|Writing assessment is challenging because of
the trade-off between validity and reliability. Highly standardized
scoring protocols often strip away the subjective appreciation of voice
and creativity, while holistic scoring captures the ‘whole’ but risks
being unreliable. \| 17\| \|S18 \|The fundamental difficulty in writing
assessment is cognitive complexity. The rater must construct a mental
model of the writer’s argument while simultaneously evaluating against
specific criteria. This dual processing makes the task prone to bias and
halo effects. \| 18\| \|S19 \|Writing assessment is difficult because it
asks us to quantify something fundamentally qualitative. To evaluate a
piece of writing, raters integrate judgments about content,
organization, and style, while also considering task demands. Scores
often reflect both the text and the rater’s implicit theory of writing.
\| 19\| \|S20 \|Writing assessment is inherently problematic because it
attempts to standardize a socially situated act. The assessment process
often decontextualizes the writing, stripping it of its communicative
purpose. Consequently, the score represents a construct of ‘school
writing’ rather than authentic communication, creating a validity gap
that simple psychometrics cannot resolve. \| 20\|

------------------------------------------------------------------------

## 5. Built-in prompt templates

The tested templates are stored as plain-text files in the package and
exposed via the template registry. You can retrieve them with
[`get_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/get_prompt_template.md):

``` r
template_ids <- paste0("test", 1:5)
template_ids
#> [1] "test1" "test2" "test3" "test4" "test5"
```

Use
[`get_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/get_prompt_template.md)
to view the text:

``` r
cat(substr(get_prompt_template("test1"), 1, 500), "...\n")
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
#> 1.  **Advocate for SAMPLE_1**: Mentally list the single strongest point of evidence that makes SAMPLE_1 the winner.
#> 2.  **Advocate for SAMPLE_2**: Mentally list the single strongest point of evidence that mak ...
```

The same pattern works for all templates:

``` r
# Retrieve another template
tmpl_test3 <- get_prompt_template("test3")

# Use it to build a concrete prompt for a single comparison
pairs <- example_writing_samples |>
  make_pairs() |>
  head(1)

prompt_text <- build_prompt(
  template   = tmpl_test3,
  trait_name = td$name,
  trait_desc = td$description,
  text1      = pairs$text1[1],
  text2      = pairs$text2[1]
)

cat(prompt_text)
```

------------------------------------------------------------------------

## 6. Forward and reverse pairs

Here is a small example of how we constructed forward and reverse
datasets for each experiment:

``` r
pairs_all <- example_writing_samples |>
  make_pairs()

pairs_forward <- pairs_all |>
  alternate_pair_order()

pairs_reverse <- sample_reverse_pairs(
  pairs_forward,
  reverse_pct = 1.0,
  seed        = 2002
)

pairs_forward[1:3, c("ID1", "ID2")]
#> # A tibble: 3 × 2
#>   ID1   ID2  
#>   <chr> <chr>
#> 1 S01   S02  
#> 2 S03   S01  
#> 3 S01   S04
pairs_reverse[1:3, c("ID1", "ID2")]
#> # A tibble: 3 × 2
#>   ID1   ID2  
#>   <chr> <chr>
#> 1 S18   S02  
#> 2 S18   S06  
#> 3 S07   S08
```

In `pairs_reverse`, SAMPLE_1 and SAMPLE_2 are swapped for every pair
relative to `pairs_forward`. All other metadata (IDs, traits, etc.)
remain consistent so that we can compare results pairwise.

------------------------------------------------------------------------

## 7. Thinking / Reasoning Configurations Used in Testing

Many LLM providers now expose *reasoning-enhanced* decoding modes
(sometimes called “thinking,” “chain-of-thought modules,” or “structured
reasoning engines”).  
In `pairwiseLLM`, these modes are exposed through a simple parameter:

    thinking = "no_thinking"   # standard inference mode  
    thinking = "with_thinking" # activates provider's reasoning system

However, the *actual meaning* of these settings is **backend-specific**.
Below we describe the exact configurations used in our positional-bias
tests.

------------------------------------------------------------------------

### 7.1 Anthropic (Claude 4.5 models)

Anthropic’s batch API allows explicit control over the reasoning system.

#### `thinking = "no_thinking"`

- `reasoning = "none"`
- `temperature = 0`  
- Thinking tokens disabled  
- Intended to give **deterministic** behavior

#### `thinking = "with_thinking"`

- `reasoning = "enabled"`
- `temperature = 1`
- `include_thoughts = TRUE`
- `thinking_budget = 1024` (max internal reasoning tokens)
- Produces Claude’s full structured reasoning trace (not returned to the
  user)

This mode yields **more reflective but less deterministic** decisions.

------------------------------------------------------------------------

### 7.2 Gemini 3 Pro Preview

Gemini’s batch API exposes reasoning through the `thinkingLevel` field.

#### Only `thinking = "with_thinking"` was used

Settings used:

- `thinkingLevel = "low"`  
- `includeThoughts = TRUE`  
- `temperature` left at **provider default**  
- Gemini’s structured reasoning is stored internally for bias testing

This yields lightweight reasoning comparable to Anthropic’s enabled
mode.

------------------------------------------------------------------------

### 7.3 OpenAI (gpt-4.1, gpt-4o, gpt-5.1)

OpenAI supports two distinct APIs:

1.  **`chat.completions`** — standard inference  
2.  **`responses`** — reasoning-enabled (formerly “Chain of Thought” via
    `o-series`)

#### `thinking = "no_thinking"`

Used for **all models**, including gpt-5.1:

- Endpoint: `chat.completions`
- `temperature = 0`
- No reasoning traces  
- Most **deterministic mode**, ideal for repeatable scoring

#### `thinking = "with_thinking"` (gpt-5.1 only)

- Endpoint: `responses`  
- `reasoning = "low"`  
- `include_thoughts = TRUE`  
- No explicit `temperature` parameter (OpenAI ignores it for this
  endpoint)

This mode returns reasoning metadata that is stripped prior to analysis.

### 7.4 TogetherAI (Deepseek-R1, Deepseek-V3, Kimi-K2, Qwen3)

For Together.ai we ran positional-bias experiments using the Chat
Completions API (/v1/chat/completions) for the following models:

- “deepseek-ai/DeepSeek-R1”
- “deepseek-ai/DeepSeek-V3”
- “moonshotai/Kimi-K2-Instruct-0905”
- “Qwen/Qwen3-235B-A22B-Instruct-2507-tput”

DeepSeek-R1 emits internal reasoning wrapped in … tags. DeepSeek-V3,
Kimi-K2, and Qwen3 do not have a separate reasoning switch; any
“thinking” they do is part of their standard text output.

Temperature settings used in testing: - “deepseek-ai/DeepSeek-R1”:
`temperature = 0.6` - DeepSeek-V3, Kimi-K2, Qwen3: `temperature = 0.0`

------------------------------------------------------------------------

### 7.5 Summary Table of Backend-Specific Behavior

| Backend   | Thinking Mode       | What It Controls                                                 | Temperature Used  | Notes                                                     |
|-----------|---------------------|------------------------------------------------------------------|-------------------|-----------------------------------------------------------|
| Anthropic | no_thinking         | reasoning=none, no thoughts                                      | **0**             | deterministic                                             |
| Anthropic | with_thinking       | reasoning enabled, thoughts included, budget=1024                | **1**             | rich internal reasoning                                   |
| Gemini    | with_thinking only  | thinkingLevel=“low”, includeThoughts                             | provider default  | batch API does not support pure no-thinking mode          |
| OpenAI    | no_thinking         | chat.completions, no reasoning                                   | **0**             | deterministic                                             |
| OpenAI    | with_thinking (5.1) | responses API with reasoning=low                                 | ignored / N/A     | only applied to gpt-5.1                                   |
| Together  | with_thinking       | Chat Completions with `<think>…</think>` extracted to `thoughts` | **0.6** (default) | internal reasoning always on; visible answer in `content` |
| Together  | no_thinking         | Chat Completions, no explicit reasoning toggle                   | **0**             | reasoning not supported in these specific models          |

------------------------------------------------------------------------

## 8. Loading summary results

The results from the experiments are stored in a CSV included in the
package (for example, under
`inst/extdata/template_test_summary_all.csv`). We load and lightly clean
that file here.

``` r
summary_path <- system.file("extdata", "template_test_summary_all.csv", package = "pairwiseLLM")
if (!nzchar(summary_path)) stop("Data file not found in installed package.")

summary_tbl <- readr::read_csv(summary_path, show_col_types = FALSE)
head(summary_tbl)
#> # A tibble: 6 × 7
#>   template_id backend model thinking prop_consistent prop_pos1 p_sample1_overall
#>   <chr>       <chr>   <chr> <chr>              <dbl>     <dbl>             <dbl>
#> 1 test1       anthro… clau… no_thin…           0.895     0.505            0.878 
#> 2 test1       anthro… clau… with_th…           0.932     0.497            0.959 
#> 3 test1       anthro… clau… no_thin…           0.884     0.516            0.573 
#> 4 test1       anthro… clau… with_th…           0.905     0.484            0.573 
#> 5 test1       anthro… clau… no_thin…           0.884     0.442            0.0273
#> 6 test1       anthro… clau… with_th…           0.884     0.447            0.0453
```

------------------------------------------------------------------------

### 8.1 Column definitions

The columns in `summary_tbl` are:

- **`template_id`**  
  ID of the prompt template (e.g., `"test1"`).

- **`backend`**  
  LLM backend (`"anthropic"`, `"gemini"`, `"openai"`, `"together"`).

- **`model`**  
  Specific model (e.g., `"claude-sonnet-4-5"`, `"gpt-4o"`,
  `"gemini-3-pro-preview"`).

- **`thinking`**  
  Reasoning configuration (usually `"no_thinking"` or
  `"with_thinking"`). The exact meaning depends on the provider and dev
  script (for example, reasoning turned on vs off, or thinking-level
  settings for Gemini).

- **`prop_consistent`**  
  Proportion of comparisons that remained consistent when the pair order
  was reversed. Higher values indicate greater order-invariance.

- **`prop_pos1`**  
  Proportion of comparisons where SAMPLE_1 was chosen as better. Values
  near 0.5 indicate little or no positional bias toward the first
  position.

- **`p_sample1_overall`**  
  p-value from a binomial test of whether the probability of choosing
  SAMPLE_1 differs from 0.5. Smaller p-values suggest that the observed
  preference (for or against SAMPLE_1) is unlikely to be due to chance
  alone.

------------------------------------------------------------------------

### 8.2 Interpreting the statistics

The three key statistics for each (template, provider, model, thinking)
combination are:

1.  **Proportion consistent (`prop_consistent`)**

    - Measures how often the underlying winner remains the same when a
      pair is presented forward vs reversed.
    - Values close to 1 indicate strong order-invariance.
    - In practice, values above roughly 0.90 are generally reassuring.

2.  **Proportion choosing SAMPLE_1 (`prop_pos1`)**

    - Measures how often the model selects the first position as better.
    - A value near 0.5 suggests little or no positional bias.
    - Values substantially above 0.5 suggest a systematic preference for
      SAMPLE_1; values substantially below 0.5 suggest a preference for
      SAMPLE_2.

3.  **Binomial test p-value (`p_sample1_overall`)**

    - Tests the null hypothesis that the true probability of choosing
      SAMPLE_1 is 0.5.
    - Small p-values (e.g., \< 0.05) provide evidence of positional
      bias.
    - Large p-values indicate that any deviation from 0.5 may be due to
      random variation.

As an example, a row with:

- `prop_consistent = 0.93`  
- `prop_pos1 = 0.48`  
- `p_sample1_overall = 0.57`

suggests:

- Very high reverse-order consistency.  
- No strong evidence of a first-position bias (probability of choosing
  SAMPLE_1 is not significantly different from 0.5).

By contrast, a row with:

- `prop_consistent = 0.83`  
- `prop_pos1 = 0.42`  
- `p_sample1_overall = 0.001`

would suggest:

- Somewhat lower consistency.  
- A statistically significant bias *against* SAMPLE_1 (the model prefers
  SAMPLE_2).

------------------------------------------------------------------------

## 9. Summary results by prompt

In this section we present, for each template:

1.  The full template text (as used in the experiments).  

2.  A simple summary table with one row per (backend, model, thinking)
    configuration and columns:

    - `Backend`  
    - `Model`  
    - `Thinking`  
    - `Prop_Consistent`  
    - `Prop_SAMPLE_1`  
    - `Binomial_Test_p`

------------------------------------------------------------------------

### 9.1 Template `test1`

#### 9.1.1 Template text

``` r
cat(get_prompt_template("test1"))
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
#> 1.  **Advocate for SAMPLE_1**: Mentally list the single strongest point of evidence that makes SAMPLE_1 the winner.
#> 2.  **Advocate for SAMPLE_2**: Mentally list the single strongest point of evidence that makes SAMPLE_2 the winner.
#> 3.  **Adjudicate**: Compare the *strength of the evidence* identified in steps 1 and 2. Which sample provided the more compelling demonstration of the definition above?
#> 
#> CRITICAL:
#> - You must construct a mental argument for BOTH samples before deciding.
#> - Do not default to the first sample read.
#> - If the samples are close, strictly follow the trait definition to break the tie.
#> 
#> FINAL DECISION:
#> Output your decision based on the stronger evidence.
#> 
#> <BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>
#> OR
#> <BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>
#> 
#> (Provide only the XML tag).
```

#### 9.1.2 Summary table

``` r
summary_tbl |>
  filter(template_id == "test1") |>
  arrange(backend, model, thinking) |>
  mutate(
    Prop_Consistent = round(prop_consistent, 3),
    Prop_SAMPLE_1   = round(prop_pos1, 3),
    Binomial_Test_p = formatC(p_sample1_overall, format = "f", digits = 3)
  ) |>
  select(
    Backend = backend,
    Model = model,
    Thinking = thinking,
    Prop_Consistent,
    Prop_SAMPLE_1,
    Binomial_Test_p
  ) |>
  kable(
    align = c("l", "l", "l", "r", "r", "r")
  )
```

| Backend   | Model                         | Thinking      | Prop_Consistent | Prop_SAMPLE_1 | Binomial_Test_p |
|:----------|:------------------------------|:--------------|----------------:|--------------:|----------------:|
| anthropic | claude-haiku-4-5              | no_thinking   |           0.884 |         0.516 |           0.573 |
| anthropic | claude-haiku-4-5              | with_thinking |           0.905 |         0.484 |           0.573 |
| anthropic | claude-opus-4-5               | no_thinking   |           0.884 |         0.442 |           0.027 |
| anthropic | claude-opus-4-5               | with_thinking |           0.884 |         0.447 |           0.045 |
| anthropic | claude-sonnet-4-5             | no_thinking   |           0.895 |         0.505 |           0.878 |
| anthropic | claude-sonnet-4-5             | with_thinking |           0.932 |         0.497 |           0.959 |
| gemini    | gemini-3-pro-preview          | with_thinking |           0.926 |         0.521 |           0.442 |
| openai    | gpt-4.1                       | no_thinking   |           0.937 |         0.479 |           0.442 |
| openai    | gpt-4o                        | no_thinking   |           0.837 |         0.418 |           0.002 |
| openai    | gpt-5.1                       | no_thinking   |           0.926 |         0.474 |           0.330 |
| openai    | gpt-5.1                       | with_thinking |           0.858 |         0.429 |           0.006 |
| together  | DeepSeek-R1                   | with_thinking |           0.837 |         0.576 |           0.003 |
| together  | DeepSeek-V3                   | no_thinking   |           0.921 |         0.487 |           0.644 |
| together  | Kimi-K2-Instruct-0905         | no_thinking   |           0.889 |         0.455 |           0.090 |
| together  | Qwen3-235B-A22B-Instruct-2507 | no_thinking   |           0.821 |         0.416 |           0.001 |

------------------------------------------------------------------------

### 9.2 Template `test2`

#### 9.2.1 Template text

``` r
cat(get_prompt_template("test2"))
#> You are an impartial, expert writing evaluator. You will be provided with two student writing samples.
#> 
#> YOUR GOAL: Identify which sample is better regarding {TRAIT_NAME}.
#> 
#> ***
#> SAMPLE_1 START
#> ***
#> {SAMPLE_1}
#> ***
#> SAMPLE_1 END
#> ***
#> 
#> ***
#> SAMPLE_2 START
#> ***
#> {SAMPLE_2}
#> ***
#> SAMPLE_2 END
#> ***
#> 
#> EVALUATION CRITERIA:
#> Trait: {TRAIT_NAME}
#> Definition: {TRAIT_DESCRIPTION}
#> 
#> DECISION PROTOCOL:
#> 1. Ignore the order in which the samples appeared.
#> 2. Mentally 'shuffle' the samples. If Sample 1 was read second, would it still be better/worse?
#> 3. Focus STRICTLY on the definition above. Ignore length, vocabulary complexity, or style unless explicitly mentioned in the definition.
#> 4. If the samples are effectively tied, scrutinize them for the slightest advantage in {TRAIT_NAME} to break the tie.
#> 
#> OUTPUT FORMAT:
#> You must output ONLY one of the following tags. Do not produce any other text, reasoning, or preamble.
#> 
#> <BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>
#> or
#> <BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>
```

#### 9.2.2 Summary table

``` r
summary_tbl |>
  filter(template_id == "test2") |>
  arrange(backend, model, thinking) |>
  mutate(
    Prop_Consistent = round(prop_consistent, 3),
    Prop_SAMPLE_1   = round(prop_pos1, 3),
    Binomial_Test_p = formatC(p_sample1_overall, format = "f", digits = 3)
  ) |>
  select(
    Backend = backend,
    Model = model,
    Thinking = thinking,
    Prop_Consistent,
    Prop_SAMPLE_1,
    Binomial_Test_p
  ) |>
  kable(
    align = c("l", "l", "l", "r", "r", "r")
  )
```

| Backend   | Model                         | Thinking      | Prop_Consistent | Prop_SAMPLE_1 | Binomial_Test_p |
|:----------|:------------------------------|:--------------|----------------:|--------------:|----------------:|
| anthropic | claude-haiku-4-5              | no_thinking   |           0.863 |         0.442 |           0.027 |
| anthropic | claude-haiku-4-5              | with_thinking |           0.932 |         0.487 |           0.644 |
| anthropic | claude-opus-4-5               | no_thinking   |           0.895 |         0.458 |           0.112 |
| anthropic | claude-opus-4-5               | with_thinking |           0.926 |         0.474 |           0.330 |
| anthropic | claude-sonnet-4-5             | no_thinking   |           0.926 |         0.468 |           0.238 |
| anthropic | claude-sonnet-4-5             | with_thinking |           0.916 |         0.484 |           0.573 |
| gemini    | gemini-3-pro-preview          | with_thinking |           0.879 |         0.561 |           0.021 |
| openai    | gpt-4.1                       | no_thinking   |           0.932 |         0.466 |           0.200 |
| openai    | gpt-4o                        | no_thinking   |           0.884 |         0.442 |           0.027 |
| openai    | gpt-5.1                       | no_thinking   |           0.853 |         0.426 |           0.005 |
| openai    | gpt-5.1                       | with_thinking |           0.853 |         0.426 |           0.005 |
| together  | DeepSeek-R1                   | with_thinking |           0.916 |         0.511 |           0.720 |
| together  | DeepSeek-V3                   | no_thinking   |           0.874 |         0.563 |           0.016 |
| together  | Kimi-K2-Instruct-0905         | no_thinking   |           0.905 |         0.458 |           0.112 |
| together  | Qwen3-235B-A22B-Instruct-2507 | no_thinking   |           0.858 |         0.434 |           0.012 |

------------------------------------------------------------------------

### 9.3 Template `test3`

#### 9.3.1 Template text

``` r
cat(get_prompt_template("test3"))
#> You are an expert writing assessor.
#> 
#> Your task: Determine which of two writing samples demonstrates superior {TRAIT_NAME}.
#> 
#> {TRAIT_NAME} is defined as:
#> {TRAIT_DESCRIPTION}
#> 
#> Below are two samples. They appear in arbitrary order—neither position indicates quality.
#> 
#> ═══════════════════════════════════════
#> FIRST SAMPLE:
#> {SAMPLE_1}
#> 
#> ═══════════════════════════════════════
#> SECOND SAMPLE:
#> {SAMPLE_2}
#> 
#> ═══════════════════════════════════════
#> 
#> ASSESSMENT PROTOCOL:
#> 
#> Step 1: Read both samples in their entirety.
#> 
#> Step 2: For each sample independently, assess the degree to which it demonstrates {TRAIT_NAME} based solely on the definition provided.
#> 
#> Step 3: Compare your assessments. Determine which sample shows stronger {TRAIT_NAME}.
#> 
#> Step 4: Select the sample with better {TRAIT_NAME}. If extremely close, choose the one with any detectable advantage. No ties are allowed.
#> 
#> Step 5: Verify your selection reflects the CONTENT quality, not the presentation order.
#> 
#> RESPONSE FORMAT:
#> 
#> Respond with exactly one line using this format:
#> 
#> <BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>
#> 
#> if the first sample is better, OR
#> 
#> <BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>
#> 
#> if the second sample is better.
#> 
#> Output only the XML tag with your choice. No explanations or additional text.
```

#### 9.3.2 Summary table

``` r
summary_tbl |>
  filter(template_id == "test3") |>
  arrange(backend, model, thinking) |>
  mutate(
    Prop_Consistent = round(prop_consistent, 3),
    Prop_SAMPLE_1   = round(prop_pos1, 3),
    Binomial_Test_p = formatC(p_sample1_overall, format = "f", digits = 3)
  ) |>
  select(
    Backend = backend,
    Model = model,
    Thinking = thinking,
    Prop_Consistent,
    Prop_SAMPLE_1,
    Binomial_Test_p
  ) |>
  kable(
    align = c("l", "l", "l", "r", "r", "r")
  )
```

| Backend   | Model                         | Thinking      | Prop_Consistent | Prop_SAMPLE_1 | Binomial_Test_p |
|:----------|:------------------------------|:--------------|----------------:|--------------:|----------------:|
| anthropic | claude-haiku-4-5              | no_thinking   |           0.921 |         0.461 |           0.137 |
| anthropic | claude-haiku-4-5              | with_thinking |           0.916 |         0.463 |           0.166 |
| anthropic | claude-opus-4-5               | no_thinking   |           0.905 |         0.463 |           0.166 |
| anthropic | claude-opus-4-5               | with_thinking |           0.916 |         0.463 |           0.166 |
| anthropic | claude-sonnet-4-5             | no_thinking   |           0.884 |         0.453 |           0.072 |
| anthropic | claude-sonnet-4-5             | with_thinking |           0.937 |         0.489 |           0.720 |
| gemini    | gemini-3-pro-preview          | with_thinking |           0.911 |         0.545 |           0.090 |
| openai    | gpt-4.1                       | no_thinking   |           0.916 |         0.458 |           0.112 |
| openai    | gpt-4o                        | no_thinking   |           0.832 |         0.416 |           0.001 |
| openai    | gpt-5.1                       | no_thinking   |           0.879 |         0.445 |           0.035 |
| openai    | gpt-5.1                       | with_thinking |           0.863 |         0.432 |           0.009 |
| together  | DeepSeek-R1                   | with_thinking |           0.953 |         0.487 |           0.644 |
| together  | DeepSeek-V3                   | no_thinking   |           0.884 |         0.453 |           0.072 |
| together  | Kimi-K2-Instruct-0905         | no_thinking   |           0.879 |         0.455 |           0.090 |
| together  | Qwen3-235B-A22B-Instruct-2507 | no_thinking   |           0.805 |         0.408 |           0.000 |

------------------------------------------------------------------------

### 9.4 Template `test4`

#### 9.4.1 Template text

``` r
cat(get_prompt_template("test4"))
#> You are an expert writing assessor.
#> 
#> Evaluate which sample better demonstrates {TRAIT_NAME}.
#> 
#> {TRAIT_NAME}: {TRAIT_DESCRIPTION}
#> 
#> ---
#> SAMPLE 1:
#> {SAMPLE_1}
#> 
#> ---
#> SAMPLE 2:
#> {SAMPLE_2}
#> 
#> ---
#> 
#> TASK:
#> - Assess both samples on {TRAIT_NAME} only
#> - Choose the sample with stronger {TRAIT_NAME}
#> - If nearly equal, select the marginally better one
#> 
#> The samples above appear in random order. Base your judgment only on which content better demonstrates {TRAIT_NAME}, not on position.
#> 
#> Respond with only one line:
#> 
#> <BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE> if Sample 1 is better
#> 
#> <BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE> if Sample 2 is better
```

#### 9.4.2 Summary table

``` r
summary_tbl |>
  filter(template_id == "test4") |>
  arrange(backend, model, thinking) |>
  mutate(
    Prop_Consistent = round(prop_consistent, 3),
    Prop_SAMPLE_1   = round(prop_pos1, 3),
    Binomial_Test_p = formatC(p_sample1_overall, format = "f", digits = 3)
  ) |>
  select(
    Backend = backend,
    Model = model,
    Thinking = thinking,
    Prop_Consistent,
    Prop_SAMPLE_1,
    Binomial_Test_p
  ) |>
  kable(
    align = c("l", "l", "l", "r", "r", "r")
  )
```

| Backend   | Model                         | Thinking      | Prop_Consistent | Prop_SAMPLE_1 | Binomial_Test_p |
|:----------|:------------------------------|:--------------|----------------:|--------------:|----------------:|
| anthropic | claude-haiku-4-5              | no_thinking   |           0.937 |         0.468 |           0.238 |
| anthropic | claude-haiku-4-5              | with_thinking |           0.937 |         0.474 |           0.328 |
| anthropic | claude-opus-4-5               | no_thinking   |           0.900 |         0.461 |           0.137 |
| anthropic | claude-opus-4-5               | with_thinking |           0.895 |         0.458 |           0.112 |
| anthropic | claude-sonnet-4-5             | no_thinking   |           0.911 |         0.461 |           0.137 |
| anthropic | claude-sonnet-4-5             | with_thinking |           0.900 |         0.482 |           0.505 |
| gemini    | gemini-3-pro-preview          | with_thinking |           0.916 |         0.542 |           0.112 |
| openai    | gpt-4.1                       | no_thinking   |           0.884 |         0.442 |           0.027 |
| openai    | gpt-4o                        | no_thinking   |           0.884 |         0.442 |           0.027 |
| openai    | gpt-5.1                       | no_thinking   |           0.858 |         0.429 |           0.006 |
| openai    | gpt-5.1                       | with_thinking |           0.832 |         0.416 |           0.001 |
| together  | DeepSeek-R1                   | with_thinking |           0.905 |         0.474 |           0.330 |
| together  | DeepSeek-V3                   | no_thinking   |           0.932 |         0.503 |           0.959 |
| together  | Kimi-K2-Instruct-0905         | no_thinking   |           0.942 |         0.503 |           0.959 |
| together  | Qwen3-235B-A22B-Instruct-2507 | no_thinking   |           0.768 |         0.384 |           0.000 |

------------------------------------------------------------------------

### 9.5 Template `test5`

#### 9.5.1 Template text

``` r
cat(get_prompt_template("test5"))
#> You are a critique-focused evaluator. Instead of looking for general quality, you will look for deviations from the ideal.
#> 
#> Target Trait: {TRAIT_NAME}
#> Ideal Standard: {TRAIT_DESCRIPTION}
#> 
#> SAMPLES:
#> 
#> >>> TEXT_BLOCK_1 (Refers to SAMPLE_1)
#> {SAMPLE_1}
#> 
#> >>> TEXT_BLOCK_2 (Refers to SAMPLE_2)
#> {SAMPLE_2}
#> 
#> EVALUATION METHOD (Gap Analysis):
#> 
#> 1. Scrutinize TEXT_BLOCK_1. Where does it fail, hesitate, or deviate from the Ideal Standard?
#> 2. Scrutinize TEXT_BLOCK_2. Where does it fail, hesitate, or deviate from the Ideal Standard?
#> 3. Compare the 'Distance from Ideal'. Which sample is closer to the definition provided?
#> 4. Select the sample with the FEWEST or LEAST SEVERE deficits regarding {TRAIT_NAME}.
#> 
#> IMPORTANT:
#> - Ignore the order of presentation.
#> - Focus purely on which text adheres more tightly to the definition.
#> - If both are excellent, select the one with the higher 'ceiling' (stronger peak performance).
#> 
#> FINAL SELECTION:
#> <BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>
#> or
#> <BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>
```

#### 9.5.2 Summary table

``` r
summary_tbl |>
  filter(template_id == "test5") |>
  arrange(backend, model, thinking) |>
  mutate(
    Prop_Consistent = round(prop_consistent, 3),
    Prop_SAMPLE_1   = round(prop_pos1, 3),
    Binomial_Test_p = formatC(p_sample1_overall, format = "f", digits = 3)
  ) |>
  select(
    Backend = backend,
    Model = model,
    Thinking = thinking,
    Prop_Consistent,
    Prop_SAMPLE_1,
    Binomial_Test_p
  ) |>
  kable(
    align = c("l", "l", "l", "r", "r", "r")
  )
```

| Backend   | Model                         | Thinking      | Prop_Consistent | Prop_SAMPLE_1 | Binomial_Test_p |
|:----------|:------------------------------|:--------------|----------------:|--------------:|----------------:|
| anthropic | claude-haiku-4-5              | no_thinking   |           0.905 |         0.463 |           0.166 |
| anthropic | claude-haiku-4-5              | with_thinking |           0.926 |         0.489 |           0.719 |
| anthropic | claude-opus-4-5               | no_thinking   |           0.874 |         0.447 |           0.045 |
| anthropic | claude-opus-4-5               | with_thinking |           0.926 |         0.489 |           0.720 |
| anthropic | claude-sonnet-4-5             | no_thinking   |           0.900 |         0.482 |           0.505 |
| anthropic | claude-sonnet-4-5             | with_thinking |           0.900 |         0.476 |           0.383 |
| gemini    | gemini-3-pro-preview          | with_thinking |           0.932 |         0.508 |           0.798 |
| openai    | gpt-4.1                       | no_thinking   |           0.911 |         0.476 |           0.383 |
| openai    | gpt-4o                        | no_thinking   |           0.863 |         0.463 |           0.166 |
| openai    | gpt-5.1                       | no_thinking   |           0.877 |         0.451 |           0.086 |
| openai    | gpt-5.1                       | with_thinking |           0.789 |         0.400 |           0.000 |
| together  | DeepSeek-R1                   | with_thinking |           0.847 |         0.497 |           0.959 |
| together  | DeepSeek-V3                   | no_thinking   |           0.811 |         0.484 |           0.573 |
| together  | Kimi-K2-Instruct-0905         | no_thinking   |           0.795 |         0.482 |           0.505 |
| together  | Qwen3-235B-A22B-Instruct-2507 | no_thinking   |           0.800 |         0.400 |           0.000 |

------------------------------------------------------------------------

## 10. Per-backend summary

It is often useful to examine positional-bias metrics **within each
backend** to see whether:

- certain models exhibit more positional bias than others,
- reasoning mode makes a difference,
- a backend shows overall higher or lower reverse-order consistency.

The tables below show, for each provider, the key statistics:

- **Prop_Consistent** — proportion of consistent decisions under pair
  reversal  
- **Prop_SAMPLE_1** — proportion of comparisons selecting SAMPLE_1  
- **Binomial_Test_p** — significance level for deviation from 0.5

Each row corresponds to a (template, model, thinking) configuration used
in testing.

------------------------------------------------------------------------

### 10.1 Anthropic models

``` r
summary_tbl |>
  filter(backend == "anthropic") |>
  arrange(template_id, model, thinking) |>
  mutate(
    Prop_Consistent = round(prop_consistent, 3),
    Prop_SAMPLE_1   = round(prop_pos1, 3),
    Binomial_Test_p = formatC(p_sample1_overall, format = "f", digits = 3)
  ) |>
  select(
    Template = template_id,
    Model    = model,
    Thinking = thinking,
    Prop_Consistent,
    Prop_SAMPLE_1,
    Binomial_Test_p
  ) |>
  kable(
    caption = "Anthropic: Positional-bias summary by template, model, and thinking configuration.",
    align = c("l", "l", "l", "r", "r", "r")
  )
```

| Template | Model             | Thinking      | Prop_Consistent | Prop_SAMPLE_1 | Binomial_Test_p |
|:---------|:------------------|:--------------|----------------:|--------------:|----------------:|
| test1    | claude-haiku-4-5  | no_thinking   |           0.884 |         0.516 |           0.573 |
| test1    | claude-haiku-4-5  | with_thinking |           0.905 |         0.484 |           0.573 |
| test1    | claude-opus-4-5   | no_thinking   |           0.884 |         0.442 |           0.027 |
| test1    | claude-opus-4-5   | with_thinking |           0.884 |         0.447 |           0.045 |
| test1    | claude-sonnet-4-5 | no_thinking   |           0.895 |         0.505 |           0.878 |
| test1    | claude-sonnet-4-5 | with_thinking |           0.932 |         0.497 |           0.959 |
| test2    | claude-haiku-4-5  | no_thinking   |           0.863 |         0.442 |           0.027 |
| test2    | claude-haiku-4-5  | with_thinking |           0.932 |         0.487 |           0.644 |
| test2    | claude-opus-4-5   | no_thinking   |           0.895 |         0.458 |           0.112 |
| test2    | claude-opus-4-5   | with_thinking |           0.926 |         0.474 |           0.330 |
| test2    | claude-sonnet-4-5 | no_thinking   |           0.926 |         0.468 |           0.238 |
| test2    | claude-sonnet-4-5 | with_thinking |           0.916 |         0.484 |           0.573 |
| test3    | claude-haiku-4-5  | no_thinking   |           0.921 |         0.461 |           0.137 |
| test3    | claude-haiku-4-5  | with_thinking |           0.916 |         0.463 |           0.166 |
| test3    | claude-opus-4-5   | no_thinking   |           0.905 |         0.463 |           0.166 |
| test3    | claude-opus-4-5   | with_thinking |           0.916 |         0.463 |           0.166 |
| test3    | claude-sonnet-4-5 | no_thinking   |           0.884 |         0.453 |           0.072 |
| test3    | claude-sonnet-4-5 | with_thinking |           0.937 |         0.489 |           0.720 |
| test4    | claude-haiku-4-5  | no_thinking   |           0.937 |         0.468 |           0.238 |
| test4    | claude-haiku-4-5  | with_thinking |           0.937 |         0.474 |           0.328 |
| test4    | claude-opus-4-5   | no_thinking   |           0.900 |         0.461 |           0.137 |
| test4    | claude-opus-4-5   | with_thinking |           0.895 |         0.458 |           0.112 |
| test4    | claude-sonnet-4-5 | no_thinking   |           0.911 |         0.461 |           0.137 |
| test4    | claude-sonnet-4-5 | with_thinking |           0.900 |         0.482 |           0.505 |
| test5    | claude-haiku-4-5  | no_thinking   |           0.905 |         0.463 |           0.166 |
| test5    | claude-haiku-4-5  | with_thinking |           0.926 |         0.489 |           0.719 |
| test5    | claude-opus-4-5   | no_thinking   |           0.874 |         0.447 |           0.045 |
| test5    | claude-opus-4-5   | with_thinking |           0.926 |         0.489 |           0.720 |
| test5    | claude-sonnet-4-5 | no_thinking   |           0.900 |         0.482 |           0.505 |
| test5    | claude-sonnet-4-5 | with_thinking |           0.900 |         0.476 |           0.383 |

Anthropic: Positional-bias summary by template, model, and thinking
configuration.

------------------------------------------------------------------------

### 10.2 Gemini models

``` r
summary_tbl |>
  filter(backend == "gemini") |>
  arrange(template_id, model, thinking) |>
  mutate(
    Prop_Consistent = round(prop_consistent, 3),
    Prop_SAMPLE_1   = round(prop_pos1, 3),
    Binomial_Test_p = formatC(p_sample1_overall, format = "f", digits = 3)
  ) |>
  select(
    Template = template_id,
    Model    = model,
    Thinking = thinking,
    Prop_Consistent,
    Prop_SAMPLE_1,
    Binomial_Test_p
  ) |>
  kable(
    caption = "Gemini: Positional-bias summary by template, model, and thinking configuration.",
    align = c("l", "l", "l", "r", "r", "r")
  )
```

| Template | Model                | Thinking      | Prop_Consistent | Prop_SAMPLE_1 | Binomial_Test_p |
|:---------|:---------------------|:--------------|----------------:|--------------:|----------------:|
| test1    | gemini-3-pro-preview | with_thinking |           0.926 |         0.521 |           0.442 |
| test2    | gemini-3-pro-preview | with_thinking |           0.879 |         0.561 |           0.021 |
| test3    | gemini-3-pro-preview | with_thinking |           0.911 |         0.545 |           0.090 |
| test4    | gemini-3-pro-preview | with_thinking |           0.916 |         0.542 |           0.112 |
| test5    | gemini-3-pro-preview | with_thinking |           0.932 |         0.508 |           0.798 |

Gemini: Positional-bias summary by template, model, and thinking
configuration.

------------------------------------------------------------------------

### 10.3 OpenAI models

``` r
summary_tbl |>
  filter(backend == "openai") |>
  arrange(template_id, model, thinking) |>
  mutate(
    Prop_Consistent = round(prop_consistent, 3),
    Prop_SAMPLE_1   = round(prop_pos1, 3),
    Binomial_Test_p = formatC(p_sample1_overall, format = "f", digits = 3)
  ) |>
  select(
    Template = template_id,
    Model    = model,
    Thinking = thinking,
    Prop_Consistent,
    Prop_SAMPLE_1,
    Binomial_Test_p
  ) |>
  kable(
    caption = "OpenAI: Positional-bias summary by template, model, and thinking configuration.",
    align = c("l", "l", "l", "r", "r", "r")
  )
```

| Template | Model   | Thinking      | Prop_Consistent | Prop_SAMPLE_1 | Binomial_Test_p |
|:---------|:--------|:--------------|----------------:|--------------:|----------------:|
| test1    | gpt-4.1 | no_thinking   |           0.937 |         0.479 |           0.442 |
| test1    | gpt-4o  | no_thinking   |           0.837 |         0.418 |           0.002 |
| test1    | gpt-5.1 | no_thinking   |           0.926 |         0.474 |           0.330 |
| test1    | gpt-5.1 | with_thinking |           0.858 |         0.429 |           0.006 |
| test2    | gpt-4.1 | no_thinking   |           0.932 |         0.466 |           0.200 |
| test2    | gpt-4o  | no_thinking   |           0.884 |         0.442 |           0.027 |
| test2    | gpt-5.1 | no_thinking   |           0.853 |         0.426 |           0.005 |
| test2    | gpt-5.1 | with_thinking |           0.853 |         0.426 |           0.005 |
| test3    | gpt-4.1 | no_thinking   |           0.916 |         0.458 |           0.112 |
| test3    | gpt-4o  | no_thinking   |           0.832 |         0.416 |           0.001 |
| test3    | gpt-5.1 | no_thinking   |           0.879 |         0.445 |           0.035 |
| test3    | gpt-5.1 | with_thinking |           0.863 |         0.432 |           0.009 |
| test4    | gpt-4.1 | no_thinking   |           0.884 |         0.442 |           0.027 |
| test4    | gpt-4o  | no_thinking   |           0.884 |         0.442 |           0.027 |
| test4    | gpt-5.1 | no_thinking   |           0.858 |         0.429 |           0.006 |
| test4    | gpt-5.1 | with_thinking |           0.832 |         0.416 |           0.001 |
| test5    | gpt-4.1 | no_thinking   |           0.911 |         0.476 |           0.383 |
| test5    | gpt-4o  | no_thinking   |           0.863 |         0.463 |           0.166 |
| test5    | gpt-5.1 | no_thinking   |           0.877 |         0.451 |           0.086 |
| test5    | gpt-5.1 | with_thinking |           0.789 |         0.400 |           0.000 |

OpenAI: Positional-bias summary by template, model, and thinking
configuration.

------------------------------------------------------------------------

### 10.4 TogetherAI-hosted models

``` r
summary_tbl |>
  filter(backend == "together") |>
  arrange(template_id, model, thinking) |>
  mutate(
    Prop_Consistent = round(prop_consistent, 3),
    Prop_SAMPLE_1   = round(prop_pos1, 3),
    Binomial_Test_p = formatC(p_sample1_overall, format = "f", digits = 3)
  ) |>
  select(
    Template = template_id,
    Model    = model,
    Thinking = thinking,
    Prop_Consistent,
    Prop_SAMPLE_1,
    Binomial_Test_p
  ) |>
  kable(
    caption = "TogetherAI: Positional-bias summary by template, model, and thinking configuration.",
    align = c("l", "l", "l", "r", "r", "r")
  )
```

| Template | Model                         | Thinking      | Prop_Consistent | Prop_SAMPLE_1 | Binomial_Test_p |
|:---------|:------------------------------|:--------------|----------------:|--------------:|----------------:|
| test1    | DeepSeek-R1                   | with_thinking |           0.837 |         0.576 |           0.003 |
| test1    | DeepSeek-V3                   | no_thinking   |           0.921 |         0.487 |           0.644 |
| test1    | Kimi-K2-Instruct-0905         | no_thinking   |           0.889 |         0.455 |           0.090 |
| test1    | Qwen3-235B-A22B-Instruct-2507 | no_thinking   |           0.821 |         0.416 |           0.001 |
| test2    | DeepSeek-R1                   | with_thinking |           0.916 |         0.511 |           0.720 |
| test2    | DeepSeek-V3                   | no_thinking   |           0.874 |         0.563 |           0.016 |
| test2    | Kimi-K2-Instruct-0905         | no_thinking   |           0.905 |         0.458 |           0.112 |
| test2    | Qwen3-235B-A22B-Instruct-2507 | no_thinking   |           0.858 |         0.434 |           0.012 |
| test3    | DeepSeek-R1                   | with_thinking |           0.953 |         0.487 |           0.644 |
| test3    | DeepSeek-V3                   | no_thinking   |           0.884 |         0.453 |           0.072 |
| test3    | Kimi-K2-Instruct-0905         | no_thinking   |           0.879 |         0.455 |           0.090 |
| test3    | Qwen3-235B-A22B-Instruct-2507 | no_thinking   |           0.805 |         0.408 |           0.000 |
| test4    | DeepSeek-R1                   | with_thinking |           0.905 |         0.474 |           0.330 |
| test4    | DeepSeek-V3                   | no_thinking   |           0.932 |         0.503 |           0.959 |
| test4    | Kimi-K2-Instruct-0905         | no_thinking   |           0.942 |         0.503 |           0.959 |
| test4    | Qwen3-235B-A22B-Instruct-2507 | no_thinking   |           0.768 |         0.384 |           0.000 |
| test5    | DeepSeek-R1                   | with_thinking |           0.847 |         0.497 |           0.959 |
| test5    | DeepSeek-V3                   | no_thinking   |           0.811 |         0.484 |           0.573 |
| test5    | Kimi-K2-Instruct-0905         | no_thinking   |           0.795 |         0.482 |           0.505 |
| test5    | Qwen3-235B-A22B-Instruct-2507 | no_thinking   |           0.800 |         0.400 |           0.000 |

TogetherAI: Positional-bias summary by template, model, and thinking
configuration.

------------------------------------------------------------------------

## 11. Conclusion

This vignette demonstrates a reproducible workflow for detecting and
quantifying positional bias in prompt templates.

Including the template text and summary statistics side by side allows
rapid inspection and informed template selection. Templates that show:

- consistently high `Prop_Consistent` (e.g., ≥ 0.90) across providers
  and models, and  
- `Prop_SAMPLE_1` close to 0.5 with non-significant `Binomial_Test_p`

are strong candidates for production scoring pipelines in `pairwiseLLM`.

------------------------------------------------------------------------

## 12. Citation

> Mercer, S. (2025). *Prompt template positional bias testing* \[R
> package vignette\]. In *pairwiseLLM: Pairwise comparison tools for
> large language model-based writing evaluation*.
> <https://doi.org/10.32614/CRAN.package.pairwiseLLM>
