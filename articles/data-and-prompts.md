# Data Schemas and Prompt Management

This article follows data from source samples to modeling inputs and
shows how prompt templates are managed. All examples are deterministic
and run without provider credentials.

## Samples and pairs

[`read_samples_df()`](https://shmercer.github.io/pairwiseLLM/reference/read_samples_df.md)
normalizes a data frame and
[`read_samples_dir()`](https://shmercer.github.io/pairwiseLLM/reference/read_samples_dir.md)
reads one text file per item. Both return an `ID`/`text` table suitable
for the fixed-pair workflow. Existing data frames can be used directly
when they already have those columns.

``` r

library(pairwiseLLM)

data("example_writing_samples", package = "pairwiseLLM")
samples <- read_samples_df(example_writing_samples[, c("ID", "text")])

pairs <- samples |>
  make_pairs() |>
  sample_pairs(n_pairs = 4L, seed = 10L) |>
  randomize_pair_order(seed = 11L)

pairs[, c("ID1", "text1", "ID2", "text2")]
#> # A tibble: 4 × 4
#>   ID1   text1                                                        ID2   text2
#>   <chr> <chr>                                                        <chr> <chr>
#> 1 S12   "Evaluating writing is challenging because no rubric can fu… S10   "The…
#> 2 S09   "Assessing writing is difficult because the construct is mu… S05   "Wri…
#> 3 S07   "Writing assessment is difficult because writing is a compl… S20   "Wri…
#> 4 S19   "Writing assessment is difficult because it asks us to quan… S16   "Ass…
```

The fixed-pair schema uses `ID1`, `text1`, `ID2`, and `text2`. Reversal
helpers preserve the unordered pair while changing presentation order.
Keep the identifiers: they are needed to map a provider’s `SAMPLE_1` or
`SAMPLE_2` decision back to `better_id`.

## Traits and prompt templates

Templates require all four placeholders: `{TRAIT_NAME}`,
`{TRAIT_DESCRIPTION}`, `{SAMPLE_1}`, and `{SAMPLE_2}`.
[`build_prompt()`](https://shmercer.github.io/pairwiseLLM/reference/build_prompt.md)
substitutes them but does not submit anything to a provider.

``` r

trait <- trait_description("overall_quality")
template <- set_prompt_template()

prompt <- build_prompt(
  template = template,
  trait_name = trait$name,
  trait_desc = trait$description,
  text1 = pairs$text1[[1]],
  text2 = pairs$text2[[1]]
)

substr(prompt, 1L, 180L)
#> [1] "You are a debate adjudicator. Your task is to weigh the comparative strengths of two writing samples regarding a specific trait.\n\nTRAIT: Overall Quality\nDEFINITION: Overall quality"
```

The named registry lasts only for the current R session. Registration
validates placeholders; `overwrite = FALSE` protects an existing name.
Persist a custom template by storing its text in your project and
registering it from a startup script, not by modifying package files.

``` r

example_name <- "task07_example"
register_prompt_template(example_name, template = template)
example_name %in% list_prompt_templates()
#> [1] TRUE
identical(get_prompt_template(example_name), template)
#> [1] TRUE

register_prompt_template(example_name, template = template, overwrite = TRUE)
remove_prompt_template(example_name)
```

[`set_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/set_prompt_template.md)
returns the built-in default or validates an inline/file template.
[`get_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/get_prompt_template.md)
resolves a user registration before a built-in template of the same
name.
[`remove_prompt_template()`](https://shmercer.github.io/pairwiseLLM/reference/remove_prompt_template.md)
removes only session registrations; it cannot delete built-in files.

## Provider and result schemas

The live wrappers accept the fixed-pair rows directly. Batch request
builders convert the same rows to provider request records, and their
matching parsers convert downloaded output back to package results. Do
not send a request table built for one provider to another provider’s
submission function.

The normalized result bundle retains pair identifiers and adds provider
metadata, visible content, optional `thoughts`, `better_sample`,
`better_id`, and token counts. Row-wise live calls return a list whose
valid `results`, unresolved `failed_pairs`, and attempt-level
`failed_attempts` components must be inspected separately. Batch parsers
may omit unsuccessful rows from their successful result table, so
preserve the provider output/error file and job registry for audit and
recovery.

``` text
samples (ID, text)
  -> pairs (ID1, text1, ID2, text2)
     -> live call ---------------------> normalized results
     -> provider request + batch output -> provider parser -> normalized results
```

## Modeling and adaptive branches

For frequentist Bradley–Terry or Elo models, start from rows with `ID1`,
`ID2`, and a valid `better_id` and call
[`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md)
or
[`build_elo_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_elo_data.md).
Invalid or missing winners are not valid outcomes; inspect failures
before modeling rather than silently treating them as ties.

``` r

data("example_writing_pairs", package = "pairwiseLLM")

bt_data <- build_bt_data(example_writing_pairs)
elo_data <- build_elo_data(example_writing_pairs)
bayes_data <- build_btl_results_data(example_writing_pairs)

names(bt_data)
#> [1] "object1" "object2" "result"
names(elo_data)
#> [1] "winner" "loser"
names(bayes_data)
#>  [1] "pair_uid"      "unordered_key" "ordered_key"   "A_id"         
#>  [5] "B_id"          "better_id"     "winner_pos"    "phase"        
#>  [9] "iter"          "received_at"   "backend"       "model"
```

Standalone Bayesian BTL uses the stricter canonical schema returned by
[`build_btl_results_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_btl_results_data.md);
see [Standalone Bayesian BTL with
CmdStan](https://shmercer.github.io/pairwiseLLM/articles/bayesian-btl.md).

Adaptive ranking is a separate branch.
[`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md)
starts from raw items and records each attempt in an adaptive state with
canonical step, round, link-stage, and item logs. A normalized
fixed-pair result table is not a drop-in replacement for an adaptive
session. Use
[`adaptive_results_history()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_results_history.md)
only when you need the committed adaptive outcomes in
[`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md)
format.

## Failure checks at schema boundaries

- Reject missing, blank, or duplicate sample IDs before pairing.
- Preserve the original pair IDs and provider `custom_id` through batch
  submission and parsing.
- Model only outcomes whose `better_id` matches one member of the pair.
- Treat `thoughts` and raw responses as sensitive submitted/returned
  text when deciding retention.
- Validate persisted adaptive sessions with
  [`validate_session_dir()`](https://shmercer.github.io/pairwiseLLM/reference/validate_session_dir.md)
  rather than editing `.rds` artifacts or coercing their schemas by
  hand.

## Related documentation

See [Getting
Started](https://shmercer.github.io/pairwiseLLM/articles/getting-started.md)
for the basic fixed-pair workflow, [Provider Controls and
Recovery](https://shmercer.github.io/pairwiseLLM/articles/provider-controls-and-recovery.md)
for live and batch failures, and [Guide: Adaptive
Pairing](https://shmercer.github.io/pairwiseLLM/articles/adaptive-pairing.md)
for adaptive state and logs.

## Citation

> Mercer, S. H. (2026). *Data schemas and prompt management* \[R package
> vignette\]. Comprehensive R Archive Network.
> <https://doi.org/10.32614/CRAN.package.pairwiseLLM>
