# Validate frozen directed judgments for an adaptive panel

Validate one primary observation per ordered pair. No rows are dropped
or reordered; optional metadata columns are retained and do not
determine lookup. Subset to one panel and one primary judgment layer
before validation.

## Usage

``` r
validate_adaptive_replay(outcomes, item_ids, complete = TRUE)
```

## Arguments

- outcomes:

  Data frame with character `A_id`, character `B_id`, and binary `Y`
  (one means presented A wins). `Y` accepts logical, numeric zero/one,
  or character `"0"`/`"1"`; factors, missing values, and other values
  are rejected.

- item_ids:

  Unique non-blank character IDs for the active panel, with at least two
  items. IDs must match the adaptive state's item IDs.

- complete:

  Logical; require all `N * (N - 1)` ordered pairs, including both
  orientations of every unordered pair. Default `TRUE`. `FALSE` permits
  a partial table; requests for absent orientations still fail during
  replay.

## Value

A tibble retaining the input rows and metadata, with integer `Y`.

## See also

[`make_adaptive_judge_replay()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_replay.md)

Other adaptive ranking:
[`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md),
[`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md),
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md),
[`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md),
[`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md),
[`make_adaptive_judge_replay()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_replay.md),
[`summarize_adaptive()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_adaptive.md)

## Examples

``` r
outcomes <- data.frame(A_id = c("a", "b"), B_id = c("b", "a"), Y = c(1L, 1L))
validate_adaptive_replay(outcomes, item_ids = c("a", "b"))
#> # A tibble: 2 × 3
#>   A_id  B_id      Y
#>   <chr> <chr> <int>
#> 1 a     b         1
#> 2 b     a         1
```
