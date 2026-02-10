# Summarize an adaptive state.

Summarize an adaptive state.

## Usage

``` r
summarize_adaptive(state)
```

## Arguments

- state:

  Adaptive state.

## Value

A one-row tibble with columns `n_items`, `steps_attempted`,
`committed_pairs`, `n_refits`, `last_stop_decision`, and
`last_stop_reason`.

## Details

Returns a compact run-level summary from canonical logs: attempted
steps, committed comparisons, refit count, and last stop
decision/reason. This is a pure view and does not recompute model
quantities.

## See also

[`adaptive_get_logs()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_get_logs.md),
[`base::print()`](https://rdrr.io/r/base/print.html)

Other adaptive ranking:
[`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md),
[`adaptive_rank_resume()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_resume.md),
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md),
[`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md),
[`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md)

## Examples

``` r
state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
summarize_adaptive(state)
#> # A tibble: 1 × 6
#>   n_items steps_attempted committed_pairs n_refits last_stop_decision
#>     <int>           <int>           <int>    <int> <lgl>             
#> 1       3               0               0        0 FALSE             
#> # ℹ 1 more variable: last_stop_reason <chr>
```
