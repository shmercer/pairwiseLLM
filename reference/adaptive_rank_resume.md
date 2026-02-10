# Adaptive ranking resume

Resume a previously persisted adaptive pairing session.

## Usage

``` r
adaptive_rank_resume(session_dir, ...)
```

## Arguments

- session_dir:

  Directory containing session artifacts.

- ...:

  Reserved for future extensions; currently unused.

## Value

An `adaptive_state` object restored from disk.

## Details

This is a thin wrapper around
[`load_adaptive_session()`](https://shmercer.github.io/pairwiseLLM/reference/load_adaptive_session.md)
and performs schema and log-shape checks during load. Returned state
preserves canonical `step_log`, `round_log`, and `item_log` contents
used for adaptive auditability.

## See also

[`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md),
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md),
[`save_adaptive_session()`](https://shmercer.github.io/pairwiseLLM/reference/save_adaptive_session.md),
[`load_adaptive_session()`](https://shmercer.github.io/pairwiseLLM/reference/load_adaptive_session.md)

Other adaptive ranking:
[`adaptive_rank()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank.md),
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md),
[`adaptive_rank_start()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_start.md),
[`make_adaptive_judge_llm()`](https://shmercer.github.io/pairwiseLLM/reference/make_adaptive_judge_llm.md),
[`summarize_adaptive()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_adaptive.md)

## Examples

``` r
dir <- tempfile("pwllm-session-")
state <- adaptive_rank_start(c("a", "b", "c"), seed = 3)
save_adaptive_session(state, dir, overwrite = TRUE)
restored <- adaptive_rank_resume(dir)
summarize_adaptive(restored)
#> # A tibble: 1 × 6
#>   n_items steps_attempted committed_pairs n_refits last_stop_decision
#>     <int>           <int>           <int>    <int> <lgl>             
#> 1       3               0               0        0 FALSE             
#> # ℹ 1 more variable: last_stop_reason <chr>
```
