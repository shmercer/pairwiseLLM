# Adaptive item log accessor.

Adaptive item log accessor.

## Usage

``` r
adaptive_item_log(state, refit_id = NULL, stack = FALSE)
```

## Arguments

- state:

  Adaptive state.

- refit_id:

  Optional refit index.

- stack:

  When TRUE, stack all refits.

## Value

A tibble of item-level summaries. When `stack = FALSE`, one row per item
for the selected refit. When `stack = TRUE`, one row per item per refit
with `refit_id` identifying source refit.

## Details

`item_log` stores per-item posterior summaries by refit. The underlying
state stores a list of refit tables; this accessor can return one refit
table (default: most recent) or stack all refits into a single tibble.

## See also

[`adaptive_get_logs()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_get_logs.md),
[`summarize_items()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_items.md),
[`adaptive_round_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_round_log.md)

Other adaptive logs:
[`adaptive_get_logs()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_get_logs.md),
[`adaptive_results_history()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_results_history.md),
[`adaptive_round_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_round_log.md),
[`adaptive_step_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_step_log.md)

## Examples

``` r
state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
adaptive_item_log(state)
#> # A tibble: 0 × 13
#> # ℹ 13 variables: refit_id <int>, item_id <chr>, theta_mean <dbl>,
#> #   theta_p2.5 <dbl>, theta_p5 <dbl>, theta_p50 <dbl>, theta_p95 <dbl>,
#> #   theta_p97.5 <dbl>, theta_sd <dbl>, rank_mean <dbl>, degree <int>,
#> #   pos_count_A <int>, pos_count_B <int>
```
