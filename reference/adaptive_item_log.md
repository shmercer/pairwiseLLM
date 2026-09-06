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

Item-level summaries are domain-explicit:

- `theta_raw_*`: raw/within-set posterior summaries (EAP, fixed
  quantiles, SD, rank) at the current refit.

- `theta_link_*`: linked/global posterior summaries (EAP, fixed
  quantiles, SD, rank) after transform application.

- During linking Phase A (`phase_scope = "phase_a_set"`), `theta_link_*`
  is typed `NA` by design.

- `phase_scope`, `phase_scope_set_id`, and `in_phase_scope` indicate
  which item domain is currently optimized.

## See also

[`adaptive_get_logs()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_get_logs.md),
[`summarize_items()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_items.md),
[`adaptive_round_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_round_log.md)

Other adaptive logs:
[`adaptive_get_logs()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_get_logs.md),
[`adaptive_results_history()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_results_history.md),
[`adaptive_round_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_round_log.md),
[`adaptive_step_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_step_log.md),
[`summarize_items()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_items.md),
[`summarize_refits()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_refits.md)

## Examples

``` r
state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
adaptive_item_log(state)
#> # A tibble: 0 × 27
#> # ℹ 27 variables: refit_id <int>, item_id <chr>, set_id <int>,
#> #   phase_scope <chr>, phase_scope_set_id <int>, in_phase_scope <lgl>,
#> #   is_hub_item <lgl>, is_spoke_item <lgl>, theta_raw_eap <dbl>,
#> #   theta_raw_p2.5 <dbl>, theta_raw_p5 <dbl>, theta_raw_p50 <dbl>,
#> #   theta_raw_p95 <dbl>, theta_raw_p97.5 <dbl>, theta_raw_sd <dbl>,
#> #   rank_raw <int>, theta_link_eap <dbl>, theta_link_p2.5 <dbl>,
#> #   theta_link_p5 <dbl>, theta_link_p50 <dbl>, theta_link_p95 <dbl>, …
```
