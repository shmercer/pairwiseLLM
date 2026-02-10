# Retrieve canonical adaptive logs.

Retrieve canonical adaptive logs.

## Usage

``` r
adaptive_get_logs(state)
```

## Arguments

- state:

  Adaptive state.

## Value

A named list with three elements:

- step_log:

  A tibble with one row per attempted step.

- round_log:

  A tibble with one row per BTL refit round.

- item_log:

  A list of per-refit item tibbles.

## Details

Returns the three canonical Adaptive logs as currently held in memory:
`step_log`, `round_log`, and `item_log`. These correspond to step
attempts, posterior refit rounds, and item-level refit summaries
respectively.

## See also

[`adaptive_step_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_step_log.md),
[`adaptive_round_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_round_log.md),
[`adaptive_item_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_item_log.md)

Other adaptive logs:
[`adaptive_item_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_item_log.md),
[`adaptive_results_history()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_results_history.md),
[`adaptive_round_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_round_log.md),
[`adaptive_step_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_step_log.md)

## Examples

``` r
state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
logs <- adaptive_get_logs(state)
names(logs)
#> [1] "step_log"  "round_log" "item_log" 
```
