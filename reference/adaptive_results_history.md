# Adaptive results history in build_bt_data() format.

Adaptive results history in build_bt_data() format.

## Usage

``` r
adaptive_results_history(state, committed_only = TRUE)
```

## Arguments

- state:

  Adaptive state.

- committed_only:

  Use only committed comparisons.

## Value

A tibble with columns:

- object1:

  Character item id shown in position A.

- object2:

  Character item id shown in position B.

- result:

  Numeric outcome in `{0, 1}` where `1` means `object1` wins.

## Details

Converts adaptive step outcomes into the three-column format used by
[`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md)
(`object1`, `object2`, `result`). With `committed_only = TRUE`, only
committed steps (`pair_id` not missing) are retained. This preserves the
transactional invariant that invalid steps do not contribute to inferred
comparisons.

## See also

[`build_bt_data()`](https://shmercer.github.io/pairwiseLLM/reference/build_bt_data.md),
[`adaptive_step_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_step_log.md)

Other adaptive logs:
[`adaptive_get_logs()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_get_logs.md),
[`adaptive_item_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_item_log.md),
[`adaptive_round_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_round_log.md),
[`adaptive_step_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_step_log.md)

## Examples

``` r
state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
adaptive_results_history(state)
#> # A tibble: 0 × 3
#> # ℹ 3 variables: object1 <chr>, object2 <chr>, result <dbl>
```
