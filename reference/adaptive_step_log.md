# Adaptive step log accessor.

Adaptive step log accessor.

## Usage

``` r
adaptive_step_log(state)
```

## Arguments

- state:

  Adaptive state.

## Value

A tibble with one row per attempted step, in execution order.

## Details

`step_log` is the canonical per-step audit log for the adaptive
workflow. It records candidate pipeline outcomes, selected pair/order,
and commit status. A step with invalid judge response keeps committed
fields as `NA` and must not update model state.

Core columns:

- Identity/outcome: `step_id`, `timestamp`, `pair_id`, `i`, `j`, `A`,
  `B`, `Y`, `status`.

- Routing/scheduling: `round_id`, `round_stage`, `pair_type`,
  `stage_committed_so_far`, `stage_quota`.

- Exposure/strata: `used_in_round_i`, `used_in_round_j`, `is_anchor_i`,
  `is_anchor_j`, `stratum_i`, `stratum_j`, `dist_stratum`.

- Candidate health: `is_explore_step`, `explore_mode`, `explore_reason`,
  `explore_rate_used`, `local_priority_mode`, `long_gate_pass`,
  `long_gate_reason`, `star_override_used`, `star_override_reason`,
  `candidate_starved`, `fallback_used`, `fallback_path`,
  `starvation_reason`.

- Candidate counts: `n_candidates_generated`,
  `n_candidates_after_hard_filters`, `n_candidates_after_duplicates`,
  `n_candidates_after_star_caps`, `n_candidates_scored`.

- Endpoint diagnostics: `deg_i`, `deg_j`, `recent_deg_i`,
  `recent_deg_j`, `mu_i`, `mu_j`, `sigma_i`, `sigma_j`, `p_ij`, `U0_ij`.

- Star-cap diagnostics: `star_cap_rejects`, `star_cap_reject_items`.

## See also

[`adaptive_get_logs()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_get_logs.md),
[`adaptive_round_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_round_log.md),
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md)

Other adaptive logs:
[`adaptive_get_logs()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_get_logs.md),
[`adaptive_item_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_item_log.md),
[`adaptive_results_history()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_results_history.md),
[`adaptive_round_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_round_log.md)

## Examples

``` r
state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
adaptive_step_log(state)
#> # A tibble: 0 × 51
#> # ℹ 51 variables: step_id <int>, timestamp <dttm>, pair_id <int>, i <int>,
#> #   j <int>, A <int>, B <int>, Y <int>, status <chr>, round_id <int>,
#> #   round_stage <chr>, pair_type <chr>, used_in_round_i <int>,
#> #   used_in_round_j <int>, is_anchor_i <lgl>, is_anchor_j <lgl>,
#> #   stratum_i <int>, stratum_j <int>, dist_stratum <int>,
#> #   stage_committed_so_far <int>, stage_quota <int>, is_explore_step <lgl>,
#> #   explore_mode <chr>, explore_reason <chr>, explore_rate_used <dbl>, …
```
