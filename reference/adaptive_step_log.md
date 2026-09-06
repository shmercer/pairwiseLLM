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
fields as `NA` and must not update model state. The selected endpoints
`i`/`j` are the pre-orientation item indices, while `A`/`B` are the
displayed / judged item indices after order assignment. `Y` is defined
relative to displayed order: `Y = 1` means `A` wins and `Y = 0` means
`B` wins. For cross-run audit and reuse, prefer the stable `*_id`
columns plus `unordered_key`/`ordered_key` rather than transient integer
item positions from the live state. Judge provenance, token counts, and
`raw_response_json` are canonical step-log outputs, with
`raw_response_json` stored as serialized character data rather than a
list-column.

Core columns:

- Identity/outcome: `step_id`, `timestamp`, `pair_id`, `i`, `j`, `i_id`,
  `j_id`, `A`, `B`, `A_id`, `B_id`, `unordered_key`, `ordered_key`, `Y`,
  `status`.

- Judge audit: `judge_backend`, `judge_model`, `judge_endpoint`,
  `judge_valid`, `judge_invalid_reason`, `llm_status_code`,
  `llm_error_message`, `llm_custom_id`, `prompt_tokens`,
  `completion_tokens`, `total_tokens`, `raw_response_json`.

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
[`adaptive_round_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_round_log.md),
[`summarize_items()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_items.md),
[`summarize_refits()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_refits.md)

## Examples

``` r
state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
adaptive_step_log(state)
#> # A tibble: 0 × 97
#> # ℹ 97 variables: step_id <int>, timestamp <dttm>, pair_id <int>, i <int>,
#> #   j <int>, i_id <chr>, j_id <chr>, A <int>, B <int>, A_id <chr>, B_id <chr>,
#> #   unordered_key <chr>, ordered_key <chr>, Y <int>, status <chr>,
#> #   judge_backend <chr>, judge_model <chr>, judge_endpoint <chr>,
#> #   judge_valid <lgl>, judge_invalid_reason <chr>, llm_status_code <int>,
#> #   llm_error_message <chr>, llm_custom_id <chr>, prompt_tokens <dbl>,
#> #   completion_tokens <dbl>, total_tokens <dbl>, raw_response_json <chr>, …
```
