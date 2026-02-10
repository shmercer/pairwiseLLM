# Adaptive round log accessor.

Adaptive round log accessor.

## Usage

``` r
adaptive_round_log(state)
```

## Arguments

- state:

  Adaptive state.

## Value

A tibble with one row per completed posterior refit round.

## Details

`round_log` is the canonical per-refit audit log for the adaptive
pairing workflow. Each row summarizes one Bayesian BTL refit and
includes diagnostics, reliability, and stopping-gate fields used to
justify stop decisions.

Core columns:

- Refit identity/state: `refit_id`, `round_id_at_refit`,
  `step_id_at_refit`, `timestamp`, `model_variant`, `n_items`,
  `total_pairs_done`, `new_pairs_since_last_refit`,
  `n_unique_pairs_seen`.

- Candidate health: `proposed_pairs_mode`,
  `starve_rate_since_last_refit`, `fallback_rate_since_last_refit`,
  `fallback_used_mode`, `starvation_reason_mode`.

- Identifiability/quota adaptation: `global_identified`,
  `global_identified_reliability_min`,
  `global_identified_rank_corr_min`, `long_quota_raw`,
  `long_quota_effective`, `long_quota_removed`, `realloc_to_mid`,
  `realloc_to_local`.

- Coverage/imbalance: `mean_degree`, `min_degree`, `pos_balance_sd`,
  `star_cap_rejects_since_last_refit`,
  `star_cap_reject_rate_since_last_refit`,
  `recent_deg_median_since_last_refit`,
  `recent_deg_max_since_last_refit`.

- Posterior parameter summaries: `epsilon_mean`/percentiles and
  `b_mean`/percentiles.

- Audit diagnostics: `ts_sigma_mean`, `ts_sigma_max`,
  `ts_degree_sigma_corr`, `ts_btl_theta_corr`, `ts_btl_rank_spearman`,
  `ci95_theta_width_*`, `near_tie_adj_frac`, `near_tie_adj_count`,
  `p_adj_median`, `cov_trace_theta`, `cov_logdet_diag_theta`,
  `post_sd_theta_p10`, `post_sd_theta_p50`, `post_sd_theta_p90`,
  `top20_boundary_entropy_*`, `nn_diff_sd_*`.

- Stopping diagnostics: `diagnostics_pass`,
  `diagnostics_divergences_pass`, `diagnostics_rhat_pass`,
  `diagnostics_ess_pass`, `divergences`, `divergences_max_allowed`,
  `max_rhat`, `max_rhat_allowed`, `min_ess_bulk`, `ess_bulk_required`,
  `near_stop_active`, `reliability_EAP`, `eap_reliability_min`,
  `eap_pass`, `theta_sd_eap`, `rho_theta`, `lag_eligible`,
  `theta_corr_min`, `theta_corr_pass`, `delta_sd_theta`,
  `theta_sd_rel_change_max`, `delta_sd_theta_pass`, `rho_rank`,
  `rank_spearman_min`, `rho_rank_pass`.

- Refit execution metadata: `mcmc_chains`, `mcmc_parallel_chains`,
  `mcmc_core_fraction`, `mcmc_cores_detected_physical`,
  `mcmc_cores_detected_logical`, `mcmc_threads_per_chain`,
  `mcmc_cmdstanr_version`.

- Stop output: `stop_decision`, `stop_reason`.

## See also

[`adaptive_get_logs()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_get_logs.md),
[`summarize_refits()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_refits.md),
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md)

Other adaptive logs:
[`adaptive_get_logs()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_get_logs.md),
[`adaptive_item_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_item_log.md),
[`adaptive_results_history()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_results_history.md),
[`adaptive_step_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_step_log.md)

## Examples

``` r
state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
adaptive_round_log(state)
#> # A tibble: 0 × 96
#> # ℹ 96 variables: refit_id <int>, round_id_at_refit <int>,
#> #   step_id_at_refit <int>, timestamp <dttm>, model_variant <chr>,
#> #   n_items <int>, total_pairs_done <int>, new_pairs_since_last_refit <int>,
#> #   n_unique_pairs_seen <int>, proposed_pairs_mode <dbl>,
#> #   starve_rate_since_last_refit <dbl>, fallback_rate_since_last_refit <dbl>,
#> #   fallback_used_mode <chr>, starvation_reason_mode <chr>,
#> #   global_identified <lgl>, global_identified_reliability_min <dbl>, …
```
