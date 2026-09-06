# Adaptive round log accessor.

Adaptive round log accessor.

## Usage

``` r
adaptive_round_log(state, reconstruct_deferred = FALSE)
```

## Arguments

- state:

  Adaptive state.

- reconstruct_deferred:

  Logical; when `TRUE`, reconstruct deferred audit-only posterior
  summaries from stored refit payloads when available. By default,
  returns the canonical stored `round_log` without reconstruction.

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
  `realloc_to_local`, `phase_scope`, `phase_scope_set_id`,
  `phase_scope_n_items`.

- Coverage/imbalance: `mean_degree`, `min_degree`, `mean_degree_scope`,
  `min_degree_scope`, `pos_balance_sd`,
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
  `near_stop_active`, `reliability_EAP`, `reliability_EAP_scope`,
  `eap_reliability_min`, `eap_pass`, `eap_pass_scope`, `theta_sd_eap`,
  `theta_sd_eap_scope`, `rho_theta`, `rho_theta_scope`, `lag_eligible`,
  `lag_eligible_scope`, `theta_corr_min`, `theta_corr_pass`,
  `theta_corr_pass_scope`, `delta_sd_theta`, `delta_sd_theta_scope`,
  `theta_sd_rel_change_max`, `delta_sd_theta_pass`,
  `delta_sd_theta_pass_scope`, `rho_rank`, `rho_rank_scope`,
  `rank_spearman_min`, `rho_rank_pass`, `rho_rank_pass_scope`.

- Refit execution metadata: `mcmc_chains`, `mcmc_parallel_chains`,
  `mcmc_core_fraction`, `mcmc_cores_detected_physical`,
  `mcmc_cores_detected_logical`, `mcmc_threads_per_chain`,
  `mcmc_cmdstanr_version`.

- Stop output: `stop_decision`, `stop_reason`, `max_pairs_after_stop`,
  `pairs_committed_after_stop`.

## See also

[`adaptive_get_logs()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_get_logs.md),
[`summarize_refits()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_refits.md),
[`adaptive_rank_run_live()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_rank_run_live.md)

Other adaptive logs:
[`adaptive_get_logs()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_get_logs.md),
[`adaptive_item_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_item_log.md),
[`adaptive_results_history()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_results_history.md),
[`adaptive_step_log()`](https://shmercer.github.io/pairwiseLLM/reference/adaptive_step_log.md),
[`summarize_items()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_items.md),
[`summarize_refits()`](https://shmercer.github.io/pairwiseLLM/reference/summarize_refits.md)

## Examples

``` r
state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
adaptive_round_log(state)
#> # A tibble: 0 × 116
#> # ℹ 116 variables: refit_id <int>, round_id_at_refit <int>,
#> #   step_id_at_refit <int>, timestamp <dttm>, model_variant <chr>,
#> #   n_items <int>, total_pairs_done <int>, new_pairs_since_last_refit <int>,
#> #   new_active_pairs_since_last_refit <int>,
#> #   new_probe_pairs_since_last_refit <int>,
#> #   new_total_cross_pairs_since_last_refit <int>, n_unique_pairs_seen <int>,
#> #   proposed_pairs_mode <dbl>, starve_rate_since_last_refit <dbl>, …
```
