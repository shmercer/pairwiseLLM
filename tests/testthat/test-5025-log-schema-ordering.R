test_that("canonical log schemas follow the expected column order", {
  expected_step <- c(
    "step_id", "timestamp", "pair_id", "i", "j", "A", "B", "Y", "status",
    "round_id", "round_stage", "pair_type", "used_in_round_i", "used_in_round_j",
    "is_anchor_i", "is_anchor_j", "stratum_i", "stratum_j", "dist_stratum",
    "stage_committed_so_far", "stage_quota",
    "is_explore_step", "explore_mode", "explore_reason", "explore_rate_used",
    "local_priority_mode", "long_gate_pass", "long_gate_reason",
    "star_override_used", "star_override_reason", "candidate_starved",
    "fallback_used", "fallback_path", "starvation_reason",
    "n_candidates_generated", "n_candidates_after_hard_filters",
    "n_candidates_after_duplicates", "n_candidates_after_star_caps", "n_candidates_scored",
    "deg_i", "deg_j", "recent_deg_i", "recent_deg_j",
    "mu_i", "mu_j", "sigma_i", "sigma_j", "p_ij", "U0_ij",
    "star_cap_rejects", "star_cap_reject_items",
    "set_i", "set_j", "is_cross_set", "link_spoke_id", "run_mode", "link_stage",
    "delta_spoke_estimate_pre", "delta_spoke_sd_pre", "dist_stratum_global",
    "posterior_win_prob_pre", "link_transform_mode", "cross_set_utility_pre",
    "utility_mode", "log_alpha_spoke_estimate_pre", "log_alpha_spoke_sd_pre",
    "hub_lock_mode", "hub_lock_kappa"
  )
  expected_round <- c(
    "refit_id", "round_id_at_refit", "step_id_at_refit", "timestamp", "model_variant", "n_items",
    "total_pairs_done", "new_pairs_since_last_refit", "n_unique_pairs_seen",
    "proposed_pairs_mode", "starve_rate_since_last_refit", "fallback_rate_since_last_refit",
    "fallback_used_mode", "starvation_reason_mode",
    "global_identified", "global_identified_reliability_min", "global_identified_rank_corr_min",
    "long_quota_raw", "long_quota_effective", "long_quota_removed",
    "realloc_to_mid", "realloc_to_local",
    "mean_degree", "min_degree", "pos_balance_sd",
    "epsilon_mean", "epsilon_p2.5", "epsilon_p5", "epsilon_p50", "epsilon_p95", "epsilon_p97.5",
    "b_mean", "b_p2.5", "b_p5", "b_p50", "b_p95", "b_p97.5",
    "ts_sigma_mean", "ts_sigma_max", "ts_degree_sigma_corr", "ts_btl_theta_corr", "ts_btl_rank_spearman",
    "star_cap_rejects_since_last_refit", "star_cap_reject_rate_since_last_refit",
    "recent_deg_median_since_last_refit", "recent_deg_max_since_last_refit",
    "ci95_theta_width_mean", "ci95_theta_width_median", "ci95_theta_width_p90", "ci95_theta_width_max",
    "near_tie_adj_frac", "near_tie_adj_count", "p_adj_median",
    "cov_trace_theta", "cov_logdet_diag_theta",
    "post_sd_theta_p10", "post_sd_theta_p50", "post_sd_theta_p90",
    "top20_boundary_entropy_mean", "top20_boundary_entropy_p90",
    "nn_diff_sd_mean", "nn_diff_sd_p90",
    "diagnostics_pass", "diagnostics_divergences_pass", "diagnostics_rhat_pass", "diagnostics_ess_pass",
    "divergences", "divergences_max_allowed",
    "max_rhat", "max_rhat_allowed", "min_ess_bulk", "ess_bulk_required", "near_stop_active",
    "reliability_EAP", "eap_reliability_min", "eap_pass", "theta_sd_eap",
    "rho_theta", "lag_eligible", "theta_corr_min", "theta_corr_pass",
    "delta_sd_theta", "theta_sd_rel_change_max", "delta_sd_theta_pass",
    "rho_rank", "rank_spearman_min", "rho_rank_pass",
    "mcmc_chains", "mcmc_parallel_chains", "mcmc_core_fraction",
    "mcmc_cores_detected_physical", "mcmc_cores_detected_logical",
    "mcmc_threads_per_chain", "mcmc_cmdstanr_version",
    "stop_decision", "stop_reason"
  )
  expected_item <- c(
    "refit_id", "item_id", "set_id", "theta_raw_eap", "theta_global_eap", "theta_global_sd",
    "rank_global_eap", "is_hub_item", "is_spoke_item",
    "theta_mean", "theta_p2.5", "theta_p5", "theta_p50",
    "theta_p95", "theta_p97.5", "theta_sd", "rank_mean", "degree", "pos_count_A", "pos_count_B"
  )
  expected_item_step <- c("step_id", "timestamp", "item_id", "mu", "sigma", "degree")
  expected_link_stage <- c(
    "refit_id", "spoke_id", "hub_id", "link_transform_mode", "link_refit_mode",
    "hub_lock_mode", "hub_lock_kappa", "delta_spoke_mean", "delta_spoke_sd",
    "log_alpha_spoke_mean", "log_alpha_spoke_sd", "delta_change_lagged",
    "log_alpha_change_lagged", "delta_change_pass", "log_alpha_change_pass",
    "delta_sd_max_used", "delta_sd_pass", "log_alpha_sd_pass",
    "reliability_EAP_link", "reliability_stop_pass", "linking_identified",
    "lag_eligible", "rank_stability_lagged", "rank_stability_pass",
    "link_stop_eligible", "link_stop_pass", "ts_btl_rank_spearman", "ppc_mae_cross",
    "link_diagnostics_divergences", "link_diagnostics_max_rhat",
    "link_diagnostics_min_ess_bulk", "link_diagnostics_divergences_pass",
    "link_diagnostics_rhat_pass", "link_diagnostics_ess_pass",
    "escalated_this_refit", "n_pairs_cross_set_done", "n_unique_cross_pairs_seen",
    "n_cross_edges_since_last_refit",
    "quota_anchor_link", "quota_long_link", "quota_mid_link", "quota_local_link",
    "quota_long_link_raw", "quota_long_link_effective", "quota_long_link_removed",
    "quota_taper_applied", "quota_taper_spoke_id",
    "committed_anchor_link", "committed_long_link", "committed_mid_link", "committed_local_link",
    "concurrent_target_pairs",
    "concurrent_floor_pairs", "concurrent_floor_met", "concurrent_target_met",
    "active_item_count_hub", "active_item_count_spoke", "coverage_bins_used",
    "coverage_source"
  )

  expect_equal(names(pairwiseLLM:::schema_step_log), expected_step)
  expect_equal(names(pairwiseLLM:::schema_round_log), expected_round)
  expect_equal(names(pairwiseLLM:::schema_link_stage_log), expected_link_stage)
  expect_equal(pairwiseLLM:::.adaptive_item_log_columns(), expected_item)
  expect_equal(names(pairwiseLLM:::schema_item_step_log), expected_item_step)
})

test_that("log accessors preserve canonical column order", {
  state <- adaptive_rank_start(make_test_items(3))
  expect_equal(names(adaptive_step_log(state)), names(pairwiseLLM:::schema_step_log))
  expect_equal(names(adaptive_round_log(state)), names(pairwiseLLM:::schema_round_log))
  expect_equal(names(adaptive_item_log(state)), pairwiseLLM:::.adaptive_item_log_columns())
})
