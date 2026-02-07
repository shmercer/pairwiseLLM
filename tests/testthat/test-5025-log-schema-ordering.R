test_that("canonical log schemas follow the expected column order", {
  expected_step <- c(
    "step_id", "timestamp", "pair_id", "i", "j", "A", "B", "Y", "status",
    "round_id", "round_stage", "pair_type", "used_in_round_i", "used_in_round_j",
    "is_anchor_i", "is_anchor_j", "stratum_i", "stratum_j", "dist_stratum",
    "stage_committed_so_far", "stage_quota",
    "is_explore_step", "explore_mode", "explore_reason", "candidate_starved",
    "fallback_used", "fallback_path", "starvation_reason",
    "n_candidates_generated", "n_candidates_after_hard_filters",
    "n_candidates_after_duplicates", "n_candidates_after_star_caps", "n_candidates_scored",
    "deg_i", "deg_j", "recent_deg_i", "recent_deg_j",
    "mu_i", "mu_j", "sigma_i", "sigma_j", "p_ij", "U0_ij",
    "star_cap_rejects", "star_cap_reject_items"
  )
  expected_round <- c(
    "refit_id", "round_id_at_refit", "step_id_at_refit", "timestamp", "model_variant", "n_items",
    "total_pairs_done", "new_pairs_since_last_refit", "n_unique_pairs_seen",
    "proposed_pairs_mode", "starve_rate_since_last_refit", "fallback_rate_since_last_refit",
    "fallback_used_mode", "starvation_reason_mode",
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
    "refit_id", "item_id", "theta_mean", "theta_p2.5", "theta_p5", "theta_p50",
    "theta_p95", "theta_p97.5", "theta_sd", "rank_mean", "degree", "pos_count_A", "pos_count_B"
  )
  expected_item_step <- c("step_id", "timestamp", "item_id", "mu", "sigma", "degree")

  expect_equal(names(pairwiseLLM:::schema_step_log), expected_step)
  expect_equal(names(pairwiseLLM:::schema_round_log), expected_round)
  expect_equal(pairwiseLLM:::.adaptive_item_log_columns(), expected_item)
  expect_equal(names(pairwiseLLM:::schema_item_step_log), expected_item_step)
})

test_that("log accessors preserve canonical column order", {
  state <- adaptive_rank_start(make_test_items(3))
  expect_equal(names(adaptive_step_log(state)), names(pairwiseLLM:::schema_step_log))
  expect_equal(names(adaptive_round_log(state)), names(pairwiseLLM:::schema_round_log))
  expect_equal(names(adaptive_item_log(state)), pairwiseLLM:::.adaptive_item_log_columns())
})
