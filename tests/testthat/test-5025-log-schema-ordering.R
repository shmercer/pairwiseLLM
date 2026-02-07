test_that("canonical log schemas follow the expected column order", {
  expected_step <- c(
    "step_id", "timestamp", "pair_id", "i", "j", "A", "B", "Y", "status",
    "round_id", "round_stage", "stage_quota", "stage_committed_before",
    "stage_committed_after", "round_committed_before", "round_committed_after",
    "is_explore_step", "explore_mode", "explore_reason", "candidate_starved",
    "fallback_used", "fallback_path", "starvation_reason",
    "n_candidates_generated", "n_candidates_after_hard_filters",
    "n_candidates_after_duplicates", "n_candidates_after_star_caps", "n_candidates_scored",
    "deg_i", "deg_j", "recent_deg_i", "recent_deg_j",
    "mu_i", "mu_j", "sigma_i", "sigma_j", "p_ij", "U0_ij",
    "star_cap_rejects", "star_cap_reject_items"
  )
  expected_round <- c(
    "round_id", "step_id_at_refit", "timestamp", "model_variant", "n_items",
    "total_pairs_done", "new_pairs_since_last_refit", "n_unique_pairs_seen",
    "proposed_pairs_mode", "starve_rate_since_last_refit", "fallback_rate_since_last_refit",
    "fallback_used_mode", "starvation_reason_mode",
    "mean_degree", "min_degree", "pos_balance_sd",
    "epsilon_mean", "epsilon_p2.5", "epsilon_p5", "epsilon_p50", "epsilon_p95", "epsilon_p97.5",
    "b_mean", "b_p2.5", "b_p5", "b_p50", "b_p95", "b_p97.5",
    "ts_sigma_mean", "ts_sigma_max", "ts_degree_sigma_corr", "ts_btl_theta_corr", "ts_btl_rank_spearman",
    "star_cap_rejects_since_last_refit", "star_cap_reject_rate_since_last_refit",
    "recent_deg_median_since_last_refit", "recent_deg_max_since_last_refit",
    "ci_width_median", "ci_width_p90", "ci_width_max",
    "near_tie_adj_frac", "near_tie_adj_count", "p_adj_median",
    "diagnostics_pass", "divergences", "max_rhat", "min_ess_bulk", "ess_bulk_required",
    "reliability_EAP", "theta_sd_eap",
    "rho_theta", "theta_corr_pass", "delta_sd_theta", "delta_sd_theta_pass",
    "rho_rank", "rho_rank_pass", "stop_decision", "stop_reason"
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
