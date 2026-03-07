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
    "set_i", "set_j", "is_cross_set", "is_probe_step", "link_spoke_id", "run_mode", "link_stage",
    "delta_spoke_estimate_pre", "delta_spoke_sd_pre", "dist_stratum_global",
    "posterior_win_prob_ij_pre", "posterior_win_prob_pre",
    "link_transform_policy", "link_transform_state", "cross_set_utility_pre",
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
    "phase_scope", "phase_scope_set_id", "phase_scope_n_items",
    "mean_degree", "min_degree", "mean_degree_scope", "min_degree_scope", "pos_balance_sd",
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
    "reliability_EAP", "reliability_EAP_scope", "eap_reliability_min", "eap_pass", "eap_pass_scope", "theta_sd_eap",
    "theta_sd_eap_scope", "rho_theta", "rho_theta_scope", "lag_eligible", "lag_eligible_scope",
    "theta_corr_min", "theta_corr_pass", "theta_corr_pass_scope",
    "delta_sd_theta", "delta_sd_theta_scope", "theta_sd_rel_change_max", "delta_sd_theta_pass",
    "delta_sd_theta_pass_scope", "rho_rank", "rho_rank_scope", "rank_spearman_min",
    "rho_rank_pass", "rho_rank_pass_scope",
    "mcmc_chains", "mcmc_parallel_chains", "mcmc_core_fraction",
    "mcmc_cores_detected_physical", "mcmc_cores_detected_logical",
    "mcmc_threads_per_chain", "mcmc_cmdstanr_version",
    "stop_decision", "stop_reason", "max_pairs_after_stop", "pairs_committed_after_stop"
  )
  expected_item <- c(
    "refit_id", "item_id", "set_id",
    "phase_scope", "phase_scope_set_id", "in_phase_scope", "is_hub_item", "is_spoke_item",
    "theta_raw_eap", "theta_raw_p2.5", "theta_raw_p5", "theta_raw_p50", "theta_raw_p95", "theta_raw_p97.5",
    "theta_raw_sd", "rank_raw",
    "theta_link_eap", "theta_link_p2.5", "theta_link_p5", "theta_link_p50", "theta_link_p95", "theta_link_p97.5",
    "theta_link_sd", "rank_link",
    "degree", "pos_count_A", "pos_count_B"
  )
  expected_item_step <- c("step_id", "timestamp", "item_id", "mu", "sigma", "degree")
  expected_link_stage <- c(
    "refit_id", "spoke_id", "hub_id", "link_transform_policy", "link_transform_state",
    "link_refit_mode", "hub_lock_mode", "hub_lock_kappa",
    "shift_only_theta_treatment", "shift_only_theta_treatment_resolved",
    "delta_spoke_mean", "delta_spoke_sd",
    "log_alpha_spoke_mean", "log_alpha_spoke_sd", "delta_change_lagged",
    "log_alpha_change_lagged", "delta_change_pass", "log_alpha_change_pass",
    "delta_sd_max_used", "delta_sd_pass", "log_alpha_sd_pass",
    "reliability_EAP_link", "reliability_stop_pass", "linking_identified",
    "lag_eligible", "rank_stability_lagged", "rank_stability_pass",
    "link_stop_eligible", "link_stop_pass", "transform_frozen",
    "transform_frozen_refit_id", "link_epoch_id", "ts_btl_rank_spearman",
    "ppc_brier_cross_active", "ppc_brier_cross_probe", "ppc_brier_cross",
    "hub_anchored", "scale_ready",
    "link_diagnostics_divergences", "link_diagnostics_max_rhat",
    "link_diagnostics_min_ess_bulk", "link_diagnostics_divergences_pass",
    "link_diagnostics_rhat_pass", "link_diagnostics_ess_pass",
    "escalation_consecutive_pass_count", "escalated_this_refit",
    "probe_brier_shift_only", "probe_brier_shift_scale", "probe_brier_delta",
    "log_alpha_spoke_sd_alt", "n_pairs_cross_set_done", "n_unique_cross_pairs_seen",
    "n_probe_pairs_since_last_refit",
    "n_cross_edges_active_since_last_refit",
    "n_cross_edges_probe_since_last_refit",
    "n_cross_edges_total_since_last_refit",
    "B_spoke_refit_budget", "B_spoke_refit_budget_source",
    "stage_target_anchor_link", "stage_target_long_link", "stage_target_mid_link",
    "stage_target_local_link", "stage_realized_anchor_link", "stage_realized_long_link",
    "stage_realized_mid_link", "stage_realized_local_link", "stage_shortfall_anchor_link",
    "stage_shortfall_long_link", "stage_shortfall_mid_link", "stage_shortfall_local_link",
    "stage_budget_unfilled",
    "quota_anchor_link", "quota_long_link", "quota_mid_link", "quota_local_link",
    "quota_long_link_raw", "quota_long_link_effective", "quota_long_link_removed",
    "quota_taper_applied", "quota_taper_spoke_id",
    "committed_anchor_link", "committed_long_link", "committed_mid_link", "committed_local_link",
    "concurrent_target_pairs",
    "concurrent_floor_pairs", "concurrent_floor_met", "concurrent_target_met",
    "active_item_count_hub", "active_item_count_spoke", "active_item_count_total",
    "var_mean_theta_global_active", "mean_var_theta_global_active",
    "it_logdet_start", "it_logdet_end", "it_trace_end", "it_n_pairs_accumulated",
    "coverage_bins_used",
    "coverage_source", "ppc_calibration_id", "cross_set_ppc_brier_max_used",
    "probe_panel_id", "probe_edges_planned", "probe_edges_realized",
    "probe_panel_shortfall", "probe_panel_reallocation_used", "probe_pred_cache_used",
    "lag_domain_key", "lag_domain_reset"
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

test_that("public log accessors cast linking categorical fields to constrained factors", {
  state <- adaptive_rank_start(make_test_items(3))
  step_log <- adaptive_step_log(state)
  logs <- adaptive_get_logs(state)

  expect_true(is.factor(step_log$run_mode))
  expect_true(is.factor(step_log$link_stage))
  expect_true(is.factor(step_log$link_transform_policy))
  expect_true(is.factor(step_log$link_transform_state))
  expect_true(is.factor(step_log$utility_mode))
  expect_true(is.factor(step_log$hub_lock_mode))
  expect_identical(
    levels(step_log$run_mode),
    c("within_set", "link_one_spoke", "link_multi_spoke", "link_probe_holdout", "link_probe")
  )
  expect_identical(levels(step_log$link_stage), c("anchor_link", "long_link", "mid_link", "local_link"))
  expect_identical(levels(step_log$link_transform_policy), c("auto", "fixed_shift_only", "fixed_shift_scale"))
  expect_identical(levels(step_log$link_transform_state), c("shift_only", "shift_scale"))
  expect_identical(
    levels(step_log$utility_mode),
    c("pairing_trueskill_u0", "linking_d_optimal")
  )
  expect_identical(levels(step_log$hub_lock_mode), c("hard_lock", "soft_lock", "free"))

  expect_true(is.factor(logs$link_stage_log$link_transform_policy))
  expect_true(is.factor(logs$link_stage_log$link_transform_state))
  expect_true(is.factor(logs$link_stage_log$link_refit_mode))
  expect_true(is.factor(logs$link_stage_log$hub_lock_mode))
  expect_identical(
    levels(logs$link_stage_log$link_transform_policy),
    c("auto", "fixed_shift_only", "fixed_shift_scale")
  )
  expect_identical(levels(logs$link_stage_log$link_transform_state), c("shift_only", "shift_scale"))
  expect_identical(levels(logs$link_stage_log$link_refit_mode), c("shift_only", "joint_refit"))
  expect_identical(levels(logs$link_stage_log$hub_lock_mode), c("hard_lock", "soft_lock", "free"))
})

test_that("public log accessors fail fast on invalid linking categorical values", {
  state <- adaptive_rank_start(make_test_items(3))
  state$step_log <- pairwiseLLM:::append_step_log(
    state$step_log,
    list(step_id = 1L, timestamp = Sys.time(), run_mode = "bad_mode")
  )
  expect_error(adaptive_step_log(state), "invalid levels")

  state2 <- adaptive_rank_start(make_test_items(3))
  state2$link_stage_log <- pairwiseLLM:::append_link_stage_log(
    pairwiseLLM:::new_link_stage_log(),
    list(
      refit_id = 1L,
      spoke_id = 2L,
      hub_id = 1L,
      link_transform_policy = "bad_mode",
      link_transform_state = "shift_only",
      link_refit_mode = "shift_only",
      hub_lock_mode = "soft_lock",
      reliability_EAP_link = 0.9,
      linking_identified = TRUE,
      link_stop_eligible = TRUE,
      link_stop_pass = TRUE,
      transform_frozen = TRUE,
      n_pairs_cross_set_done = 1L,
      n_unique_cross_pairs_seen = 1L,
      n_probe_pairs_since_last_refit = 1L,
      n_cross_edges_active_since_last_refit = 0L,
      n_cross_edges_probe_since_last_refit = 1L,
      n_cross_edges_total_since_last_refit = 1L,
      coverage_bins_used = 3L
    )
  )
  expect_error(adaptive_get_logs(state2), "invalid levels")
})

test_that("adaptive print helper/accessor guard branches are exercised", {
  plain <- pairwiseLLM:::.adaptive_cast_log_factors(
    tibble::tibble(a = "x"),
    specs = list(),
    log_name = "dummy"
  )
  expect_identical(names(plain), "a")

  no_match <- pairwiseLLM:::.adaptive_cast_log_factors(
    tibble::tibble(a = "x"),
    specs = list(run_mode = c("within_set")),
    log_name = "dummy"
  )
  expect_identical(no_match$a, "x")

  state <- adaptive_rank_start(make_test_items(3))
  state_missing_round <- state
  state_missing_round$round_log <- NULL
  expect_error(adaptive_get_logs(state_missing_round), "round_log")

  state_missing_item <- state
  state_missing_item$item_log <- NULL
  expect_error(adaptive_get_logs(state_missing_item), "item_log")

  expect_error(pairwiseLLM::summarize_adaptive(list()), "adaptive_state")

  cfg <- pairwiseLLM:::.adaptive_progress_config(
    progress = "steps",
    progress_redraw_every = 1L,
    progress_show_events = FALSE,
    progress_errors = FALSE
  )
  cfg$refit_pairs_target <- NA_integer_
  handle <- pairwiseLLM:::adaptive_progress_init(state, cfg)
  expect_true(is.list(handle))
  cfg$progress <- "none"
  handle2 <- pairwiseLLM:::adaptive_progress_update(handle, state, cfg)
  expect_identical(handle2, handle)
})
