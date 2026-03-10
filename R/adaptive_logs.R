# -------------------------------------------------------------------------
# Adaptive canonical log schemas and append helpers.
# -------------------------------------------------------------------------

schema_step_log <- c(
  step_id = "integer",
  timestamp = "POSIXct",
  pair_id = "integer",
  i = "integer",
  j = "integer",
  A = "integer",
  B = "integer",
  Y = "integer",
  status = "character",
  round_id = "integer",
  round_stage = "character",
  pair_type = "character",
  used_in_round_i = "integer",
  used_in_round_j = "integer",
  is_anchor_i = "logical",
  is_anchor_j = "logical",
  stratum_i = "integer",
  stratum_j = "integer",
  dist_stratum = "integer",
  stage_committed_so_far = "integer",
  stage_quota = "integer",
  is_explore_step = "logical",
  explore_mode = "character",
  explore_reason = "character",
  explore_rate_used = "double",
  local_priority_mode = "character",
  long_gate_pass = "logical",
  long_gate_reason = "character",
  star_override_used = "logical",
  star_override_reason = "character",
  candidate_starved = "logical",
  fallback_used = "character",
  fallback_path = "character",
  starvation_reason = "character",
  n_candidates_generated = "integer",
  n_candidates_after_route_filters = "integer",
  n_candidates_after_active_domain = "integer",
  n_candidates_after_stage_filters = "integer",
  n_candidates_after_exposure_filters = "integer",
  n_candidates_after_hard_filters = "integer",
  n_candidates_after_duplicates = "integer",
  n_candidates_after_star_caps = "integer",
  n_candidates_scored = "integer",
  hard_filter_collapse_stage = "character",
  deg_i = "integer",
  deg_j = "integer",
  recent_deg_i = "integer",
  recent_deg_j = "integer",
  mu_i = "double",
  mu_j = "double",
  sigma_i = "double",
  sigma_j = "double",
  p_ij = "double",
  U0_ij = "double",
  star_cap_rejects = "integer",
  star_cap_reject_items = "integer",
  set_i = "integer",
  set_j = "integer",
  is_cross_set = "logical",
  is_probe_step = "logical",
  is_holdout_probe_step = "logical",
  is_drift_probe_step = "logical",
  link_spoke_id = "integer",
  run_mode = "character",
  link_stage = "character",
  delta_spoke_estimate_pre = "double",
  delta_spoke_sd_pre = "double",
  dist_stratum_global = "integer",
  posterior_win_prob_ij_pre = "double",
  posterior_win_prob_pre = "double",
  link_transform_policy = "character",
  link_transform_state = "character",
  cross_set_utility_pre = "double",
  utility_mode = "character",
  log_alpha_spoke_estimate_pre = "double",
  log_alpha_spoke_sd_pre = "double",
  hub_lock_mode = "character",
  hub_lock_kappa = "double"
)

schema_round_log <- c(
  refit_id = "integer",
  round_id_at_refit = "integer",
  step_id_at_refit = "integer",
  timestamp = "POSIXct",
  model_variant = "character",
  n_items = "integer",
  total_pairs_done = "integer",
  new_pairs_since_last_refit = "integer",
  n_unique_pairs_seen = "integer",
  proposed_pairs_mode = "double",
  starve_rate_since_last_refit = "double",
  fallback_rate_since_last_refit = "double",
  fallback_used_mode = "character",
  starvation_reason_mode = "character",
  global_identified = "logical",
  global_identified_reliability_min = "double",
  global_identified_rank_corr_min = "double",
  long_quota_raw = "integer",
  long_quota_effective = "integer",
  long_quota_removed = "integer",
  realloc_to_mid = "integer",
  realloc_to_local = "integer",
  phase_scope = "character",
  phase_scope_set_id = "integer",
  phase_scope_n_items = "integer",
  mean_degree = "double",
  min_degree = "integer",
  mean_degree_scope = "double",
  min_degree_scope = "integer",
  pos_balance_sd = "double",
  epsilon_mean = "double",
  epsilon_p2.5 = "double",
  epsilon_p5 = "double",
  epsilon_p50 = "double",
  epsilon_p95 = "double",
  epsilon_p97.5 = "double",
  b_mean = "double",
  b_p2.5 = "double",
  b_p5 = "double",
  b_p50 = "double",
  b_p95 = "double",
  b_p97.5 = "double",
  ts_sigma_mean = "double",
  ts_sigma_max = "double",
  ts_degree_sigma_corr = "double",
  ts_btl_theta_corr = "double",
  ts_btl_rank_spearman = "double",
  star_cap_rejects_since_last_refit = "integer",
  star_cap_reject_rate_since_last_refit = "double",
  recent_deg_median_since_last_refit = "double",
  recent_deg_max_since_last_refit = "integer",
  ci95_theta_width_mean = "double",
  ci95_theta_width_median = "double",
  ci95_theta_width_p90 = "double",
  ci95_theta_width_max = "double",
  near_tie_adj_frac = "double",
  near_tie_adj_count = "integer",
  p_adj_median = "double",
  cov_trace_theta = "double",
  cov_logdet_diag_theta = "double",
  post_sd_theta_p10 = "double",
  post_sd_theta_p50 = "double",
  post_sd_theta_p90 = "double",
  top20_boundary_entropy_mean = "double",
  top20_boundary_entropy_p90 = "double",
  nn_diff_sd_mean = "double",
  nn_diff_sd_p90 = "double",
  diagnostics_pass = "logical",
  diagnostics_divergences_pass = "logical",
  diagnostics_rhat_pass = "logical",
  diagnostics_ess_pass = "logical",
  divergences = "integer",
  divergences_max_allowed = "integer",
  max_rhat = "double",
  max_rhat_allowed = "double",
  min_ess_bulk = "double",
  ess_bulk_required = "double",
  near_stop_active = "logical",
  reliability_EAP = "double",
  reliability_EAP_scope = "double",
  eap_reliability_min = "double",
  eap_pass = "logical",
  eap_pass_scope = "logical",
  theta_sd_eap = "double",
  theta_sd_eap_scope = "double",
  rho_theta = "double",
  rho_theta_scope = "double",
  lag_eligible = "logical",
  lag_eligible_scope = "logical",
  theta_corr_min = "double",
  theta_corr_pass = "logical",
  theta_corr_pass_scope = "logical",
  delta_sd_theta = "double",
  delta_sd_theta_scope = "double",
  theta_sd_rel_change_max = "double",
  delta_sd_theta_pass = "logical",
  delta_sd_theta_pass_scope = "logical",
  rho_rank = "double",
  rho_rank_scope = "double",
  rank_spearman_min = "double",
  rho_rank_pass = "logical",
  rho_rank_pass_scope = "logical",
  mcmc_chains = "integer",
  mcmc_parallel_chains = "integer",
  mcmc_core_fraction = "double",
  mcmc_cores_detected_physical = "integer",
  mcmc_cores_detected_logical = "integer",
  mcmc_threads_per_chain = "integer",
  mcmc_cmdstanr_version = "character",
  stop_decision = "logical",
  stop_reason = "character",
  max_pairs_after_stop = "integer",
  pairs_committed_after_stop = "integer"
)

schema_link_stage_log <- c(
  refit_id = "integer",
  spoke_id = "integer",
  hub_id = "integer",
  link_transform_policy = "character",
  link_transform_state = "character",
  link_refit_mode = "character",
  hub_lock_mode = "character",
  hub_lock_kappa = "double",
  shift_only_theta_treatment = "character",
  shift_only_theta_treatment_resolved = "character",
  delta_spoke_mean = "double",
  delta_spoke_sd = "double",
  log_alpha_spoke_mean = "double",
  log_alpha_spoke_sd = "double",
  delta_change_lagged = "double",
  log_alpha_change_lagged = "double",
  delta_change_pass = "logical",
  log_alpha_change_pass = "logical",
  delta_sd_max_used = "double",
  delta_sd_pass = "logical",
  log_alpha_sd_pass = "logical",
  reliability_EAP_link = "double",
  reliability_link_global = "double",
  reliability_stop_pass = "logical",
  linking_identified = "logical",
  lag_eligible = "logical",
  link_lag_eligible = "logical",
  link_min_refit_eligible = "logical",
  link_stop_gate_open = "logical",
  rank_stability_lagged = "double",
  rank_stability_pass = "logical",
  link_stop_eligible = "logical",
  stop_consecutive_pass_count = "integer",
  link_stop_pass = "logical",
  transform_frozen = "logical",
  transform_frozen_refit_id = "integer",
  link_epoch_id = "integer",
  ts_btl_rank_spearman = "double",
  ppc_brier_cross_active = "double",
  ppc_brier_cross_probe = "double",
  ppc_brier_cross = "double",
  hub_anchored = "logical",
  scale_ready = "logical",
  link_fit_method = "character",
  link_uncertainty_approximation = "character",
  link_diagnostics_divergences = "integer",
  link_diagnostics_max_rhat = "double",
  link_diagnostics_min_ess_bulk = "double",
  link_diagnostics_divergences_pass = "logical",
  link_diagnostics_rhat_pass = "logical",
  link_diagnostics_ess_pass = "logical",
  escalation_consecutive_pass_count = "integer",
  escalated_this_refit = "logical",
  probe_brier_shift_only = "double",
  probe_brier_shift_scale = "double",
  probe_brier_delta = "double",
  log_alpha_spoke_sd_alt = "double",
  n_pairs_cross_set_done = "integer",
  n_unique_cross_pairs_seen = "integer",
  n_probe_pairs_since_last_refit = "integer",
  n_cross_edges_active_since_last_refit = "integer",
  n_cross_edges_probe_since_last_refit = "integer",
  n_cross_edges_total_since_last_refit = "integer",
  B_spoke_refit_budget = "integer",
  B_spoke_refit_budget_source = "character",
  stage_target_anchor_link = "integer",
  stage_target_long_link = "integer",
  stage_target_mid_link = "integer",
  stage_target_local_link = "integer",
  feasible_stage_capacity_anchor_link = "integer",
  feasible_stage_capacity_long_link = "integer",
  feasible_stage_capacity_mid_link = "integer",
  feasible_stage_capacity_local_link = "integer",
  feasibility_budget_released = "integer",
  feasibility_reallocation_used = "logical",
  feasibility_reallocation_rule = "character",
  stage_realized_anchor_link = "integer",
  stage_realized_long_link = "integer",
  stage_realized_mid_link = "integer",
  stage_realized_local_link = "integer",
  stage_shortfall_anchor_link = "integer",
  stage_shortfall_long_link = "integer",
  stage_shortfall_mid_link = "integer",
  stage_shortfall_local_link = "integer",
  stage_reallocation_used = "logical",
  stage_reallocation_rule_used = "character",
  stage_budget_unfilled = "integer",
  quota_anchor_link = "integer",
  quota_long_link = "integer",
  quota_mid_link = "integer",
  quota_local_link = "integer",
  quota_long_link_raw = "integer",
  quota_long_link_effective = "integer",
  quota_long_link_removed = "integer",
  quota_taper_applied = "logical",
  quota_taper_spoke_id = "integer",
  long_link_taper_applied = "logical",
  stage_target_long_link_pre_taper = "integer",
  stage_target_long_link_post_taper = "integer",
  committed_anchor_link = "integer",
  committed_long_link = "integer",
  committed_mid_link = "integer",
  committed_local_link = "integer",
  concurrent_target_pairs = "integer",
  concurrent_floor_pairs = "integer",
  concurrent_floor_met = "logical",
  concurrent_target_met = "logical",
  concurrent_utility_mass = "double",
  concurrent_top_k_used = "integer",
  concurrent_candidate_count = "integer",
  active_item_count_hub = "integer",
  active_item_count_spoke = "integer",
  active_item_count_total = "integer",
  var_mean_theta_global_active = "double",
  mean_var_theta_global_active = "double",
  reliability_var_mu_epsilon_used = "double",
  reliability_total_var_epsilon_used = "double",
  it_logdet_start = "double",
  it_logdet_end = "double",
  it_trace_end = "double",
  it_n_pairs_accumulated = "integer",
  coverage_bins_used = "integer",
  coverage_source = "character",
  probe_panel_id = "character",
  N_spoke_phase_b_start = "integer",
  probe_edges_planned = "integer",
  probe_edges_realized = "integer",
  probe_panel_shortfall = "integer",
  probe_panel_reallocation_used = "logical",
  probe_pred_cache_used = "logical",
  blocker_probe_panel_shortfall_weight = "double",
  blocker_probe_brier_weight = "double",
  blocker_probe_pred_rmse_weight = "double",
  blocker_theta_global_rmse_weight = "double",
  blocker_delta_spoke_sd_weight = "double",
  blocker_reweighting_rule = "character",
  probe_brier = "double",
  probe_pred_rmse_lagged = "double",
  theta_global_rmse_scope = "character",
  theta_global_rmse_lagged = "double",
  probe_edges_min_for_stop_used = "integer",
  alternative_fit_method = "character",
  alternative_uncertainty_approximation = "character",
  alt_eval_active_edges = "integer",
  alt_eval_converged = "logical",
  probe_brier_delta_min_used = "double",
  logalpha_sd_guardrail_used = "double",
  link_transform_escalation_refits_required_used = "integer",
  probe_edges_count_toward_active_constraints_used = "logical",
  lag_domain_key = "character",
  lag_domain_reset = "logical"
)

schema_item_step_log <- c(
  step_id = "integer",
  timestamp = "POSIXct",
  item_id = "integer",
  mu = "double",
  sigma = "double",
  degree = "integer"
)

.adaptive_schema_empty_col <- function(type) {
  if (identical(type, "integer")) {
    return(integer())
  }
  if (identical(type, "double")) {
    return(double())
  }
  if (identical(type, "logical")) {
    return(logical())
  }
  if (identical(type, "character")) {
    return(character())
  }
  if (identical(type, "POSIXct")) {
    return(as.POSIXct(character(), tz = "UTC"))
  }
  rlang::abort("Unknown schema column type.")
}

.adaptive_schema_typed_na <- function(type) {
  if (identical(type, "integer")) {
    return(NA_integer_)
  }
  if (identical(type, "double")) {
    return(NA_real_)
  }
  if (identical(type, "logical")) {
    return(NA)
  }
  if (identical(type, "character")) {
    return(NA_character_)
  }
  if (identical(type, "POSIXct")) {
    return(as.POSIXct(NA, tz = "UTC"))
  }
  rlang::abort("Unknown schema column type.")
}

.adaptive_schema_empty_tbl <- function(schema) {
  if (is.null(names(schema)) || any(names(schema) == "")) {
    rlang::abort("`schema` must be a named list of column types.")
  }
  cols <- lapply(schema, .adaptive_schema_empty_col)
  tibble::as_tibble(cols)
}

.adaptive_is_integerish <- function(x) {
  if (is.integer(x)) {
    return(TRUE)
  }
  if (!is.numeric(x)) {
    return(FALSE)
  }
  if (any(is.na(x))) {
    return(FALSE)
  }
  all(x == as.integer(x))
}

#' @keywords internal
#' @noRd
append_canonical_row <- function(log_tbl, row, schema, allow_multirow = FALSE) {
  if (!is.data.frame(log_tbl)) {
    rlang::abort("`log_tbl` must be a data frame.")
  }
  log_tbl <- tibble::as_tibble(log_tbl)
  schema_names <- names(schema)
  if (is.null(schema_names) || any(schema_names == "")) {
    rlang::abort("`schema` must be a named list of column types.")
  }
  missing_cols <- setdiff(schema_names, names(log_tbl))
  extra_cols <- setdiff(names(log_tbl), schema_names)
  if (length(extra_cols) > 0L) {
    rlang::abort("`log_tbl` must not include non-canonical columns.")
  }
  if (length(missing_cols) > 0L) {
    for (col in missing_cols) {
      log_tbl[[col]] <- rep_len(.adaptive_schema_typed_na(schema[[col]]), nrow(log_tbl))
    }
  }
  log_tbl <- log_tbl[, schema_names, drop = FALSE]

  if (is.list(row) && !is.data.frame(row)) {
    row <- tibble::as_tibble(row)
  } else if (is.data.frame(row)) {
    row <- tibble::as_tibble(row)
  } else {
    rlang::abort("`row` must be a named list or data frame.")
  }

  n_rows <- nrow(row)
  if (!allow_multirow && n_rows != 1L) {
    rlang::abort("`row` must have exactly one row.")
  }
  if (allow_multirow && n_rows < 1L) {
    rlang::abort("`row` must have at least one row.")
  }

  unknown <- setdiff(names(row), schema_names)
  if (length(unknown) > 0L) {
    rlang::abort(paste0(
      "`row` has unknown columns: ",
      paste(unknown, collapse = ", "),
      "."
    ))
  }

  missing <- setdiff(schema_names, names(row))
  if (length(missing) > 0L) {
    for (col in missing) {
      row[[col]] <- rep_len(.adaptive_schema_typed_na(schema[[col]]), n_rows)
    }
  }

  for (col in schema_names) {
    type <- schema[[col]]
    value <- row[[col]]

    if (identical(type, "POSIXct")) {
      if (!inherits(value, "POSIXct")) {
        rlang::abort(paste0("`row$", col, "` must be POSIXct."))
      }
      next
    }

    before_na <- is.na(value)
    if (identical(type, "integer")) {
      if (!all(is.na(value)) && !.adaptive_is_integerish(value[!is.na(value)])) {
        rlang::abort(paste0("`row$", col, "` must be integer-like."))
      }
      value <- suppressWarnings(as.integer(value))
    } else if (identical(type, "double")) {
      value <- suppressWarnings(as.double(value))
    } else if (identical(type, "logical")) {
      value <- suppressWarnings(as.logical(value))
    } else if (identical(type, "character")) {
      value <- suppressWarnings(as.character(value))
    } else {
      rlang::abort("Unknown schema column type.")
    }

    if (any(!before_na & is.na(value))) {
      rlang::abort(paste0(
        "`row$", col, "` could not be coerced to ",
        type,
        " without introducing NA."
      ))
    }
    row[[col]] <- value
  }

  row <- row[, schema_names, drop = FALSE]
  dplyr::bind_rows(log_tbl, row)
}

#' @keywords internal
#' @noRd
new_step_log <- function(now_fn = function() Sys.time()) {
  force(now_fn)
  .adaptive_schema_empty_tbl(schema_step_log)
}

#' @keywords internal
#' @noRd
append_step_log <- function(step_log, row) {
  append_canonical_row(step_log, row, schema_step_log, allow_multirow = FALSE)
}

#' @keywords internal
#' @noRd
new_round_log <- function() {
  .adaptive_schema_empty_tbl(schema_round_log)
}

#' @keywords internal
#' @noRd
append_round_log <- function(round_log, row) {
  append_canonical_row(round_log, row, schema_round_log, allow_multirow = FALSE)
}

#' @keywords internal
#' @noRd
new_link_stage_log <- function() {
  .adaptive_schema_empty_tbl(schema_link_stage_log)
}

#' @keywords internal
#' @noRd
append_link_stage_log <- function(link_stage_log, rows) {
  append_canonical_row(link_stage_log, rows, schema_link_stage_log, allow_multirow = TRUE)
}

#' @keywords internal
#' @noRd
new_item_step_log <- function(items) {
  if (!is.data.frame(items)) {
    rlang::abort("`items` must be a data frame.")
  }
  items <- tibble::as_tibble(items)
  if (!"item_id" %in% names(items)) {
    rlang::abort("`items` must include `item_id`.")
  }
  .adaptive_schema_empty_tbl(schema_item_step_log)
}

#' @keywords internal
#' @noRd
append_item_step_log <- function(item_log, rows) {
  append_canonical_row(item_log, rows, schema_item_step_log, allow_multirow = TRUE)
}
