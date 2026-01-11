# Internal helpers for standardizing metrics outputs across runners

#' Create a 0-row metrics tibble with standardized columns
#'
#' @param se_probs Numeric vector of probabilities used for se quantiles.
#'
#' @return A 0-row tibble with the standardized metrics schema.
#' @keywords internal
.bt_metrics_template <- function(se_probs = c(0.5, 0.9, 0.95)) {
  se_probs <- as.numeric(se_probs)
  if (length(se_probs) < 1L || any(!is.finite(se_probs))) {
    se_probs <- c(0.5, 0.9, 0.95)
  }
  se_cols <- paste0("se_p", as.integer(round(100 * se_probs)))

  tmpl <- tibble::tibble(
    # Bookkeeping
    batch_index = integer(),
    round_index = integer(),
    stage = character(),
    stop = logical(),
    stop_reason = character(),

    # Allocation and pairing diagnostics
    allocation = character(),
    allocation_source = character(),
    within_batch_frac = double(),
    core_audit_frac = double(),
    round_size = integer(),
    n_pairs_proposed = integer(),
    n_results_total = integer(),
    n_new_ids = integer(),
    n_core_new = integer(),
    n_new_new = integer(),
    n_core_core = integer(),

    # Stop metrics (bt_stop_metrics)
    engine = character(),
    n_items = integer(),
    n_total_items = integer(),
    theta_sd = double(),
    se_mean = double(),
    se_max = double(),
    rel_se_mean = double(),
    rel_se_p90 = double(),
    reliability = double(),
    sepG = double(),
    item_misfit_prop = double(),
    judge_misfit_prop = double(),

    # Drift metrics (bt_drift_metrics with prefix core_)
    core_n = integer(),
    core_mean_abs_shift = numeric(),
    core_p90_abs_shift = numeric(),
    core_p95_abs_shift = numeric(),
    core_max_abs_shift = numeric(),
    core_mean_signed_shift = numeric(),
    core_theta_cor = numeric(),
    core_theta_spearman = numeric(),

    # Linking parameters
    linking_mode = character(),
    linking_reference = character(),
    linking_method = character(),
    linking_applied = logical(),
    linking_reason = character(),
    linking_a = numeric(),
    linking_b = numeric(),
    linking_n_core = integer(),
    linking_min_n = integer(),
    linking_threshold_r = numeric(),
    linking_threshold_p90 = numeric(),

    # Linking drift (baseline drift used to decide/apply linking)
    linking_n = integer(),
    linking_mean_abs_shift = numeric(),
    linking_p90_abs_shift = numeric(),
    linking_p95_abs_shift = numeric(),
    linking_max_abs_shift = numeric(),
    linking_mean_signed_shift = numeric(),
    linking_theta_cor = numeric(),
    linking_theta_spearman = numeric(),
    # Linking drift (post-linking, diagnostic)
    linking_post_n = integer(),
    linking_post_mean_abs_shift = numeric(),
    linking_post_p90_abs_shift = numeric(),
    linking_post_p95_abs_shift = numeric(),
    linking_post_max_abs_shift = numeric(),
    linking_post_mean_signed_shift = numeric(),
    linking_post_theta_cor = numeric(),
    linking_post_theta_spearman = numeric(),
    # PR7: Per-round pairing / graph diagnostics and stability metrics
    n_pairs_new = integer(),
    n_pairs_total = integer(),
    n_missing_better_id = integer(),
    degree_min = numeric(),
    degree_min_lcc = numeric(),
    largest_component_frac = numeric(),
    graph_healthy = logical(),

    # Effective stop gating thresholds (arguments may be NA; these reflect internal effective defaults)
    stop_min_degree_eff = integer(),
    stop_min_largest_component_frac_eff = double(),
    stop_gating_active = logical(),

    # Graph bottleneck (cheap mixing proxy)
    bridge_edge_count = integer(),
    bridge_edge_frac = double(),
    n_component_bridge_pairs_planned = integer(),
    n_component_bridge_pairs_valid = integer(),

    # Mixing guards / optional spectral diagnostics
    mix_ok = logical(),
    mix_checked_this_round = logical(),
    mix_streak = integer(),
    spectral_gap_est = double(),
    lambda2_est = double(),
    spectral_gap_iters = integer(),
    spectral_gap_converged = logical(),
    spectral_gap_warn = logical(),
    spectral_gap_when = character(),
    rms_theta_delta = numeric(),
    topk_overlap = numeric(),
    rank_corr = numeric(),
    n_matched = integer(),
    stability_streak = integer(),
    stability_pass = logical(),
    stage1_escalated = logical(),
    stage1_escalation_round = integer(),
    core_flip_applied = logical(),
    linking_flip_applied = logical(),
    linking_post_flip_applied = logical()
  )

  for (nm in se_cols) {
    if (!(nm %in% names(tmpl))) {
      tmpl[[nm]] <- double()
    }
  }

  tmpl
}

#' Align a metrics tibble to the standardized schema
#'
#' Ensures all standard columns exist (adding typed NA columns when missing),
#' and orders columns with the standardized schema first (keeping any extras).
#'
#' @param metrics A tibble/data.frame of metrics rows.
#' @param se_probs The `se_probs` used for computing se quantiles.
#'
#' @return A tibble with standardized columns.
#' @keywords internal
.bt_align_metrics <- function(metrics, se_probs = c(0.5, 0.9, 0.95)) {
  tmpl <- .bt_metrics_template(se_probs)
  m <- tibble::as_tibble(metrics)

  out <- dplyr::bind_rows(tmpl, m)

  extra <- setdiff(names(out), names(tmpl))
  out <- out[, c(names(tmpl), extra), drop = FALSE]

  out
}
