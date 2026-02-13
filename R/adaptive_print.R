# -------------------------------------------------------------------------
# Adaptive printing, summaries, and log accessors.
# -------------------------------------------------------------------------

.adaptive_item_log_columns <- function() {
  c(
    "refit_id",
    "item_id",
    "set_id",
    "theta_raw_eap",
    "theta_global_eap",
    "theta_global_sd",
    "rank_global_eap",
    "is_hub_item",
    "is_spoke_item",
    "theta_mean",
    "theta_p2.5",
    "theta_p5",
    "theta_p50",
    "theta_p95",
    "theta_p97.5",
    "theta_sd",
    "rank_mean",
    "degree",
    "pos_count_A",
    "pos_count_B"
  )
}

.adaptive_empty_item_log_tbl <- function() {
  tibble::tibble(
    refit_id = integer(),
    item_id = character(),
    set_id = integer(),
    theta_raw_eap = double(),
    theta_global_eap = double(),
    theta_global_sd = double(),
    rank_global_eap = integer(),
    is_hub_item = logical(),
    is_spoke_item = logical(),
    theta_mean = double(),
    theta_p2.5 = double(),
    theta_p5 = double(),
    theta_p50 = double(),
    theta_p95 = double(),
    theta_p97.5 = double(),
    theta_sd = double(),
    rank_mean = double(),
    degree = integer(),
    pos_count_A = integer(),
    pos_count_B = integer()
  )
}

.adaptive_item_log_na_value <- function(col) {
  if (col %in% c("refit_id", "set_id", "rank_global_eap", "degree", "pos_count_A", "pos_count_B")) {
    return(NA_integer_)
  }
  if (identical(col, "item_id")) {
    return(NA_character_)
  }
  if (col %in% c("is_hub_item", "is_spoke_item")) {
    return(NA)
  }
  NA_real_
}

.adaptive_link_item_raw_global_summaries <- function(state, ids, set_id, theta_mean, theta_sd) {
  controller <- .adaptive_controller_resolve(state)
  run_mode <- as.character(controller$run_mode %||% "within_set")
  is_link_mode <- run_mode %in% c("link_one_spoke", "link_multi_spoke")

  raw_mean <- as.double(theta_mean)
  raw_sd <- as.double(theta_sd)
  global_mean <- as.double(theta_mean)
  global_sd <- as.double(theta_sd)

  if (!isTRUE(is_link_mode)) {
    return(list(
      theta_raw_eap = raw_mean,
      theta_raw_sd = raw_sd,
      theta_global_eap = global_mean,
      theta_global_sd = global_sd
    ))
  }

  items <- state$items
  item_key <- as.character(items$item_id)
  global_key <- as.character(items$global_item_id)
  item_to_global <- stats::setNames(global_key, item_key)
  id_global <- as.character(item_to_global[ids])

  artifacts <- (state$linking$phase_a %||% list())$artifacts %||% list()
  for (spoke_key in names(artifacts)) {
    artifact <- artifacts[[spoke_key]] %||% NULL
    items_tbl <- tibble::as_tibble(artifact$items %||% tibble::tibble())
    if (nrow(items_tbl) < 1L ||
      !all(c("global_item_id", "theta_raw_mean", "theta_raw_sd") %in% names(items_tbl))) {
      next
    }
    map_mean <- stats::setNames(as.double(items_tbl$theta_raw_mean), as.character(items_tbl$global_item_id))
    map_sd <- stats::setNames(as.double(items_tbl$theta_raw_sd), as.character(items_tbl$global_item_id))
    idx <- match(id_global, names(map_mean))
    keep <- !is.na(idx)
    if (any(keep)) {
      matched <- id_global[keep]
      raw_mean[keep] <- as.double(map_mean[matched])
      raw_sd[keep] <- as.double(map_sd[matched])
    }
  }
  raw_sd[!is.finite(raw_sd) | raw_sd < 0] <- theta_sd[!is.finite(raw_sd) | raw_sd < 0]
  global_mean <- as.double(raw_mean)
  global_sd <- as.double(raw_sd)

  hub_id <- as.integer(controller$hub_id %||% 1L)
  link_stats <- controller$link_refit_stats_by_spoke %||% list()
  spoke_ids <- sort(unique(as.integer(set_id[as.integer(set_id) != hub_id])))
  for (spoke_id in spoke_ids) {
    spoke_idx <- which(as.integer(set_id) == as.integer(spoke_id))
    if (length(spoke_idx) < 1L) {
      next
    }
    stats_row <- link_stats[[as.character(spoke_id)]] %||% list()
    mode <- as.character(stats_row$link_transform_mode %||%
      .adaptive_link_transform_mode_for_spoke(controller, spoke_id))
    delta <- as.double(stats_row$delta_spoke_mean %||% 0)
    if (!is.finite(delta)) {
      delta <- 0
    }
    alpha <- 1
    if (identical(mode, "shift_scale")) {
      log_alpha <- as.double(stats_row$log_alpha_spoke_mean %||% 0)
      alpha <- if (is.finite(log_alpha)) exp(log_alpha) else 1
    }
    global_mean[spoke_idx] <- as.double(delta + alpha * raw_mean[spoke_idx])
    global_sd[spoke_idx] <- as.double(abs(alpha) * raw_sd[spoke_idx])
  }

  list(
    theta_raw_eap = as.double(raw_mean),
    theta_raw_sd = as.double(raw_sd),
    theta_global_eap = as.double(global_mean),
    theta_global_sd = as.double(global_sd)
  )
}

.adaptive_build_item_log_refit <- function(state, refit_id) {
  fit <- state$btl_fit %||% list()
  draws <- fit$btl_posterior_draws %||% NULL
  if (!is.matrix(draws) || !is.numeric(draws)) {
    return(.adaptive_empty_item_log_tbl())
  }

  ids <- as.character(state$item_ids)
  set_id <- as.integer(state$items$set_id[match(ids, as.character(state$items$item_id))])
  if (is.null(colnames(draws))) {
    colnames(draws) <- ids
  }
  draws <- draws[, ids, drop = FALSE]
  draws <- .pairwiseLLM_sanitize_draws_matrix(draws, name = "btl_posterior_draws")

  probs <- c(0.025, 0.05, 0.5, 0.95, 0.975)
  theta_mean <- as.double(colMeans(draws))
  theta_sd <- as.double(apply(draws, 2, stats::sd))
  theta_quantiles <- vapply(
    seq_len(ncol(draws)),
    function(idx) stats::quantile(draws[, idx], probs = probs, names = FALSE),
    numeric(length(probs))
  )

  rank_mat <- t(apply(draws, 1, function(row) rank(-row, ties.method = "average")))
  colnames(rank_mat) <- ids
  rank_mean <- as.double(colMeans(rank_mat))
  link_summary <- .adaptive_link_item_raw_global_summaries(
    state = state,
    ids = ids,
    set_id = set_id,
    theta_mean = theta_mean,
    theta_sd = theta_sd
  )
  rank_global_eap <- as.integer(rank(-as.double(link_summary$theta_global_eap), ties.method = "first"))
  hub_id <- as.integer((.adaptive_controller_resolve(state) %||% list())$hub_id %||% 1L)
  is_hub_item <- as.logical(set_id == hub_id)
  is_spoke_item <- as.logical(set_id != hub_id)

  counts <- .adaptive_pair_counts(.adaptive_history_tbl(state), ids)
  degree <- as.integer(counts$deg[ids])
  pos_count_A <- as.integer(counts$posA[ids])
  pos_count_B <- as.integer(counts$posB[ids])

  tibble::tibble(
    refit_id = as.integer(refit_id),
    item_id = as.character(ids),
    set_id = as.integer(set_id),
    theta_raw_eap = as.double(link_summary$theta_raw_eap),
    theta_global_eap = as.double(link_summary$theta_global_eap),
    theta_global_sd = as.double(link_summary$theta_global_sd),
    rank_global_eap = as.integer(rank_global_eap),
    is_hub_item = as.logical(is_hub_item),
    is_spoke_item = as.logical(is_spoke_item),
    theta_mean = as.double(theta_mean),
    theta_p2.5 = as.double(theta_quantiles[1L, ]),
    theta_p5 = as.double(theta_quantiles[2L, ]),
    theta_p50 = as.double(theta_quantiles[3L, ]),
    theta_p95 = as.double(theta_quantiles[4L, ]),
    theta_p97.5 = as.double(theta_quantiles[5L, ]),
    theta_sd = as.double(theta_sd),
    rank_mean = as.double(rank_mean),
    degree = as.integer(degree),
    pos_count_A = as.integer(pos_count_A),
    pos_count_B = as.integer(pos_count_B)
  )
}

.adaptive_canonicalize_item_log <- function(item_log, state, refit_id = NULL) {
  if (!is.data.frame(item_log)) {
    rlang::abort("`item_log` entries must be data frames.")
  }
  item_log <- tibble::as_tibble(item_log)

  if (!"item_id" %in% names(item_log) && "ID" %in% names(item_log)) {
    item_log$item_id <- as.character(item_log$ID)
  }

  if (!"degree" %in% names(item_log) && "deg" %in% names(item_log)) {
    item_log$degree <- as.integer(item_log$deg)
  }

  if (!"refit_id" %in% names(item_log)) {
    item_log$refit_id <- as.integer(refit_id %||% NA_integer_)
  }

  ids <- as.character(state$item_ids)
  counts <- .adaptive_pair_counts(.adaptive_history_tbl(state), ids)
  idx <- match(as.character(item_log$item_id), ids)
  if (!"set_id" %in% names(item_log)) {
    state_set <- state$items$set_id[match(ids, as.character(state$items$item_id))]
    item_log$set_id <- as.integer(state_set[idx])
  }
  if (!"pos_count_A" %in% names(item_log)) {
    item_log$pos_count_A <- as.integer(counts$posA[idx])
  }
  if (!"pos_count_B" %in% names(item_log)) {
    item_log$pos_count_B <- as.integer(counts$posB[idx])
  }
  if (!"degree" %in% names(item_log)) {
    item_log$degree <- as.integer(counts$deg[idx])
  }

  missing <- setdiff(.adaptive_item_log_columns(), names(item_log))
  if (length(missing) > 0L) {
    for (col in missing) {
      item_log[[col]] <- rep_len(.adaptive_item_log_na_value(col), nrow(item_log))
    }
  }

  item_log <- item_log[, .adaptive_item_log_columns(), drop = FALSE]
  item_log$refit_id <- as.integer(item_log$refit_id)
  item_log$item_id <- as.character(item_log$item_id)
  item_log$set_id <- as.integer(item_log$set_id)
  item_log$theta_raw_eap <- as.double(item_log$theta_raw_eap)
  item_log$theta_global_eap <- as.double(item_log$theta_global_eap)
  item_log$theta_global_sd <- as.double(item_log$theta_global_sd)
  item_log$rank_global_eap <- as.integer(item_log$rank_global_eap)
  item_log$is_hub_item <- as.logical(item_log$is_hub_item)
  item_log$is_spoke_item <- as.logical(item_log$is_spoke_item)
  item_log$theta_mean <- as.double(item_log$theta_mean)
  item_log$theta_p2.5 <- as.double(item_log$theta_p2.5)
  item_log$theta_p5 <- as.double(item_log$theta_p5)
  item_log$theta_p50 <- as.double(item_log$theta_p50)
  item_log$theta_p95 <- as.double(item_log$theta_p95)
  item_log$theta_p97.5 <- as.double(item_log$theta_p97.5)
  item_log$theta_sd <- as.double(item_log$theta_sd)
  item_log$rank_mean <- as.double(item_log$rank_mean)
  item_log$degree <- as.integer(item_log$degree)
  item_log$pos_count_A <- as.integer(item_log$pos_count_A)
  item_log$pos_count_B <- as.integer(item_log$pos_count_B)
  tibble::as_tibble(item_log)
}

.adaptive_append_item_log <- function(state, item_log_tbl) {
  if (is.null(item_log_tbl) || nrow(item_log_tbl) == 0L) {
    return(state)
  }
  item_logs <- state$item_log
  if (is.null(item_logs)) {
    item_logs <- list()
  }
  if (!is.list(item_logs)) {
    rlang::abort("`state$item_log` must be a list.")
  }
  state$item_log <- c(item_logs, list(item_log_tbl))
  state
}

#' Retrieve canonical adaptive logs.
#'
#' @details
#' Returns the canonical adaptive logs as currently held in memory:
#' \code{step_log}, \code{round_log}, \code{item_log}, and
#' \code{link_stage_log}. These correspond to step attempts, posterior refit
#' rounds, item-level refit summaries, and per-refit linking summaries.
#'
#' @param state Adaptive state.
#'
#' @return A named list with four elements:
#' \describe{
#'   \item{step_log}{A tibble with one row per attempted step.}
#'   \item{round_log}{A tibble with one row per BTL refit round.}
#'   \item{item_log}{A list of per-refit item tibbles.}
#'   \item{link_stage_log}{A tibble with one row per \code{(refit_id, spoke_id)}
#'   linking summary when linking mode is active.}
#' }
#'
#' @examples
#' state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
#' logs <- adaptive_get_logs(state)
#' names(logs)
#'
#' @seealso [adaptive_step_log()], [adaptive_round_log()], [adaptive_item_log()]
#'
#' @family adaptive logs
#' @export
adaptive_get_logs <- function(state) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }
  if (is.null(state$step_log)) {
    rlang::abort("`state$step_log` is missing.")
  }
  if (is.null(state$round_log)) {
    rlang::abort("`state$round_log` is missing.")
  }
  if (is.null(state$item_log)) {
    rlang::abort("`state$item_log` is missing.")
  }
  item_log <- if (length(state$item_log) == 0L) {
    list()
  } else {
    lapply(seq_along(state$item_log), function(idx) {
      .adaptive_canonicalize_item_log(state$item_log[[idx]], state, refit_id = idx)
    })
  }
  list(
    step_log = tibble::as_tibble(state$step_log),
    round_log = tibble::as_tibble(state$round_log),
    item_log = item_log,
    link_stage_log = tibble::as_tibble(state$link_stage_log %||% new_link_stage_log())
  )
}

#' Adaptive step log accessor.
#'
#' @details
#' \code{step_log} is the canonical per-step audit log for the adaptive
#' workflow. It records candidate pipeline outcomes, selected pair/order, and
#' commit status. A step with invalid judge response keeps committed fields
#' as \code{NA} and must not update model state.
#'
#' Core columns:
#' \itemize{
#'   \item Identity/outcome: \code{step_id}, \code{timestamp}, \code{pair_id},
#'   \code{i}, \code{j}, \code{A}, \code{B}, \code{Y}, \code{status}.
#'   \item Routing/scheduling: \code{round_id}, \code{round_stage},
#'   \code{pair_type}, \code{stage_committed_so_far}, \code{stage_quota}.
#'   \item Exposure/strata: \code{used_in_round_i}, \code{used_in_round_j},
#'   \code{is_anchor_i}, \code{is_anchor_j}, \code{stratum_i},
#'   \code{stratum_j}, \code{dist_stratum}.
#'   \item Candidate health: \code{is_explore_step}, \code{explore_mode},
#'   \code{explore_reason}, \code{explore_rate_used},
#'   \code{local_priority_mode}, \code{long_gate_pass},
#'   \code{long_gate_reason}, \code{star_override_used},
#'   \code{star_override_reason}, \code{candidate_starved},
#'   \code{fallback_used}, \code{fallback_path}, \code{starvation_reason}.
#'   \item Candidate counts: \code{n_candidates_generated},
#'   \code{n_candidates_after_hard_filters}, \code{n_candidates_after_duplicates},
#'   \code{n_candidates_after_star_caps}, \code{n_candidates_scored}.
#'   \item Endpoint diagnostics: \code{deg_i}, \code{deg_j},
#'   \code{recent_deg_i}, \code{recent_deg_j}, \code{mu_i}, \code{mu_j},
#'   \code{sigma_i}, \code{sigma_j}, \code{p_ij}, \code{U0_ij}.
#'   \item Star-cap diagnostics: \code{star_cap_rejects},
#'   \code{star_cap_reject_items}.
#' }
#'
#' @param state Adaptive state.
#'
#' @return A tibble with one row per attempted step, in execution order.
#'
#' @examples
#' state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
#' adaptive_step_log(state)
#'
#' @seealso [adaptive_get_logs()], [adaptive_round_log()], [adaptive_rank_run_live()]
#'
#' @family adaptive logs
#' @export
adaptive_step_log <- function(state) {
  if (is.null(state$step_log)) {
    rlang::abort("`state$step_log` is missing.")
  }
  tibble::as_tibble(state$step_log)
}

#' Adaptive round log accessor.
#'
#' @details
#' \code{round_log} is the canonical per-refit audit log for the adaptive
#' pairing workflow.
#' Each row summarizes one Bayesian BTL refit and includes
#' diagnostics, reliability, and stopping-gate fields used to justify stop
#' decisions.
#'
#' Core columns:
#' \itemize{
#'   \item Refit identity/state: \code{refit_id}, \code{round_id_at_refit},
#'   \code{step_id_at_refit}, \code{timestamp}, \code{model_variant},
#'   \code{n_items}, \code{total_pairs_done}, \code{new_pairs_since_last_refit},
#'   \code{n_unique_pairs_seen}.
#'   \item Candidate health: \code{proposed_pairs_mode},
#'   \code{starve_rate_since_last_refit}, \code{fallback_rate_since_last_refit},
#'   \code{fallback_used_mode}, \code{starvation_reason_mode}.
#'   \item Identifiability/quota adaptation: \code{global_identified},
#'   \code{global_identified_reliability_min},
#'   \code{global_identified_rank_corr_min}, \code{long_quota_raw},
#'   \code{long_quota_effective}, \code{long_quota_removed},
#'   \code{realloc_to_mid}, \code{realloc_to_local}.
#'   \item Coverage/imbalance: \code{mean_degree}, \code{min_degree},
#'   \code{pos_balance_sd}, \code{star_cap_rejects_since_last_refit},
#'   \code{star_cap_reject_rate_since_last_refit},
#'   \code{recent_deg_median_since_last_refit},
#'   \code{recent_deg_max_since_last_refit}.
#'   \item Posterior parameter summaries:
#'   \code{epsilon_mean}/percentiles and \code{b_mean}/percentiles.
#'   \item Audit diagnostics: \code{ts_sigma_mean}, \code{ts_sigma_max},
#'   \code{ts_degree_sigma_corr}, \code{ts_btl_theta_corr},
#'   \code{ts_btl_rank_spearman}, \code{ci95_theta_width_*},
#'   \code{near_tie_adj_frac}, \code{near_tie_adj_count}, \code{p_adj_median},
#'   \code{cov_trace_theta}, \code{cov_logdet_diag_theta},
#'   \code{post_sd_theta_p10}, \code{post_sd_theta_p50},
#'   \code{post_sd_theta_p90}, \code{top20_boundary_entropy_*},
#'   \code{nn_diff_sd_*}.
#'   \item Stopping diagnostics: \code{diagnostics_pass},
#'   \code{diagnostics_divergences_pass}, \code{diagnostics_rhat_pass},
#'   \code{diagnostics_ess_pass}, \code{divergences},
#'   \code{divergences_max_allowed}, \code{max_rhat},
#'   \code{max_rhat_allowed}, \code{min_ess_bulk},
#'   \code{ess_bulk_required}, \code{near_stop_active},
#'   \code{reliability_EAP}, \code{eap_reliability_min}, \code{eap_pass},
#'   \code{theta_sd_eap}, \code{rho_theta}, \code{lag_eligible},
#'   \code{theta_corr_min}, \code{theta_corr_pass}, \code{delta_sd_theta},
#'   \code{theta_sd_rel_change_max}, \code{delta_sd_theta_pass},
#'   \code{rho_rank}, \code{rank_spearman_min}, \code{rho_rank_pass}.
#'   \item Refit execution metadata: \code{mcmc_chains},
#'   \code{mcmc_parallel_chains}, \code{mcmc_core_fraction},
#'   \code{mcmc_cores_detected_physical}, \code{mcmc_cores_detected_logical},
#'   \code{mcmc_threads_per_chain}, \code{mcmc_cmdstanr_version}.
#'   \item Stop output: \code{stop_decision}, \code{stop_reason}.
#' }
#'
#' @param state Adaptive state.
#'
#' @return A tibble with one row per completed posterior refit round.
#'
#' @examples
#' state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
#' adaptive_round_log(state)
#'
#' @seealso [adaptive_get_logs()], [summarize_refits()], [adaptive_rank_run_live()]
#'
#' @family adaptive logs
#' @export
adaptive_round_log <- function(state) {
  if (is.null(state$round_log)) {
    rlang::abort("`state$round_log` is missing.")
  }
  tibble::as_tibble(state$round_log)
}

#' Adaptive item log accessor.
#'
#' @details
#' \code{item_log} stores per-item posterior summaries by refit.
#' The underlying state stores a list of refit tables; this
#' accessor can return one refit table (default: most recent) or stack all
#' refits into a single tibble.
#'
#' In linking mode, raw and global summaries are kept separate:
#' \itemize{
#'   \item \code{theta_raw_eap}: within-set scale summary (from Phase A artifacts
#'   when available).
#'   \item \code{theta_global_eap} and \code{theta_global_sd}: global-scale
#'   summaries after spoke transform application.
#' }
#'
#' @param state Adaptive state.
#' @param refit_id Optional refit index.
#' @param stack When TRUE, stack all refits.
#'
#' @return A tibble of item-level summaries. When \code{stack = FALSE}, one row
#'   per item for the selected refit. When \code{stack = TRUE}, one row per item
#'   per refit with \code{refit_id} identifying source refit.
#'
#' @examples
#' state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
#' adaptive_item_log(state)
#'
#' @seealso [adaptive_get_logs()], [summarize_items()], [adaptive_round_log()]
#'
#' @family adaptive logs
#' @export
adaptive_item_log <- function(state, refit_id = NULL, stack = FALSE) {
  if (is.null(state$item_log)) {
    rlang::abort("`state$item_log` is missing.")
  }
  if (!is.list(state$item_log)) {
    rlang::abort("`state$item_log` must be a list of data frames.")
  }

  item_logs <- state$item_log
  if (length(item_logs) == 0L) {
    return(.adaptive_empty_item_log_tbl())
  }

  if (isTRUE(stack)) {
    stacked <- lapply(seq_along(item_logs), function(idx) {
      .adaptive_canonicalize_item_log(item_logs[[idx]], state, refit_id = idx)
    })
    return(dplyr::bind_rows(stacked))
  }

  idx <- refit_id %||% length(item_logs)
  idx <- as.integer(idx)
  if (length(idx) != 1L || is.na(idx) || idx < 1L || idx > length(item_logs)) {
    rlang::abort("`refit_id` is not available in `state$item_log`.")
  }

  .adaptive_canonicalize_item_log(item_logs[[idx]], state, refit_id = idx)
}

#' Adaptive results history in build_bt_data() format.
#'
#' @details
#' Converts adaptive step outcomes into the three-column format used by
#' [build_bt_data()] (\code{object1}, \code{object2}, \code{result}). With
#' \code{committed_only = TRUE}, only committed steps (\code{pair_id} not
#' missing) are retained. This preserves the transactional invariant that
#' invalid steps do not contribute to inferred comparisons.
#'
#' @param state Adaptive state.
#' @param committed_only Use only committed comparisons.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{object1}{Character item id shown in position A.}
#'   \item{object2}{Character item id shown in position B.}
#'   \item{result}{Numeric outcome in \code{\{0, 1\}} where \code{1} means
#'     \code{object1} wins.}
#' }
#'
#' @examples
#' state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
#' adaptive_results_history(state)
#'
#' @seealso [build_bt_data()], [adaptive_step_log()]
#'
#' @family adaptive logs
#' @export
adaptive_results_history <- function(state, committed_only = TRUE) {
  step_log <- adaptive_step_log(state)
  ids <- as.character(state$item_ids)

  if (isTRUE(committed_only)) {
    step_log <- step_log[!is.na(step_log$pair_id), , drop = FALSE]
  } else {
    ok <- !is.na(step_log$A) & !is.na(step_log$B) & !is.na(step_log$Y)
    step_log <- step_log[ok, , drop = FALSE]
  }
  if (nrow(step_log) == 0L) {
    return(tibble::tibble(object1 = character(), object2 = character(), result = double()))
  }

  tibble::tibble(
    object1 = as.character(ids[step_log$A]),
    object2 = as.character(ids[step_log$B]),
    result = as.numeric(step_log$Y)
  )
}

#' Summarize an adaptive state.
#'
#' @details
#' Returns a compact run-level summary from canonical logs: attempted steps,
#' committed comparisons, refit count, and last stop decision/reason. This is a
#' pure view and does not recompute model quantities.
#'
#' @param state Adaptive state.
#'
#' @return A one-row tibble with columns \code{n_items},
#'   \code{steps_attempted}, \code{committed_pairs}, \code{n_refits},
#'   \code{last_stop_decision}, and \code{last_stop_reason}.
#'
#' @examples
#' state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
#' summarize_adaptive(state)
#'
#' @seealso [adaptive_get_logs()], [base::print()]
#'
#' @family adaptive ranking
#' @export
summarize_adaptive <- function(state) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }
  step_log <- adaptive_step_log(state)
  round_log <- adaptive_round_log(state)
  committed <- sum(!is.na(step_log$pair_id))

  last_stop_decision <- NA
  last_stop_reason <- NA_character_
  if (nrow(round_log) > 0L) {
    last_stop_decision <- round_log$stop_decision[[nrow(round_log)]]
    last_stop_reason <- round_log$stop_reason[[nrow(round_log)]]
  } else if (is.list(state$meta)) {
    last_stop_decision <- as.logical(state$meta$stop_decision %||% NA)
    last_stop_reason <- as.character(state$meta$stop_reason %||% NA_character_)
  }

  tibble::tibble(
    n_items = as.integer(state$n_items),
    steps_attempted = as.integer(nrow(step_log)),
    committed_pairs = as.integer(committed),
    n_refits = as.integer(nrow(round_log)),
    last_stop_decision = as.logical(last_stop_decision),
    last_stop_reason = as.character(last_stop_reason)
  )
}

#' Print an adaptive state summary.
#'
#' @description
#' S3 method for printing \code{adaptive_state} objects.
#'
#' @param x An \code{adaptive_state} object.
#' @param ... Unused.
#'
#' @return \code{x}, invisibly.
#'
#' @examples
#' state <- adaptive_rank_start(c("a", "b", "c"), seed = 1)
#' print(state)
#'
#' @seealso [summarize_adaptive()]
#'
#' @export
print.adaptive_state <- function(x, ...) {
  summary <- summarize_adaptive(x)
  header <- "Adaptive state"
  lines <- c(
    header,
    paste0("items: ", summary$n_items),
    paste0("steps: ", summary$steps_attempted, " (committed=", summary$committed_pairs, ")"),
    paste0("refits: ", summary$n_refits)
  )

  if (!is.na(summary$last_stop_decision)) {
    decision <- if (isTRUE(summary$last_stop_decision)) "stop" else "continue"
    reason <- summary$last_stop_reason %||% NA_character_
    suffix <- if (!is.na(reason) && reason != "") paste0(" (", reason, ")") else ""
    lines <- c(lines, paste0("last stop: ", decision, suffix))
  }

  cat(paste(lines, collapse = "\n"))
  invisible(x)
}

.adaptive_progress_config <- function(progress, progress_redraw_every, progress_show_events, progress_errors) {
  progress <- match.arg(progress, c("all", "refits", "steps", "none"))
  redraw <- as.integer(progress_redraw_every)
  if (is.na(redraw) || redraw < 1L) {
    rlang::abort("`progress_redraw_every` must be a positive integer.")
  }
  show_events <- isTRUE(progress_show_events) && progress %in% c("all", "steps")
  show_errors <- isTRUE(progress_errors) && progress %in% c("all", "steps")
  list(
    progress = progress,
    progress_redraw_every = redraw,
    progress_show_events = show_events,
    progress_errors = show_errors,
    refit_pairs_target = NA_integer_,
    stop_thresholds = list()
  )
}

.adaptive_progress_metrics <- function(state, refit_pairs_target) {
  step_log <- adaptive_step_log(state)
  last_step <- state$refit_meta$last_refit_step %||% 0L
  last_M_done <- state$refit_meta$last_refit_M_done %||% 0L

  subset <- step_log[step_log$step_id > last_step, , drop = FALSE]
  committed <- sum(!is.na(step_log$pair_id))
  new_pairs <- committed - as.integer(last_M_done)
  starved <- sum(subset$candidate_starved %in% TRUE, na.rm = TRUE)
  invalid <- sum(subset$status == "invalid", na.rm = TRUE)
  fallback_rate <- if (nrow(subset) > 0L) {
    mean(!subset$fallback_used %in% c("base", "warm_start"), na.rm = TRUE)
  } else {
    NA_real_
  }

  list(
    steps_attempted = as.integer(nrow(step_log)),
    committed_pairs_done = as.integer(committed),
    new_pairs_since_last_refit = as.integer(new_pairs),
    refit_pairs_target = as.integer(refit_pairs_target),
    n_starved_since_last_refit = as.integer(starved),
    n_invalid_since_last_refit = as.integer(invalid),
    fallback_rate_since_last_refit = as.double(fallback_rate)
  )
}

.adaptive_meets_threshold <- function(value, threshold, direction = c("ge", "le")) {
  direction <- match.arg(direction)
  if (!is.finite(value) || !is.finite(threshold)) {
    return(FALSE)
  }
  if (identical(direction, "ge")) {
    return(value >= threshold)
  }
  value <= threshold
}

adaptive_progress_init <- function(state, cfg) {
  if (cfg$progress %in% c("none", "refits")) {
    return(NULL)
  }
  total <- as.integer(cfg$refit_pairs_target %||% NA_integer_)
  if (!is.finite(total) || total < 1L) {
    total <- as.integer(adaptive_defaults(state$n_items)$refit_pairs_target)
  }
  id <- cli::cli_progress_bar(
    total = total,
    format = "Adaptive {cli::pb_bar} {current}/{total} pairs {extra}",
    .envir = rlang::caller_env()
  )
  list(id = id, last_redraw = 0L, total = total)
}

adaptive_progress_update <- function(handle, state, cfg) {
  if (is.null(handle)) {
    return(handle)
  }
  if (!cfg$progress %in% c("all", "steps")) {
    return(handle)
  }
  step_id <- nrow(state$step_log)
  if ((step_id - handle$last_redraw) < cfg$progress_redraw_every) {
    return(handle)
  }
  metrics <- .adaptive_progress_metrics(state, cfg$refit_pairs_target)
  label <- paste0(
    "to next refit (steps=",
    metrics$steps_attempted,
    " committed=",
    metrics$committed_pairs_done,
    " starved=",
    metrics$n_starved_since_last_refit,
    " invalid=",
    metrics$n_invalid_since_last_refit,
    if (is.finite(metrics$fallback_rate_since_last_refit)) {
      paste0(" fallback_rate=", sprintf("%.2f", metrics$fallback_rate_since_last_refit))
    } else {
      ""
    },
    ")"
  )

  tryCatch(
    cli::cli_progress_update(
      handle$id,
      set = metrics$new_pairs_since_last_refit,
      extra = label
    ),
    error = function(e) NULL
  )
  if (isTRUE(cfg$progress_show_events)) {
    cli::cli_inform(paste0(
      "step ",
      step_id,
      ": new_pairs_since_last_refit=",
      metrics$new_pairs_since_last_refit,
      "/",
      handle$total,
      " committed=",
      metrics$committed_pairs_done,
      " invalid=",
      metrics$n_invalid_since_last_refit,
      " starved=",
      metrics$n_starved_since_last_refit
    ))
  }
  handle$last_redraw <- step_id
  handle
}

adaptive_progress_finish <- function(handle) {
  if (is.null(handle)) {
    return(invisible(NULL))
  }
  tryCatch(
    cli::cli_progress_done(handle$id),
    error = function(e) NULL
  )
  invisible(NULL)
}

adaptive_progress_step_event <- function(step_row, cfg) {
  if (!isTRUE(cfg$progress_show_events) || nrow(step_row) == 0L) {
    return(NULL)
  }
  step_id <- step_row$step_id[[1L]]
  stage <- as.character(step_row$round_stage[[1L]] %||% NA_character_)
  stage_txt <- if (!is.na(stage) && stage != "") paste0(" stage=", stage) else ""
  if (isTRUE(step_row$candidate_starved[[1L]])) {
    return(paste0("step ", step_id, ":", stage_txt, " candidate_starved=TRUE; pair_id=NA"))
  }
  if (identical(step_row$status[[1L]], "invalid") && isTRUE(cfg$progress_errors)) {
    reason <- step_row$starvation_reason[[1L]]
    if (is.na(reason) || reason == "") {
      reason <- "invalid"
    }
    return(paste0("step ", step_id, ":", stage_txt, " invalid judge (", reason, "); pair_id=NA"))
  }
  if (!is.na(step_row$fallback_used[[1L]]) &&
    !step_row$fallback_used[[1L]] %in% c("base", "warm_start")) {
    return(paste0("step ", step_id, ":", stage_txt, " fallback_used=", step_row$fallback_used[[1L]]))
  }
  NULL
}

adaptive_progress_refit_block <- function(round_row, cfg) {
  if (nrow(round_row) == 0L) {
    return(character())
  }
  row <- round_row[1L, , drop = FALSE]
  thresholds <- cfg$stop_thresholds %||% list()

  header <- paste0(
    "REFIT #",
    sprintf("%04d", as.integer(row$refit_id)),
    "  round_id_at_refit=",
    row$round_id_at_refit,
    "  step_id_at_refit=",
    row$step_id_at_refit,
    "  model_variant=",
    row$model_variant,
    "  n_items=",
    row$n_items
  )

  pairs <- paste0(
    "Pairs: total_pairs_done=",
    row$total_pairs_done,
    "  new_pairs_since_last_refit=",
    row$new_pairs_since_last_refit,
    "  n_unique_pairs_seen=",
    row$n_unique_pairs_seen
  )

  selection <- c(
    "Selection health (since last refit):",
    paste0("  proposed_pairs_mode=", row$proposed_pairs_mode),
    paste0("  starve_rate_since_last_refit=", row$starve_rate_since_last_refit),
    paste0("  fallback_rate_since_last_refit=", row$fallback_rate_since_last_refit),
    paste0("  fallback_used_mode=", row$fallback_used_mode),
    paste0("  starvation_reason_mode=", row$starvation_reason_mode)
  )

  coverage <- c(
    "Coverage / balance:",
    paste0("  mean_degree=", row$mean_degree, "  min_degree=", row$min_degree),
    paste0("  pos_balance_sd=", row$pos_balance_sd)
  )

  model_params <- character()
  if (!is.na(row$epsilon_mean)) {
    model_params <- c(
      model_params,
      paste0(
        "  epsilon_mean=",
        row$epsilon_mean,
        " [p2.5=",
        row$epsilon_p2.5,
        " p50=",
        row$epsilon_p50,
        " p97.5=",
        row$epsilon_p97.5,
        "]"
      )
    )
  }
  if (!is.na(row$b_mean)) {
    model_params <- c(
      model_params,
      paste0(
        "  b_mean=",
        row$b_mean,
        " [p2.5=",
        row$b_p2.5,
        " p50=",
        row$b_p50,
        " p97.5=",
        row$b_p97.5,
        "]"
      )
    )
  }
  if (length(model_params) > 0L) {
    model_params <- c("Model params (posterior; compact):", model_params)
  }

  diagnostics <- c(
    "Diagnostics gate:",
    paste0("  diagnostics_pass=", row$diagnostics_pass),
    paste0(
      "  divergences=",
      row$divergences,
      " (need <= ",
      row$divergences_max_allowed,
      "; pass=",
      row$diagnostics_divergences_pass,
      ")"
    ),
    paste0(
      "  max_rhat=",
      row$max_rhat,
      " (need <= ",
      row$max_rhat_allowed,
      "; pass=",
      row$diagnostics_rhat_pass,
      ")"
    ),
    paste0("  min_ess_bulk=", row$min_ess_bulk, " (need >= ", row$ess_bulk_required, ")")
  )

  stop_table <- c("Stop criteria (rule: ALL applicable must pass):")
  stop_table <- c(
    stop_table,
    paste0(
      "  ",
      if (isTRUE(row$diagnostics_pass)) "[x] " else "[ ] ",
      "diagnostics_pass"
    )
  )

  eap_min <- row$eap_reliability_min %||% thresholds$eap_reliability_min %||% NA_real_
  eap_pass <- .adaptive_meets_threshold(row$reliability_EAP, eap_min, "ge")
  stop_table <- c(
    stop_table,
    paste0(
      "  ",
      if (isTRUE(eap_pass)) "[x] " else "[ ] ",
      "reliability_EAP >= eap_reliability_min  value=",
      row$reliability_EAP,
      " (need >= ",
      eap_min,
      ")"
    )
  )

  rho_rank_min <- row$rank_spearman_min %||% thresholds$rank_spearman_min %||% NA_real_
  rho_rank_pass <- .adaptive_meets_threshold(row$rho_rank, rho_rank_min, "ge")
  stop_table <- c(
    stop_table,
    paste0(
      "  ",
      if (isTRUE(rho_rank_pass)) "[x] " else "[ ] ",
      "rho_rank >= rank_spearman_min  value=",
      row$rho_rank,
      " (need >= ",
      rho_rank_min,
      ")"
    )
  )

  theta_min <- row$theta_corr_min %||% thresholds$theta_corr_min %||% NA_real_
  theta_pass <- .adaptive_meets_threshold(row$rho_theta, theta_min, "ge")
  stop_table <- c(
    stop_table,
    paste0(
      "  ",
      if (isTRUE(theta_pass)) "[x] " else "[ ] ",
      "rho_theta >= theta_corr_min  value=",
      row$rho_theta,
      " (need >= ",
      theta_min,
      ")"
    )
  )

  sd_max <- row$theta_sd_rel_change_max %||% thresholds$theta_sd_rel_change_max %||% NA_real_
  sd_pass <- .adaptive_meets_threshold(row$delta_sd_theta, sd_max, "le")
  stop_table <- c(
    stop_table,
    paste0(
      "  ",
      if (isTRUE(sd_pass)) "[x] " else "[ ] ",
      "delta_sd_theta <= theta_sd_rel_change_max  value=",
      row$delta_sd_theta,
      " (need <= ",
      sd_max,
      ")"
    )
  )
  stop_table <- c(
    stop_table,
    paste0(
      "  ",
      if (isTRUE(row$lag_eligible)) "[x] " else "[ ] ",
      "lag_eligible"
    )
  )

  report_only <- c(
    "Report-only metrics (not used for stopping):",
    paste0("  ci95_theta_width_mean=", row$ci95_theta_width_mean),
    paste0("  near_tie_adj_frac=", row$near_tie_adj_frac),
    paste0("  cov_trace_theta=", row$cov_trace_theta),
    paste0("  top20_boundary_entropy_mean=", row$top20_boundary_entropy_mean),
    paste0("  nn_diff_sd_mean=", row$nn_diff_sd_mean)
  )

  mcmc <- c(
    "Refit MCMC settings:",
    paste0("  chains=", row$mcmc_chains),
    paste0("  parallel_chains=", row$mcmc_parallel_chains),
    paste0("  core_fraction=", row$mcmc_core_fraction),
    paste0("  threads_per_chain=", row$mcmc_threads_per_chain)
  )

  decision <- if (isTRUE(row$stop_decision)) "Decision: STOP" else "Decision: continue"
  if (!is.na(row$stop_reason) && row$stop_reason != "") {
    decision <- paste0(decision, "  stop_reason=\"", row$stop_reason, "\"")
  }

  stop_summary <- paste0(
    "stop_decision=",
    row$stop_decision,
    "  stop_reason=",
    row$stop_reason
  )

  c(
    strrep("-", 64),
    header,
    pairs,
    selection,
    coverage,
    model_params,
    mcmc,
    diagnostics,
    report_only,
    stop_table,
    stop_summary,
    decision,
    strrep("-", 64)
  )
}
