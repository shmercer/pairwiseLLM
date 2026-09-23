# -------------------------------------------------------------------------
# Adaptive Bayesian BTL refits (inference-only) + stopping rules.
# -------------------------------------------------------------------------

.adaptive_btl_defaults <- function(N) {
  N <- as.integer(N)
  if (is.na(N) || N < 2L) {
    rlang::abort("`N` must be a positive integer >= 2.")
  }

  ess_bulk_min <- max(400L, as.integer(round(20 * sqrt(N))))
  ess_bulk_min_near_stop <- max(1000L, as.integer(round(50 * sqrt(N))))

  list(
    model_variant = "btl_e_b",
    ess_bulk_min = as.double(ess_bulk_min),
    ess_bulk_min_near_stop = as.double(ess_bulk_min_near_stop),
    max_rhat = 1.01,
    divergences_max = 0L,
    eap_reliability_min = 0.90,
    stability_lag = 2L,
    theta_corr_min = 0.95,
    theta_sd_rel_change_max = 0.10,
    rank_spearman_min = 0.95,
    near_tie_p_low = 0.40,
    near_tie_p_high = 0.60,
    deferred_audit_max_draws = 400L,
    phase_b_refit_parallel = FALSE,
    phase_b_refit_workers = 1L
  )
}

.adaptive_safe_cor <- function(x, y, method = "pearson") {
  x <- as.double(x)
  y <- as.double(y)
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]
  y <- y[keep]
  if (length(x) < 2L || length(y) < 2L) {
    return(NA_real_)
  }
  if (!is.finite(stats::sd(x)) || !is.finite(stats::sd(y)) ||
    stats::sd(x) <= 0 || stats::sd(y) <= 0) {
    return(NA_real_)
  }
  as.double(stats::cor(x, y, method = method, use = "pairwise.complete.obs"))
}

.adaptive_btl_resolve_config <- function(state, config) {
  defaults <- .adaptive_btl_defaults(state$n_items)
  if (is.null(config)) {
    return(defaults)
  }
  if (!is.list(config)) {
    rlang::abort("`config` must be a list when provided.")
  }
  resolved <- utils::modifyList(defaults, config)
  if (!is.logical(resolved$phase_b_refit_parallel) ||
    length(resolved$phase_b_refit_parallel) != 1L ||
    is.na(resolved$phase_b_refit_parallel)) {
    rlang::abort("`config$phase_b_refit_parallel` must be TRUE or FALSE.")
  }
  if (!.btl_mcmc_intish(resolved$phase_b_refit_workers) ||
    as.integer(resolved$phase_b_refit_workers) < 1L) {
    rlang::abort("`config$phase_b_refit_workers` must be a positive integer.")
  }
  if (!is.numeric(resolved$deferred_audit_max_draws) ||
    length(resolved$deferred_audit_max_draws) != 1L ||
    is.na(resolved$deferred_audit_max_draws) ||
    (!is.infinite(resolved$deferred_audit_max_draws) &&
      (!.btl_mcmc_intish(resolved$deferred_audit_max_draws) ||
        as.integer(resolved$deferred_audit_max_draws) < 2L))) {
    rlang::abort("`config$deferred_audit_max_draws` must be >= 2 or Inf.")
  }
  resolved$phase_b_refit_workers <- as.integer(resolved$phase_b_refit_workers)
  if (!is.infinite(resolved$deferred_audit_max_draws)) {
    resolved$deferred_audit_max_draws <- as.integer(resolved$deferred_audit_max_draws)
  }
  resolved
}

.adaptive_phase_b_refit_pairs_target_floor <- function(state, controller, phase_ctx) {
  run_mode <- as.character(controller$run_mode %||% "within_set")
  concurrent_mode <- identical(run_mode, "link_multi_spoke") &&
    identical(as.character(controller$multi_spoke_mode %||% "independent"), "concurrent")
  if (!isTRUE(concurrent_mode) ||
    !identical(as.character(phase_ctx$phase %||% "phase_a"), "phase_b")) {
    return(0L)
  }

  active_spokes <- as.integer(phase_ctx$active_spokes %||% integer())
  active_spokes <- sort(unique(active_spokes[!is.na(active_spokes)]))
  if (length(active_spokes) < 2L) {
    return(0L)
  }

  probe_cap <- max(0L, as.integer(controller$probe_pairs_per_refit_per_spoke %||% 4L))
  sizes <- .adaptive_link_spoke_size_summary(
    set_ids = state$items$set_id,
    hub_id = controller$hub_id %||% 1L
  )
  scaled_active <- if (identical(as.character(controller$link_refit_pairs_per_spoke_rule %||% "scaled"), "scaled")) {
    .adaptive_scaled_count(
      sizes$max_spoke_n,
      min_value = controller$link_refit_pairs_per_spoke_min %||% 40L,
      frac = controller$link_refit_pairs_per_spoke_frac %||% 0.035,
      lower = 1L
    )
  } else {
    0L
  }
  active_floor_min <- if (isTRUE(controller$probe_active_floor_enabled)) {
    max(0L, as.integer(controller$probe_active_floor_min %||% 20L))
  } else {
    0L
  }
  active_floor_frac <- if (isTRUE(controller$probe_active_floor_enabled)) {
    as.double(controller$probe_active_floor_frac %||% 0.5)
  } else {
    0
  }
  if (!is.finite(active_floor_frac) || active_floor_frac < 0 || active_floor_frac > 1) {
    rlang::abort("`adaptive_config$probe_active_floor_frac` must be in [0, 1].")
  }

  per_spoke_budget <- as.integer(max(active_floor_min, scaled_active) + probe_cap)
  for (unused in seq_len(10L)) {
    active_floor <- max(
      active_floor_min,
      as.integer(ceiling(active_floor_frac * per_spoke_budget))
    )
    needed <- as.integer(active_floor + probe_cap)
    if (needed <= per_spoke_budget) {
      break
    }
    per_spoke_budget <- needed
  }

  as.integer(length(active_spokes) * per_spoke_budget)
}

.adaptive_refit_pairs_target <- function(state, config) {
  effective_n <- as.integer(state$n_items)
  controller <- .adaptive_controller_resolve(state)
  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  is_link_phase_a <- as.character(controller$run_mode %||% "within_set") %in% c("link_one_spoke", "link_multi_spoke") &&
    !identical(as.character(phase_ctx$phase %||% "phase_a"), "phase_b")
  if (isTRUE(is_link_phase_a)) {
    active_set <- as.integer(phase_ctx$active_phase_a_set %||% NA_integer_)
    if (!is.na(active_set)) {
      scoped_n <- as.integer(sum(as.integer(state$items$set_id) == active_set, na.rm = TRUE))
      if (is.finite(scoped_n) && scoped_n >= 2L) {
        effective_n <- scoped_n
      }
    }
  }
  item_scaled_target <- .btl_mcmc_clamp(
    20L,
    5000L,
    as.integer(ceiling(effective_n / 2))
  )
  if (.adaptive_link_mode_active(controller) &&
    identical(as.character(phase_ctx$phase %||% "phase_a"), "phase_b")) {
    phase_b_floor <- .adaptive_phase_b_refit_pairs_target_floor(
      state = state,
      controller = controller,
      phase_ctx = phase_ctx
    )
    configured_target <- config$refit_pairs_target
    has_configured_target <- !is.null(configured_target) &&
      length(configured_target) == 1L &&
      !is.na(configured_target)
    refit_pairs_target <- if (isTRUE(has_configured_target)) {
      max(as.integer(configured_target), phase_b_floor)
    } else {
      phase_b_floor
    }
  } else {
    refit_pairs_target <- config$refit_pairs_target %||% item_scaled_target
  }
  as.integer(refit_pairs_target)
}

.adaptive_refit_phase_a_scope <- function(state) {
  controller <- .adaptive_controller_resolve(state)
  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  run_mode <- as.character(controller$run_mode %||% "within_set")
  if (!run_mode %in% c("link_one_spoke", "link_multi_spoke")) {
    return(list(active = FALSE, set_id = NA_integer_))
  }
  if (identical(as.character(phase_ctx$phase %||% "phase_a"), "phase_b")) {
    return(list(active = FALSE, set_id = NA_integer_))
  }
  set_id <- as.integer(phase_ctx$active_phase_a_set %||% NA_integer_)
  if (is.na(set_id)) {
    return(list(active = FALSE, set_id = NA_integer_))
  }
  list(active = TRUE, set_id = as.integer(set_id))
}

.adaptive_refit_scope_counts <- function(state) {
  phase_scope <- .adaptive_refit_phase_a_scope(state)
  if (!isTRUE(phase_scope$active)) {
    return(list(
      M_done = as.integer(nrow(state$history_pairs)),
      last_refit_M_done = as.integer(state$refit_meta$last_refit_M_done %||% 0L),
      last_refit_step = as.integer(state$refit_meta$last_refit_step %||% 0L),
      scope_set_id = NA_integer_
    ))
  }

  set_id <- as.integer(phase_scope$set_id)
  M_done <- .adaptive_phase_a_within_set_pair_count(state, set_id = set_id)

  key <- as.character(set_id)
  last_refit_M_done_map <- state$refit_meta$last_refit_M_done_by_phase_a_set %||% list()
  last_refit_step_map <- state$refit_meta$last_refit_step_by_phase_a_set %||% list()
  list(
    M_done = as.integer(M_done),
    last_refit_M_done = as.integer(last_refit_M_done_map[[key]] %||% 0L),
    last_refit_step = as.integer(last_refit_step_map[[key]] %||% 0L),
    scope_set_id = as.integer(set_id)
  )
}

.adaptive_committed_results_empty <- function() {
  tibble::tibble(
    pair_id = integer(),
    step_id = integer(),
    A_id = character(),
    B_id = character(),
    Y = integer(),
    timestamp = as.POSIXct(character()),
    is_cross_set = logical()
  )
}

.adaptive_committed_results_rebuild <- function(state) {
  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  required <- c("pair_id", "step_id", "A", "B", "Y", "timestamp")
  if (nrow(step_log) < 1L || !all(required %in% names(step_log))) {
    return(.adaptive_committed_results_empty())
  }

  rows <- step_log[!is.na(step_log$pair_id), , drop = FALSE]
  if (nrow(rows) < 1L) {
    return(.adaptive_committed_results_empty())
  }

  ids <- as.character(state$item_ids %||% character())
  out <- tibble::tibble(
    pair_id = as.integer(rows$pair_id),
    step_id = as.integer(rows$step_id),
    A_id = as.character(ids[as.integer(rows$A)]),
    B_id = as.character(ids[as.integer(rows$B)]),
    Y = as.integer(rows$Y),
    timestamp = rows$timestamp,
    is_cross_set = if ("is_cross_set" %in% names(rows)) {
      as.logical(rows$is_cross_set %in% TRUE)
    } else {
      rep(FALSE, nrow(rows))
    }
  )
  out[order(out$step_id, out$pair_id), , drop = FALSE]
}

.adaptive_committed_results_cache <- function(state) {
  refit_meta <- state$refit_meta %||% list()
  cache <- refit_meta$committed_results_cache %||% NULL
  if (is.null(cache)) {
    return(NULL)
  }
  cache <- tibble::as_tibble(cache)
  required <- names(.adaptive_committed_results_empty())
  if (!all(required %in% names(cache))) {
    return(NULL)
  }
  cache[, required, drop = FALSE]
}

.adaptive_committed_results_resolve <- function(state) {
  refit_meta <- state$refit_meta %||% list()
  cache_built <- isTRUE(refit_meta$committed_results_cache_built %||% FALSE)
  cache <- .adaptive_committed_results_cache(state)
  if (isTRUE(cache_built) && !is.null(cache)) {
    return(cache)
  }
  .adaptive_committed_results_rebuild(state)
}

.adaptive_committed_results_update <- function(cache, step_row, A_id, B_id, Y) {
  step_row <- tibble::as_tibble(step_row)
  if (nrow(step_row) != 1L) {
    return(cache %||% .adaptive_committed_results_empty())
  }

  pair_id <- as.integer(step_row$pair_id[[1L]] %||% NA_integer_)
  if (is.na(pair_id)) {
    return(cache %||% .adaptive_committed_results_empty())
  }

  cache <- tibble::as_tibble(cache %||% .adaptive_committed_results_empty())
  out <- dplyr::bind_rows(
    cache,
    tibble::tibble(
      pair_id = pair_id,
      step_id = as.integer(step_row$step_id[[1L]] %||% NA_integer_),
      A_id = as.character(A_id),
      B_id = as.character(B_id),
      Y = as.integer(Y %||% NA_integer_),
      timestamp = step_row$timestamp[[1L]] %||% as.POSIXct(NA),
      is_cross_set = as.logical(step_row$is_cross_set[[1L]] %||% FALSE)
    )
  )
  out[, names(.adaptive_committed_results_empty()), drop = FALSE]
}

.adaptive_stop_metric_scope <- function(state, ids = NULL) {
  ids <- as.character(ids %||% state$item_ids)
  phase_scope <- .adaptive_refit_phase_a_scope(state)
  if (!isTRUE(phase_scope$active)) {
    return(list(
      phase_scope = "global",
      phase_scope_set_id = NA_integer_,
      scope_ids = as.character(ids)
    ))
  }
  set_id <- as.integer(phase_scope$set_id)
  set_map <- stats::setNames(as.integer(state$items$set_id), as.character(state$items$item_id))
  scope_ids <- as.character(ids[as.integer(set_map[ids]) == set_id])
  if (length(scope_ids) < 2L) {
    rlang::warn(paste0(
      "Phase A scoped refit could not resolve at least two items for active set ",
      set_id,
      "; falling back to global scope. Check `state$items$set_id` mapping and ",
      "`state$linking$phase_a$active_phase_a_set`."
    ))
    return(list(
      phase_scope = "global",
      phase_scope_set_id = NA_integer_,
      scope_ids = as.character(ids)
    ))
  }
  list(
    phase_scope = "phase_a_set",
    phase_scope_set_id = as.integer(set_id),
    scope_ids = as.character(scope_ids)
  )
}

#' @keywords internal
#' @noRd
.adaptive_link_phase_b_active <- function(state, controller = NULL) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  .adaptive_link_mode_active(controller) &&
    identical(
      as.character((.adaptive_link_phase_context(state, controller = controller)$phase %||% "phase_a")),
      "phase_b"
    )
}

#' @keywords internal
#' @noRd
.adaptive_phase_a_artifact_item_ids <- function(state, artifact, set_id) {
  expected_ids <- as.character(
    state$items$item_id[as.integer(state$items$set_id) == as.integer(set_id)]
  )
  if (length(expected_ids) < 1L) {
    rlang::abort(paste0("No state items found for Phase A artifact set_id=", as.integer(set_id), "."))
  }

  items_tbl <- tibble::as_tibble(artifact$items %||% tibble::tibble())
  artifact_ids <- character()
  if ("item_id" %in% names(items_tbl)) {
    artifact_ids <- as.character(items_tbl$item_id)
  } else if ("global_item_id" %in% names(items_tbl)) {
    item_map <- stats::setNames(
      as.character(state$items$item_id),
      as.character(state$items$global_item_id)
    )
    artifact_ids <- as.character(item_map[as.character(items_tbl$global_item_id)])
  }

  if (length(artifact_ids) > 0L) {
    missing_ids <- setdiff(expected_ids, artifact_ids)
    extra_ids <- setdiff(artifact_ids, expected_ids)
    if (length(missing_ids) > 0L || length(extra_ids) > 0L) {
      rlang::abort(paste0(
        "Phase A artifact item domain mismatch for set_id=",
        as.integer(set_id),
        "."
      ))
    }
  }

  expected_ids
}

#' @keywords internal
#' @noRd
.adaptive_phase_a_artifact_draws_for_phase_b_global <- function(state, set_id) {
  phase_a <- state$linking$phase_a %||% list()
  artifact <- (phase_a$artifacts %||% list())[[as.character(set_id)]] %||% NULL
  if (!is.list(artifact)) {
    rlang::abort(paste0(
      "Phase B global metric reconstruction requires a Phase A artifact for set_id=",
      as.integer(set_id),
      "."
    ))
  }

  draws <- artifact$posterior_draws %||% NULL
  if (!is.matrix(draws) || !is.numeric(draws) || nrow(draws) < 2L || ncol(draws) < 1L) {
    rlang::abort(paste0(
      "Phase B global metric reconstruction requires numeric `posterior_draws` with at least two ",
      "draws for set_id=",
      as.integer(set_id),
      "."
    ))
  }

  item_ids <- .adaptive_phase_a_artifact_item_ids(state, artifact, set_id = set_id)
  if (is.null(colnames(draws))) {
    if (ncol(draws) != length(item_ids)) {
      rlang::abort(paste0(
        "Phase A artifact draw columns do not match the item count for set_id=",
        as.integer(set_id),
        "."
      ))
    }
    colnames(draws) <- item_ids
  }
  if (!all(item_ids %in% colnames(draws))) {
    rlang::abort(paste0(
      "Phase A artifact draw columns are missing required item ids for set_id=",
      as.integer(set_id),
      "."
    ))
  }

  .pairwiseLLM_sanitize_draws_matrix(
    draws[, item_ids, drop = FALSE],
    name = paste0("phase_a_artifact_posterior_draws_set_", as.integer(set_id))
  )
}

#' @keywords internal
#' @noRd
.adaptive_phase_b_global_metric_transform_stats <- function(state, spoke_id, controller = NULL) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  key <- as.character(as.integer(spoke_id))

  stats_row <- (controller$link_refit_stats_by_spoke %||% list())[[key]] %||% list()
  last_row <- tibble::tibble()
  link_stage_log <- tibble::as_tibble(state$link_stage_log %||% new_link_stage_log())
  if (nrow(link_stage_log) > 0L && all(c("spoke_id", "refit_id") %in% names(link_stage_log))) {
    link_stage_log <- link_stage_log[
      as.integer(link_stage_log$spoke_id) == as.integer(spoke_id),
      ,
      drop = FALSE
    ]
    if (nrow(link_stage_log) > 0L) {
      link_stage_log <- link_stage_log[
        order(as.integer(link_stage_log$refit_id), seq_len(nrow(link_stage_log))),
        ,
        drop = FALSE
      ]
      last_row <- link_stage_log[nrow(link_stage_log), , drop = FALSE]
    }
  }

  transform_state <- as.character(
    stats_row$link_transform_state %||%
      if (nrow(last_row) > 0L) last_row$link_transform_state[[1L]] else NA_character_ %||%
      .adaptive_link_transform_state_for_spoke(controller, spoke_id)
  )
  if (!transform_state %in% .adaptive_link_transform_state_levels()) {
    rlang::abort(paste0(
      "Phase B global metric reconstruction could not resolve a valid transform state for spoke_id=",
      as.integer(spoke_id),
      "."
    ))
  }

  delta_mean <- as.double(
    stats_row$delta_spoke_mean %||%
      if (nrow(last_row) > 0L) last_row$delta_spoke_mean[[1L]] else NA_real_ %||%
      (controller$link_transform_frozen_delta_by_spoke %||% list())[[key]] %||%
      (controller$link_transform_last_delta_by_spoke %||% list())[[key]] %||%
      NA_real_
  )
  if (!is.finite(delta_mean)) {
    rlang::abort(paste0(
      "Phase B global metric reconstruction requires a finite delta for spoke_id=",
      as.integer(spoke_id),
      "."
    ))
  }

  log_alpha_mean <- as.double(
    stats_row$log_alpha_spoke_mean %||%
      if (nrow(last_row) > 0L) last_row$log_alpha_spoke_mean[[1L]] else NA_real_ %||%
      (controller$link_transform_frozen_log_alpha_by_spoke %||% list())[[key]] %||%
      (controller$link_transform_last_log_alpha_by_spoke %||% list())[[key]] %||%
      NA_real_
  )
  if (identical(transform_state, "shift_scale") && !is.finite(log_alpha_mean)) {
    rlang::abort(paste0(
      "Phase B global metric reconstruction requires a finite log-alpha for shift-scale spoke_id=",
      as.integer(spoke_id),
      "."
    ))
  }

  list(
    link_transform_state = transform_state,
    delta_spoke_mean = as.double(delta_mean),
    log_alpha_spoke_mean = as.double(log_alpha_mean)
  )
}

#' @keywords internal
#' @noRd
.adaptive_phase_b_global_metric_uncertainty_approximation <- function(link_estimation_mode = NULL,
                                                                      link_uncertainty_approximation = NULL,
                                                                      link_fit_method = NULL) {
  approx <- as.character(link_uncertainty_approximation %||% NA_character_)
  if (length(approx) != 1L || is.na(approx) || !nzchar(approx)) return(NA_character_)
  approx
}

#' @keywords internal
#' @noRd


#' @keywords internal
#' @noRd
.adaptive_phase_b_global_metric_draws <- function(state, controller = NULL) {
  .link_guard_adaptive_selection(state, controller)
  NULL
}

#' @keywords internal
#' @noRd
.adaptive_phase_b_global_metric_history_update <- function(state, refit_id = NULL, draws = NULL) {
  controller <- .adaptive_controller_resolve(state)
  if (!isTRUE(.adaptive_link_phase_b_active(state, controller = controller))) {
    return(state)
  }

  draws <- draws %||% .adaptive_phase_b_global_metric_draws(state, controller = controller)
  theta_mean <- stats::setNames(as.double(colMeans(draws)), as.character(colnames(draws)))
  history <- state$refit_meta$phase_b_global_theta_mean_history %||% list()
  refit_id <- as.integer(refit_id %||% (nrow(state$round_log %||% tibble::tibble()) + 1L))
  if (!is.finite(refit_id) || refit_id < 1L) {
    rlang::abort("Phase B global metric history update requires a positive `refit_id`.")
  }
  if (length(history) < refit_id) {
    history <- c(history, rep_len(list(NULL), refit_id - length(history)))
  }
  history[[refit_id]] <- theta_mean
  state$refit_meta$phase_b_global_theta_mean_history <- history
  state
}

.adaptive_refit_eligibility <- function(total_committed, last_refit_committed, refit_pairs_target) {
  total_committed <- as.integer(total_committed %||% 0L)
  last_refit_committed <- as.integer(last_refit_committed %||% 0L)
  refit_pairs_target <- as.integer(refit_pairs_target %||% 0L)
  new_pairs_since_last_refit <- as.integer(total_committed - last_refit_committed)
  eligible <- new_pairs_since_last_refit >= refit_pairs_target
  list(
    eligible = isTRUE(eligible),
    new_pairs_since_last_refit = as.integer(new_pairs_since_last_refit)
  )
}

.adaptive_results_from_step_log <- function(state, scope_ids = NULL) {
  results_cache <- .adaptive_committed_results_resolve(state)
  if (nrow(results_cache) == 0L) {
    return(tibble::tibble())
  }
  A_id <- as.character(results_cache$A_id)
  B_id <- as.character(results_cache$B_id)
  if (!is.null(scope_ids)) {
    scope_ids <- as.character(scope_ids)
    in_scope <- A_id %in% scope_ids & B_id %in% scope_ids
    results_cache <- results_cache[in_scope, , drop = FALSE]
    A_id <- A_id[in_scope]
    B_id <- B_id[in_scope]
    if (nrow(results_cache) == 0L) {
      return(tibble::tibble())
    }
  }
  y_vals <- as.integer(results_cache$Y)
  if (any(is.na(y_vals) | !y_vals %in% c(0L, 1L))) {
    rlang::abort(
      "Adaptive refit invariant failed: committed step rows must encode Y in {0,1} with Y=1 meaning A wins."
    )
  }
  winner_pos <- ifelse(results_cache$Y == 1L, 1L, 2L)
  better_id <- ifelse(results_cache$Y == 1L, A_id, B_id)
  controller <- .adaptive_controller_resolve(state)
  run_mode <- as.character(controller$run_mode %||% "within_set")
  is_link_mode <- run_mode %in% c("link_one_spoke", "link_multi_spoke")
  phase_a <- state$linking$phase_a %||% list()
  phase_b_ready <- isTRUE(phase_a$ready_for_phase_b %||% FALSE)
  phase_b_start_step <- as.integer(phase_a$phase_b_started_at_step %||% NA_integer_)
  is_cross <- as.logical(results_cache$is_cross_set %in% TRUE)
  phase_is_b <- rep(FALSE, nrow(results_cache))
  if (isTRUE(is_link_mode) && is.finite(phase_b_start_step)) {
    # Prefer explicit phase metadata when available.
    phase_is_b <- as.integer(results_cache$step_id) >= phase_b_start_step
  } else if (isTRUE(is_link_mode) && any(is_cross)) {
    # Guarded legacy fallback for resumed sessions without explicit boundary metadata.
    phase_is_b <- cumsum(is_cross) > 0L
  } else if (isTRUE(is_link_mode) && isTRUE(phase_b_ready)) {
    phase_is_b <- rep(TRUE, nrow(results_cache))
  }
  phase <- rep("phase2", nrow(results_cache))
  if (isTRUE(is_link_mode)) {
    phase <- ifelse(phase_is_b, "phase3", "phase2")
  }
  judge_mode <- as.character(controller$judge_param_mode %||% "global_shared")
  judge_scope <- rep("shared", nrow(results_cache))
  if (identical(judge_mode, "phase_specific")) {
    judge_scope <- ifelse(phase_is_b, "link", "within")
  }

  tibble::tibble(
    pair_uid = paste0("pair_", results_cache$pair_id),
    unordered_key = make_unordered_key(A_id, B_id),
    ordered_key = make_ordered_key(A_id, B_id),
    A_id = as.character(A_id),
    B_id = as.character(B_id),
    better_id = as.character(better_id),
    winner_pos = as.integer(winner_pos),
    phase = as.character(phase),
    judge_scope = as.character(judge_scope),
    iter = as.integer(results_cache$step_id),
    received_at = results_cache$timestamp,
    backend = rep("adaptive", nrow(results_cache)),
    model = rep("adaptive", nrow(results_cache))
  )
}

.adaptive_btl_adapt_fit <- function(fit) {
  if (!is.list(fit)) {
    rlang::abort("`fit` must be a list.")
  }
  adaptive_fit <- fit
  if (!"btl_posterior_draws" %in% names(adaptive_fit)) {
    if (!"theta_draws" %in% names(adaptive_fit)) {
      rlang::abort("`fit` must include `btl_posterior_draws` or `theta_draws`.")
    }
    adaptive_fit$btl_posterior_draws <- adaptive_fit$theta_draws
  }
  if ("theta_draws" %in% names(adaptive_fit)) {
    adaptive_fit$theta_draws <- NULL
  }
  adaptive_fit
}

.adaptive_btl_extract_fit_contract <- function(fit_out) {
  fit_contract <- NULL
  if (is.list(fit_out) && "fit" %in% names(fit_out)) {
    fit_contract <- fit_out[["fit"]]
  }
  if (!is.null(fit_contract)) {
    return(fit_contract)
  }
  fits <- NULL
  if (is.list(fit_out) && "fits" %in% names(fit_out)) {
    fits <- fit_out[["fits"]]
  }
  if (is.list(fits) && length(fits) >= 1L) {
    return(fits[[length(fits)]])
  }
  fit_out
}

.adaptive_btl_fit_theta_mean <- function(fit) {
  theta_mean <- fit$theta_mean %||% NULL
  if (!is.null(theta_mean)) {
    theta_mean <- as.double(theta_mean)
    if (!is.null(names(fit$theta_mean))) {
      names(theta_mean) <- names(fit$theta_mean)
    }
  } else {
    draws <- fit$btl_posterior_draws %||% NULL
    if (!is.matrix(draws) || !is.numeric(draws)) {
      rlang::abort("`fit$btl_posterior_draws` must be a numeric matrix.")
    }
    theta_mean <- colMeans(draws)
  }
  theta_mean
}

.adaptive_ts_btl_rank_spearman <- function(state, theta_mean) {
  if (is.null(state$trueskill_state) ||
    is.null(state$trueskill_state$items) ||
    !is.data.frame(state$trueskill_state$items)) {
    return(NA_real_)
  }
  ids <- as.character(state$item_ids)
  theta_names <- names(theta_mean)
  theta_mean <- as.double(theta_mean)
  names(theta_mean) <- theta_names
  if (is.null(theta_names) || !all(ids %in% theta_names)) {
    return(NA_real_)
  }
  theta_vals <- theta_mean[ids]
  ts_ids <- as.character(state$trueskill_state$items$item_id)
  ts_mu <- as.double(state$trueskill_state$items$mu[match(ids, ts_ids)])
  if (any(!is.finite(theta_vals)) || any(!is.finite(ts_mu))) {
    return(NA_real_)
  }
  rank_theta <- rank(theta_vals, ties.method = "average")
  rank_mu <- rank(ts_mu, ties.method = "average")
  if (stats::sd(rank_theta) == 0 || stats::sd(rank_mu) == 0) {
    return(NA_real_)
  }
  as.double(stats::cor(rank_mu, rank_theta, method = "spearman", use = "pairwise.complete.obs"))
}

.adaptive_update_identifiability_state <- function(state, config) {
  out <- state
  controller <- .adaptive_controller_resolve(out)
  if (!is.null(config$global_identified_reliability_min)) {
    controller$global_identified_reliability_min <- as.double(config$global_identified_reliability_min)
  }
  if (!is.null(config$global_identified_rank_corr_min)) {
    controller$global_identified_rank_corr_min <- as.double(config$global_identified_rank_corr_min)
  }

  draws <- out$btl_fit$btl_posterior_draws %||% NULL
  theta_mean <- .adaptive_btl_fit_theta_mean(out$btl_fit %||% list())
  reliability <- compute_reliability_EAP(draws)
  rho_rank <- .adaptive_ts_btl_rank_spearman(out, theta_mean)

  controller$reliability_EAP <- as.double(reliability)
  controller$ts_btl_rank_spearman <- as.double(rho_rank)
  controller$global_identified <- is.finite(reliability) &&
    is.finite(rho_rank) &&
    reliability >= as.double(controller$global_identified_reliability_min) &&
    rho_rank >= as.double(controller$global_identified_rank_corr_min)

  out$controller <- controller
  out
}

.adaptive_mode_value <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0L) {
    return(NA_character_)
  }
  tab <- table(x)
  names(tab)[[which.max(tab)]]
}

.adaptive_link_spoke_ids <- function(state, hub_id) {
  setdiff(unique(as.integer(state$items$set_id)), as.integer(hub_id))
}

.adaptive_link_transform_state_for_spoke <- function(controller, spoke_id) {
  if ((controller$link_estimation_mode %||% "") %in% .adaptive_link_estimation_mode_levels()) return(NA_character_)

  policy <- .adaptive_normalize_link_transform_policy(
    controller$link_transform_policy %||% "auto"
  )
  if (!identical(policy, "auto")) {
    return(.adaptive_default_link_transform_state(policy))
  }
  state_map <- controller$link_transform_state_by_spoke %||% list()
  state <- as.character(state_map[[as.character(spoke_id)]] %||% "shift_only")
  if (!state %in% .adaptive_link_transform_state_levels()) {
    return("shift_only")
  }
  state
}

.adaptive_link_active_item_ids <- function(state, spoke_id, hub_id) {
  spoke_items <- as.character(state$items$item_id[as.integer(state$items$set_id) == as.integer(spoke_id)])
  cumulative <- .adaptive_link_cross_edges(state, spoke_id = as.integer(spoke_id), last_refit_step = NULL)
  hub_active_cross <- unique(as.character(cumulative$hub_item[!(cumulative$is_probe_step %in% TRUE)]))
  hub_active_cross <- hub_active_cross[!is.na(hub_active_cross)]
  active_hub <- unique(hub_active_cross)
  active_all <- unique(c(spoke_items, active_hub))

  list(
    active_all = as.character(active_all),
    active_hub = as.character(active_hub),
    active_spoke = as.character(spoke_items)
  )
}

.adaptive_link_epoch_start_step_default <- function(state, spoke_id) {
  phase_a <- state$linking$phase_a %||% list()
  phase_b_start <- as.integer(phase_a$phase_b_started_at_step %||% NA_integer_)
  if (is.finite(phase_b_start) && !is.na(phase_b_start) && phase_b_start >= 1L) {
    return(as.integer(phase_b_start))
  }

  rows <- .adaptive_link_cross_edges(state, spoke_id = as.integer(spoke_id), last_refit_step = NULL)
  rows <- rows[!(rows$is_probe_step %in% TRUE), , drop = FALSE]
  if (nrow(rows) > 0L) {
    return(as.integer(min(as.integer(rows$step_id), na.rm = TRUE)))
  }

  1L
}

.adaptive_link_epoch_start_step_for_spoke <- function(state, spoke_id) {
  controller <- .adaptive_controller_resolve(state)
  start_map <- controller$link_epoch_start_step_by_spoke %||% list()
  start_step <- as.integer(start_map[[as.character(spoke_id)]] %||% NA_integer_)
  if (!is.finite(start_step) || is.na(start_step) || start_step < 1L) {
    start_step <- .adaptive_link_epoch_start_step_default(state, spoke_id)
  }
  as.integer(start_step)
}

.adaptive_link_transform_theta_mean_for_spoke <- function(state,
                                                          theta_mean,
                                                          spoke_id,
                                                          hub_id,
                                                          transform_mode,
                                                          delta_mean,
                                                          log_alpha_mean = NA_real_) {
  if (!is.numeric(theta_mean) || length(theta_mean) < 1L || is.null(names(theta_mean))) {
    return(stats::setNames(numeric(), character()))
  }
  theta <- as.double(theta_mean)
  names(theta) <- as.character(names(theta_mean))

  mode <- as.character(transform_mode %||% "shift_only")
  if (!mode %in% c("shift_only", "shift_scale")) {
    mode <- "shift_only"
  }
  delta <- as.double(delta_mean %||% NA_real_)
  if (!is.finite(delta)) {
    return(stats::setNames(rep(NA_real_, length(theta)), names(theta)))
  }
  alpha <- 1
  if (identical(mode, "shift_scale")) {
    log_alpha <- as.double(log_alpha_mean %||% NA_real_)
    if (!is.finite(log_alpha)) {
      return(stats::setNames(rep(NA_real_, length(theta)), names(theta)))
    }
    alpha <- exp(log_alpha)
  }

  item_ids <- as.character(state$items$item_id)
  set_ids <- as.integer(state$items$set_id)
  set_by_item <- stats::setNames(set_ids, item_ids)
  spoke_items <- names(set_by_item)[set_by_item == as.integer(spoke_id)]
  hub_items <- names(set_by_item)[set_by_item == as.integer(hub_id)]
  keep <- names(theta) %in% c(spoke_items, hub_items)
  theta <- theta[keep]
  is_spoke <- names(theta) %in% spoke_items
  theta[is_spoke] <- as.double(delta + alpha * theta[is_spoke])
  theta
}

.adaptive_link_reliability_decomposition <- function(mu_vals,
                                                     var_vals,
                                                     var_mu_epsilon,
                                                     total_var_epsilon) {
  mu_vals <- as.double(mu_vals)
  var_vals <- as.double(var_vals)
  keep <- is.finite(mu_vals) & is.finite(var_vals) & var_vals >= 0
  mu_vals <- mu_vals[keep]
  var_vals <- var_vals[keep]
  if (length(mu_vals) < 2L) {
    return(list(
      reliability = NA_real_,
      V_mu = NA_real_,
      V_post = NA_real_,
      defined = FALSE
    ))
  }
  V_mu <- as.double(stats::var(mu_vals))
  V_post <- as.double(mean(var_vals))
  defined <- is.finite(V_mu) &&
    is.finite(V_post) &&
    V_mu >= as.double(var_mu_epsilon) &&
    (V_mu + V_post) >= as.double(total_var_epsilon)
  reliability <- if (isTRUE(defined)) {
    as.double(max(0, min(1, V_mu / (V_mu + V_post))))
  } else {
    NA_real_
  }
  list(
    reliability = reliability,
    V_mu = as.double(V_mu),
    V_post = as.double(V_post),
    defined = isTRUE(defined)
  )
}

.adaptive_link_reliability_active <- function(state, active_ids) {
  fit <- state$btl_fit %||% list()
  draws <- fit$btl_posterior_draws %||% NULL
  if (!is.matrix(draws) || !is.numeric(draws) || is.null(colnames(draws))) {
    return(NA_real_)
  }
  ids <- intersect(as.character(active_ids), as.character(colnames(draws)))
  if (length(ids) < 2L) {
    return(NA_real_)
  }
  as.double(compute_reliability_EAP(draws[, ids, drop = FALSE]))
}

.adaptive_link_global_score_stats_active <- function(state,
                                                     active_ids,
                                                     spoke_id,
                                                     hub_id,
                                                     transform_mode,
                                                     delta_mean,
                                                     log_alpha_mean = NA_real_,
                                                     fit = NULL,
                                                     refit_mode = "shift_only",
                                                     hub_lock_mode = "soft_lock",
                                                     shift_only_theta_treatment = "fixed_eap_plugin_var",
                                                     link_estimation_mode = "transform",
                                                     accepted_state = NULL,
                                                     var_mu_epsilon = 1e-6,
                                                     total_var_epsilon = 1e-6) {
  active_ids <- as.character(active_ids)
  if (length(active_ids) < 2L) {
    return(list(
      reliability = NA_real_,
      V_mu = NA_real_,
      V_post = NA_real_,
      mean_map = stats::setNames(numeric(), character()),
      var_map = stats::setNames(numeric(), character()),
      defined = FALSE
    ))
  }

  fit <- fit %||% list()
  fit_post <- fit$posterior_draws %||% list()

  mode <- as.character(transform_mode %||% "shift_only")
  if (!mode %in% c("shift_only", "shift_scale")) {
    mode <- "shift_only"
  }
  delta <- as.double(delta_mean %||% NA_real_)
  if (!is.finite(delta)) {
    return(list(
      reliability = NA_real_,
      V_mu = NA_real_,
      V_post = NA_real_,
      mean_map = stats::setNames(rep(NA_real_, length(active_ids)), active_ids),
      var_map = stats::setNames(rep(NA_real_, length(active_ids)), active_ids),
      defined = FALSE
    ))
  }
  alpha <- 1
  if (identical(mode, "shift_scale")) {
    log_alpha <- as.double(log_alpha_mean %||% NA_real_)
    if (!is.finite(log_alpha)) {
      return(list(
        reliability = NA_real_,
        V_mu = NA_real_,
        V_post = NA_real_,
        mean_map = stats::setNames(rep(NA_real_, length(active_ids)), active_ids),
        var_map = stats::setNames(rep(NA_real_, length(active_ids)), active_ids),
        defined = FALSE
      ))
    }
    alpha <- exp(log_alpha)
  }

  hub_mu <- .adaptive_link_phase_a_theta_map(state, hub_id, "theta_raw_mean")
  hub_sd <- .adaptive_link_phase_a_theta_map(state, hub_id, "theta_raw_sd")
  spoke_mu <- .adaptive_link_phase_a_theta_map(state, spoke_id, "theta_raw_mean")
  spoke_sd <- .adaptive_link_phase_a_theta_map(state, spoke_id, "theta_raw_sd")
  if (identical(as.character(refit_mode), "joint_refit")) {
    hub_mu <- fit$theta_hub_post %||% hub_mu
    if (is.matrix(fit_post$theta_hub) && !is.null(colnames(fit_post$theta_hub))) {
      hub_sd_draw <- .pairwiseLLM_col_sds(fit_post$theta_hub)
      names(hub_sd_draw) <- colnames(fit_post$theta_hub)
      hub_sd <- as.double(hub_sd_draw)
      names(hub_sd) <- names(hub_sd_draw)
    }
    spoke_mu <- fit$theta_spoke_post %||% spoke_mu
    if (is.matrix(fit_post$theta_spoke) && !is.null(colnames(fit_post$theta_spoke))) {
      spoke_sd_draw <- .pairwiseLLM_col_sds(fit_post$theta_spoke)
      names(spoke_sd_draw) <- colnames(fit_post$theta_spoke)
      spoke_sd <- as.double(spoke_sd_draw)
      names(spoke_sd) <- names(spoke_sd_draw)
    }
  }

  hub_mu <- as.double(hub_mu)
  names(hub_mu) <- names(.adaptive_link_phase_a_theta_map(state, hub_id, "theta_raw_mean"))
  hub_sd <- pmax(0, as.double(hub_sd))
  names(hub_sd) <- names(.adaptive_link_phase_a_theta_map(state, hub_id, "theta_raw_sd"))
  spoke_mu <- as.double(spoke_mu)
  names(spoke_mu) <- names(.adaptive_link_phase_a_theta_map(state, spoke_id, "theta_raw_mean"))
  spoke_sd <- pmax(0, as.double(spoke_sd))
  names(spoke_sd) <- names(.adaptive_link_phase_a_theta_map(state, spoke_id, "theta_raw_sd"))

  delta_draws <- as.double(fit_post$delta %||% numeric())
  if (length(delta_draws) < 1L) {
    delta_draws <- rep(delta, 1L)
  }
  log_alpha_draws <- as.double(fit_post$log_alpha %||% numeric())
  if (identical(mode, "shift_scale")) {
    if (length(log_alpha_draws) != length(delta_draws)) {
      log_alpha_draws <- rep(log_alpha_mean, length(delta_draws))
    }
    alpha_draws <- exp(log_alpha_draws)
  } else {
    alpha_draws <- rep(1, length(delta_draws))
  }

  mean_map <- stats::setNames(rep(NA_real_, length(active_ids)), active_ids)
  var_map <- stats::setNames(rep(NA_real_, length(active_ids)), active_ids)
  hub_draws <- fit_post$theta_hub %||% NULL
  spoke_draws <- fit_post$theta_spoke %||% NULL

  for (item_id in active_ids) {
    if (item_id %in% names(hub_mu)) {
      mu_i <- as.double(hub_mu[[item_id]])
      v_i <- as.double((hub_sd[[item_id]] %||% 0)^2)
      if (identical(as.character(refit_mode), "joint_refit") &&

        is.matrix(hub_draws) &&
        item_id %in% colnames(hub_draws)) {
        draws_i <- as.double(hub_draws[, item_id, drop = TRUE])
        if (sum(is.finite(draws_i)) >= 2L) {
          mu_i <- as.double(mean(draws_i))
          v_i <- as.double(stats::var(draws_i))
        }
      }
      mean_map[[item_id]] <- mu_i
      var_map[[item_id]] <- max(0, v_i)
      next
    }
    if (!item_id %in% names(spoke_mu)) {
      next
    }
    base_mu <- as.double(spoke_mu[[item_id]])
    plugin_var <- if (identical(as.character(shift_only_theta_treatment), "fixed_eap_plugin_var")) {
      as.double((spoke_sd[[item_id]] %||% 0)^2)
    } else {
      0
    }
    if (is.matrix(spoke_draws) && item_id %in% colnames(spoke_draws)) {
      theta_draws <- as.double(spoke_draws[, item_id, drop = TRUE])
    } else {
      theta_draws <- rep(base_mu, length(delta_draws))
    }
    if (length(theta_draws) != length(delta_draws)) {
      theta_draws <- rep(base_mu, length(delta_draws))
    }
    score_draws <- delta_draws + alpha_draws * theta_draws
    mu_i <- as.double(mean(score_draws))
    v_i <- as.double(stats::var(score_draws))
    if (!is.finite(v_i)) {
      v_i <- 0
    }
    if (!is.matrix(spoke_draws) && plugin_var > 0) {
      v_i <- v_i + alpha^2 * plugin_var
    }
    mean_map[[item_id]] <- mu_i
    var_map[[item_id]] <- max(0, v_i)
  }

  decomp <- .adaptive_link_reliability_decomposition(
    mu_vals = unname(mean_map),
    var_vals = unname(var_map),
    var_mu_epsilon = var_mu_epsilon,
    total_var_epsilon = total_var_epsilon
  )
  c(
    decomp,
    list(
      mean_map = mean_map,
      var_map = var_map
    )
  )
}

.adaptive_link_reliability_transformed_active <- function(state,
                                                          active_ids,
                                                          spoke_id,
                                                          hub_id,
                                                          transform_mode,
                                                          delta_mean,
                                                          log_alpha_mean = NA_real_,
                                                          fit = NULL,
                                                          refit_mode = "shift_only",
                                                          hub_lock_mode = "soft_lock",
                                                          shift_only_theta_treatment = "fixed_eap_plugin_var",
                                                          link_estimation_mode = "transform",
                                                          accepted_state = NULL,
                                                          var_mu_epsilon = 1e-6,
                                                          total_var_epsilon = 1e-6) {
  out <- .adaptive_link_global_score_stats_active(
    state = state,
    active_ids = active_ids,
    spoke_id = spoke_id,
    hub_id = hub_id,
    transform_mode = transform_mode,
    delta_mean = delta_mean,
    log_alpha_mean = log_alpha_mean,
    fit = fit,
    refit_mode = refit_mode,
    hub_lock_mode = hub_lock_mode,
    shift_only_theta_treatment = shift_only_theta_treatment,
    link_estimation_mode = link_estimation_mode,
    accepted_state = accepted_state,
    var_mu_epsilon = var_mu_epsilon,
    total_var_epsilon = total_var_epsilon
  )
  as.double(out$reliability %||% NA_real_)
}

.adaptive_link_ts_btl_rank_spearman_active <- function(state, active_ids, theta_mean = NULL) {
  if (is.null(state$trueskill_state) ||
    is.null(state$trueskill_state$items) ||
    !is.data.frame(state$trueskill_state$items)) {
    return(NA_real_)
  }
  theta_mean <- theta_mean %||% .adaptive_btl_fit_theta_mean(state$btl_fit %||% list())
  theta_names <- names(theta_mean)
  theta_mean <- as.double(theta_mean)
  names(theta_mean) <- theta_names
  if (is.null(names(theta_mean))) {
    return(NA_real_)
  }
  ids <- as.character(active_ids)
  if (length(ids) < 2L || !all(ids %in% names(theta_mean))) {
    return(NA_real_)
  }
  theta_vals <- as.double(theta_mean[ids])
  ts_ids <- as.character(state$trueskill_state$items$item_id)
  ts_mu <- as.double(state$trueskill_state$items$mu[match(ids, ts_ids)])
  if (any(!is.finite(theta_vals)) || any(!is.finite(ts_mu))) {
    return(NA_real_)
  }
  rank_theta <- rank(theta_vals, ties.method = "average")
  rank_mu <- rank(ts_mu, ties.method = "average")
  if (stats::sd(rank_theta) == 0 || stats::sd(rank_mu) == 0) {
    return(NA_real_)
  }
  as.double(stats::cor(rank_mu, rank_theta, method = "spearman", use = "pairwise.complete.obs"))
}

.adaptive_link_rank_stability_lagged <- function(state,
                                                 active_ids,
                                                 stability_lag,
                                                 spoke_id,
                                                 hub_id,
                                                 transform_mode,
                                                 delta_mean,
                                                 log_alpha_mean = NA_real_,
                                                 lag_row = tibble::tibble()) {
  history <- state$refit_meta$theta_mean_history %||% list()
  current_refit <- as.integer(length(history))
  lag <- as.integer(stability_lag)
  lag_eligible <- !is.na(lag) && lag >= 1L && current_refit > lag
  if (!isTRUE(lag_eligible)) {
    return(list(
      lag_eligible = FALSE,
      rho_rank_lagged = NA_real_,
      rho_rank_lagged_pass = NA
    ))
  }

  current_raw <- history[[current_refit]]
  if (!is.numeric(current_raw) || is.null(names(current_raw))) {
    current_raw <- .adaptive_btl_fit_theta_mean(state$btl_fit %||% list())
  }
  lagged_raw <- history[[current_refit - lag]]
  if (!is.numeric(current_raw) ||
    !is.numeric(lagged_raw) ||
    is.null(names(current_raw)) ||
    is.null(names(lagged_raw))) {
    return(list(
      lag_eligible = TRUE,
      rho_rank_lagged = NA_real_,
      rho_rank_lagged_pass = FALSE
    ))
  }

  lag_row <- tibble::as_tibble(lag_row)
  lag_mode <- if (nrow(lag_row) > 0L) {
    if ("link_transform_state" %in% names(lag_row)) {
      as.character(lag_row$link_transform_state[[1L]] %||% "shift_only")
    } else {
      "shift_only"
    }
  } else {
    as.character(transform_mode %||% "shift_only")
  }
  lag_delta <- if (nrow(lag_row) > 0L) {
    as.double(lag_row$delta_spoke_mean[[1L]] %||% NA_real_)
  } else {
    NA_real_
  }
  lag_log_alpha <- if (nrow(lag_row) > 0L) {
    as.double(lag_row$log_alpha_spoke_mean[[1L]] %||% NA_real_)
  } else {
    NA_real_
  }

  current <- .adaptive_link_transform_theta_mean_for_spoke(
    state = state,
    theta_mean = current_raw,
    spoke_id = spoke_id,
    hub_id = hub_id,
    transform_mode = transform_mode,
    delta_mean = delta_mean,
    log_alpha_mean = log_alpha_mean
  )
  lagged <- .adaptive_link_transform_theta_mean_for_spoke(
    state = state,
    theta_mean = lagged_raw,
    spoke_id = spoke_id,
    hub_id = hub_id,
    transform_mode = lag_mode,
    delta_mean = lag_delta,
    log_alpha_mean = lag_log_alpha
  )

  ids <- as.character(active_ids)
  if (length(ids) < 2L || !all(ids %in% names(current)) || !all(ids %in% names(lagged))) {
    return(list(
      lag_eligible = TRUE,
      rho_rank_lagged = NA_real_,
      rho_rank_lagged_pass = FALSE
    ))
  }

  theta_t <- as.double(current[ids])
  theta_l <- as.double(lagged[ids])
  if (any(!is.finite(theta_t)) || any(!is.finite(theta_l))) {
    return(list(
      lag_eligible = TRUE,
      rho_rank_lagged = NA_real_,
      rho_rank_lagged_pass = FALSE
    ))
  }

  rank_t <- rank(theta_t, ties.method = "average")
  rank_l <- rank(theta_l, ties.method = "average")
  rho <- as.double(stats::cor(rank_t, rank_l, method = "spearman", use = "pairwise.complete.obs"))
  pass <- is.finite(rho) && rho >= 0.98
  list(
    lag_eligible = TRUE,
    rho_rank_lagged = rho,
    rho_rank_lagged_pass = pass
  )
}

.adaptive_link_delta_sd_max_derived <- function(state, hub_id, delta_sd_mult) {
  theta_mean <- .adaptive_btl_fit_theta_mean(state$btl_fit %||% list())
  if (!is.numeric(theta_mean) || is.null(names(theta_mean))) {
    return(NA_real_)
  }
  hub_items <- as.character(state$items$item_id[as.integer(state$items$set_id) == as.integer(hub_id)])
  hub_theta <- as.double(theta_mean[hub_items])
  if (sum(is.finite(hub_theta)) < 2L) {
    return(NA_real_)
  }
  as.double(delta_sd_mult) * as.double(stats::sd(hub_theta, na.rm = TRUE))
}

.adaptive_link_probe_metrics_current <- function(state, refit_id, spoke_id) {
  cache <- .adaptive_link_probe_state(state)$prediction_cache
  # Old caches do not record presentation orientation or estimator identity.
  required <- c("observation_id", "y_A", "estimator_id", "estimator_version", "input_hash")
  if (!all(required %in% names(cache))) return(list(probe_brier = NA_real_, realized_n = 0L))
  current <- cache[cache$refit_id == refit_id & cache$spoke_id == spoke_id, , drop = FALSE]
  if (!nrow(current)) return(list(probe_brier = NA_real_, realized_n = 0L))
  result <- .link_orchestration_result(state, spoke_id)
  .link_check(all(current$estimator_id == result$estimator_id) &&
    all(current$estimator_version == result$estimator_version) &&
    all(current$input_hash == result$provenance$hashes$input), "Probe cache estimator/evidence mismatch.")
  .link_check(!anyDuplicated(current$observation_id), "Probe cache repeats held-out observations.")
  list(probe_brier = mean((current$y_A - current$pred_prob)^2), realized_n = nrow(current))
}

.adaptive_link_probe_pred_rmse_lagged <- function(state, refit_id, spoke_id, lag_refit_id, epoch_id) {
  cache <- .adaptive_link_probe_state(state)$prediction_cache
  required <- c("observation_id", "estimator_id", "estimator_version", "config_hash", "history_hash",
    "active_edges", "cross_evidence_hash", "input_hash",
    "hub_set_id", "spoke_set_id", "uncertainty_scope", "A_set", "A_item", "B_set", "B_item", "y_A")
  if (!all(required %in% names(cache))) return(NA_real_)
  rows <- cache[cache$spoke_id == spoke_id & cache$link_epoch_id == epoch_id, , drop = FALSE]
  current <- rows[rows$refit_id == refit_id, , drop = FALSE]
  lagged <- rows[rows$refit_id == lag_refit_id, , drop = FALSE]
  if (!nrow(current) || !nrow(lagged)) return(NA_real_)
  for (k in c("estimator_id", "estimator_version", "config_hash", "history_hash",
    "hub_set_id", "spoke_set_id", "uncertainty_scope")) {
    .link_check(length(unique(c(current[[k]], lagged[[k]]))) == 1L &&
      !anyNA(c(current[[k]], lagged[[k]])), "Lagged probe cache identity mismatch; reset the spoke history.")
  }
  result <- .link_orchestration_result(state, spoke_id)
  input <- result$continuation$input
  .link_check(all(current$input_hash == input$hashes$input) &&
    all(current$history_hash == .link_orchestration_history_hash(result)),
    "Current probe cache does not match the accepted estimator result.")
  n_old <- unique(lagged$active_edges)
  .link_check(length(n_old) == 1L && is.finite(n_old) && n_old >= 0L &&
    n_old == floor(n_old) && n_old <= nrow(input$cross), "Invalid lagged active evidence count.")
  .link_check(all(lagged$cross_evidence_hash == .link_hash(input$cross[seq_len(n_old), , drop = FALSE])),
    "Lagged probe cache changed the active evidence prefix; reset the spoke history.")
  .link_check(!anyDuplicated(current$observation_id) && !anyDuplicated(lagged$observation_id),
    "Probe cache repeats held-out observations.")
  at <- match(current$observation_id, lagged$observation_id)
  keep <- !is.na(at)
  if (!any(keep)) return(NA_real_)
  for (k in c("A_set", "A_item", "B_set", "B_item", "y_A")) {
    .link_check(identical(current[[k]][keep], lagged[[k]][at[keep]]),
      "Lagged probe observations changed orientation or outcome.")
  }
  sqrt(mean((current$pred_prob[keep] - lagged$pred_prob[at[keep]])^2))
}

.adaptive_link_theta_global_scope_ids <- function(state, spoke_id, scope) {
  scope <- as.character(scope %||% "direct_evidence_spoke")
  spoke_items <- as.character(state$items$item_id[as.integer(state$items$set_id) == as.integer(spoke_id)])
  cross_all <- .adaptive_link_cross_edges(state, spoke_id = spoke_id, last_refit_step = NULL)
  active_spoke <- if (nrow(cross_all) > 0L) {
    as.character(unique(cross_all$spoke_item[!(cross_all$is_probe_step %in% TRUE)]))
  } else {
    character()
  }
  if (identical(scope, "all_spoke_items")) {
    return(spoke_items)
  }
  if (identical(scope, "min_cross_set_edges_k")) {
    controller <- .adaptive_controller_resolve(state)
    k <- as.integer(controller$min_cross_set_edges_k %||% 1L)
    if (nrow(cross_all) < 1L) {
      return(character())
    }
    active_non_probe <- cross_all[!(cross_all$is_probe_step %in% TRUE), , drop = FALSE]
    counts <- table(as.character(active_non_probe$spoke_item))
    ids <- names(counts)[as.integer(counts) >= k]
    return(as.character(ids))
  }
  as.character(active_spoke)
}

.adaptive_link_theta_global_rmse_lagged <- function(state,
                                                    spoke_id,
                                                    hub_id,
                                                    scope_ids,
                                                    transform_mode,
                                                    delta_mean,
                                                    log_alpha_mean,
                                                    lag_row,
                                                    lag = 1L) {
  ids <- as.character(scope_ids)
  if (length(ids) < 2L) {
    return(NA_real_)
  }
  history <- state$refit_meta$theta_mean_history %||% list()
  current_refit <- length(history)
  lag <- as.integer(lag %||% 1L)
  if (current_refit < 1L || is.na(lag) || lag < 1L || current_refit <= lag) {
    return(NA_real_)
  }
  current_raw <- history[[current_refit]]
  lag_raw <- history[[current_refit - lag]]
  if (!is.numeric(current_raw) || !is.numeric(lag_raw) || is.null(names(current_raw)) || is.null(names(lag_raw))) {
    return(NA_real_)
  }
  lag_mode <- as.character(lag_row$link_transform_state[[1L]] %||% "shift_only")
  lag_delta <- as.double(lag_row$delta_spoke_mean[[1L]] %||% NA_real_)
  lag_log_alpha <- as.double(lag_row$log_alpha_spoke_mean[[1L]] %||% NA_real_)
  current_theta <- .adaptive_link_transform_theta_mean_for_spoke(
    state = state,
    theta_mean = current_raw,
    spoke_id = spoke_id,
    hub_id = hub_id,
    transform_mode = transform_mode,
    delta_mean = delta_mean,
    log_alpha_mean = log_alpha_mean
  )
  lag_theta <- .adaptive_link_transform_theta_mean_for_spoke(
    state = state,
    theta_mean = lag_raw,
    spoke_id = spoke_id,
    hub_id = hub_id,
    transform_mode = lag_mode,
    delta_mean = lag_delta,
    log_alpha_mean = lag_log_alpha
  )
  if (!all(ids %in% names(current_theta)) || !all(ids %in% names(lag_theta))) {
    return(NA_real_)
  }
  diff <- as.double(current_theta[ids] - lag_theta[ids])
  diff <- diff[is.finite(diff)]
  if (length(diff) < 2L) {
    return(NA_real_)
  }
  sqrt(mean(diff^2))
}

#' @keywords internal
#' @noRd
.adaptive_phase_b_global_theta_history_at_refit <- function(state, refit_id) {
  refit_id <- as.integer(refit_id %||% NA_integer_)
  history <- state$refit_meta$phase_b_global_theta_mean_history %||% list()
  if (!is.finite(refit_id) || is.na(refit_id) || refit_id < 1L || length(history) < refit_id) {
    return(NULL)
  }
  theta <- history[[refit_id]]
  if (!is.numeric(theta) || is.null(names(theta))) {
    return(NULL)
  }
  theta <- as.double(theta)
  names(theta) <- as.character(names(history[[refit_id]]))
  theta
}

#' @keywords internal
#' @noRd
.adaptive_link_theta_global_rmse_from_maps <- function(current_theta, lag_theta, scope_ids) {
  ids <- as.character(scope_ids)
  if (length(ids) < 2L) {
    return(NA_real_)
  }
  if (!is.numeric(current_theta) || !is.numeric(lag_theta) ||
    is.null(names(current_theta)) || is.null(names(lag_theta))) {
    return(NA_real_)
  }
  current_names <- as.character(names(current_theta))
  current_theta <- as.double(current_theta)
  names(current_theta) <- current_names
  lag_names <- as.character(names(lag_theta))
  lag_theta <- as.double(lag_theta)
  names(lag_theta) <- lag_names
  if (!all(ids %in% names(current_theta)) || !all(ids %in% names(lag_theta))) {
    return(NA_real_)
  }
  diff <- as.double(current_theta[ids] - lag_theta[ids])
  diff <- diff[is.finite(diff)]
  if (length(diff) < 2L) {
    return(NA_real_)
  }
  sqrt(mean(diff^2))
}

#' @keywords internal
#' @noRd

.adaptive_link_epoch_signature_components <- function(transform_state,
                                                     refit_mode,
                                                     lock_mode,
                                                     hub_art,
                                                     spoke_art,
                                                     link_estimation_mode = "transform") {
  c(
    link_estimation_mode = as.character(link_estimation_mode),
    link_transform_state = as.character(transform_state),
    link_refit_mode = as.character(refit_mode),
    hub_lock_mode = as.character(lock_mode),
    hub_artifact_refit_id = as.character(as.integer(hub_art$refit_id %||% NA_integer_)),
    spoke_artifact_refit_id = as.character(as.integer(spoke_art$refit_id %||% NA_integer_)),
    hub_artifact_config_hash = as.character(hub_art$fit_config_hash %||% NA_character_),
    spoke_artifact_config_hash = as.character(spoke_art$fit_config_hash %||% NA_character_)
  )
}

.adaptive_link_epoch_signature_string <- function(components) {
  paste(as.character(components), collapse = "|")
}

.adaptive_link_epoch_reset_reason <- function(previous_signature, current_components) {
  if (!is.character(previous_signature) || length(previous_signature) != 1L ||
    is.na(previous_signature) || !nzchar(previous_signature)) {
    return(NA_character_)
  }
  previous_parts <- strsplit(previous_signature, "|", fixed = TRUE)[[1L]]
  previous_parts[previous_parts %in% c("NA", "")] <- NA_character_
  expected_names <- names(current_components)
  if (length(previous_parts) < length(expected_names)) {
    return("legacy_epoch_signature_schema")
  }
  names(previous_parts) <- c(
    expected_names,
    if (length(previous_parts) > length(expected_names)) {
      paste0("legacy_extra_", seq_len(length(previous_parts) - length(expected_names)))
    } else {
      character()
    }
  )
  previous_components <- previous_parts[expected_names]
  current_values <- as.character(current_components)
  same_component <- mapply(
    function(previous_value, current_value) {
      if (is.na(previous_value) && is.na(current_value)) {
        return(TRUE)
      }
      identical(as.character(previous_value), as.character(current_value))
    },
    previous_value = previous_components,
    current_value = current_values,
    USE.NAMES = FALSE
  )
  changed <- expected_names[!same_component]
  if (length(changed) < 1L) {
    return(NA_character_)
  }
  reason_map <- c(
    link_estimation_mode = "link_estimation_mode_change",
    link_transform_state = "transform_state_change",
    link_refit_mode = "link_refit_mode_change",
    hub_lock_mode = "hub_lock_mode_change",
    hub_artifact_refit_id = "hub_artifact_replaced",
    spoke_artifact_refit_id = "spoke_artifact_replaced",
    hub_artifact_config_hash = "hub_artifact_reloaded",
    spoke_artifact_config_hash = "spoke_artifact_reloaded"
  )
  as.character(reason_map[[changed[[1L]]]] %||% "epoch_signature_change")
}

.adaptive_link_stop_blockers <- function(link_diagnostics_pass,
                                         link_lag_eligible,
                                         link_min_refit_eligible,
                                         probe_edges_realized,
                                         probe_edges_min_for_stop,
                                         link_stop_reliability_min,
                                         reliability_active,
                                         probe_brier,
                                         probe_brier_max,
                                         probe_pred_rmse_lagged,
                                         probe_pred_rmse_max,
                                         theta_global_rmse_lagged,
                                         theta_global_rmse_max,
                                         probe_quality_pass,
                                         hub_anchored) {
  blocker_names <- c(
    "diagnostics_failed",
    "lag_not_eligible",
    "min_refits_not_met",
    "probe_edges_min_for_stop",
    "reliability_link_global",
    "probe_brier",
    "probe_pred_rmse_lagged",
    "theta_global_rmse_lagged",
    "hub_not_anchored"
  )
  blockers <- c(
    diagnostics_failed = !isTRUE(link_diagnostics_pass),
    lag_not_eligible = !isTRUE(link_lag_eligible),
    min_refits_not_met = !isTRUE(link_min_refit_eligible),
    probe_edges_min_for_stop = as.integer(probe_edges_realized %||% 0L) <
      as.integer(probe_edges_min_for_stop %||% 0L),
    reliability_link_global = !is.finite(as.double(reliability_active %||% NA_real_)) ||
      as.double(reliability_active) < as.double(link_stop_reliability_min %||% 0.90),
    probe_brier = !is.finite(as.double(probe_brier %||% NA_real_)) ||
      as.double(probe_brier) > as.double(probe_brier_max %||% 0.19),
    probe_pred_rmse_lagged = !is.finite(as.double(probe_pred_rmse_lagged %||% NA_real_)) ||
      as.double(probe_pred_rmse_lagged) > as.double(probe_pred_rmse_max %||% 0.015),
    theta_global_rmse_lagged = !is.finite(as.double(theta_global_rmse_lagged %||% NA_real_)) ||
      as.double(theta_global_rmse_lagged) > as.double(theta_global_rmse_max %||% 0.05),
    hub_not_anchored = !isTRUE(hub_anchored)
  )
  blockers <- stats::setNames(as.logical(unname(blockers)), blocker_names)
  list(
    blockers = blockers,
    codes = if (any(blockers)) {
      paste(names(blockers)[blockers], collapse = ",")
    } else {
      "none"
    }
  )
}

.adaptive_link_reconstruct_stop_from_logs <- function(link_row, diagnostics_pass, hub_theta_sd, controller) {
  row <- tibble::as_tibble(link_row)
  if (nrow(row) != 1L) {
    rlang::abort("`link_row` must have exactly one row.")
  }
  controller <- utils::modifyList(.adaptive_controller_defaults(2L), controller %||% list())
  eligible <- isTRUE(row$link_stop_eligible[[1L]])
  if (!eligible) {
    return(FALSE)
  }
  if (!isTRUE(diagnostics_pass)) {
    return(FALSE)
  }
  row_col <- function(name) row[[name]] %||% NULL
  reliability_min <- as.double(
    row_col("link_stop_reliability_min_used")[[1L]] %||%
      controller$link_stop_reliability_min %||%
      0.90
  )
  probe_brier_max <- as.double(
    row_col("probe_brier_max_used")[[1L]] %||%
      controller$probe_brier_max %||%
      0.19
  )
  probe_pred_rmse_max <- as.double(
    row_col("probe_pred_rmse_max_used")[[1L]] %||%
      controller$probe_pred_rmse_max %||%
      0.015
  )
  theta_global_rmse_max <- as.double(
    row_col("theta_global_rmse_max_used")[[1L]] %||%
      controller$theta_global_rmse_max %||%
      0.05
  )
  rel_gate <- if ("reliability_stop_pass" %in% names(row)) {
    isTRUE(row$reliability_stop_pass[[1L]] %||% FALSE)
  } else if ("reliability_link_global" %in% names(row)) {
    is.finite(row$reliability_link_global[[1L]]) &&
      row$reliability_link_global[[1L]] >= reliability_min
  } else if ("reliability_EAP_link" %in% names(row)) {
    is.finite(row$reliability_EAP_link[[1L]]) &&
      row$reliability_EAP_link[[1L]] >= reliability_min
  } else {
    FALSE
  }
  hub_gate <- if ("hub_anchored" %in% names(row)) {
    isTRUE(row$hub_anchored[[1L]] %||% FALSE)
  } else {
    FALSE
  }
  probe_gate <- if ("probe_brier" %in% names(row)) {
    is.finite(as.double(row$probe_brier[[1L]] %||% NA_real_)) &&
      as.double(row$probe_brier[[1L]]) <= probe_brier_max
  } else {
    FALSE
  }
  probe_quality_gate <- if ("probe_quality_pass" %in% names(row)) {
    value <- row$probe_quality_pass[[1L]]
    if (is.na(value)) TRUE else isTRUE(value)
  } else {
    TRUE
  }
  probe_rmse_gate <- if ("probe_pred_rmse_lagged" %in% names(row)) {
    is.finite(as.double(row$probe_pred_rmse_lagged[[1L]] %||% NA_real_)) &&
      as.double(row$probe_pred_rmse_lagged[[1L]]) <= probe_pred_rmse_max
  } else {
    FALSE
  }
  theta_rmse_gate <- if ("theta_global_rmse_lagged" %in% names(row)) {
    is.finite(as.double(row$theta_global_rmse_lagged[[1L]] %||% NA_real_)) &&
      as.double(row$theta_global_rmse_lagged[[1L]]) <= theta_global_rmse_max
  } else {
    FALSE
  }
  isTRUE(rel_gate) &&
    isTRUE(hub_gate) &&
    isTRUE(probe_gate) &&
    isTRUE(probe_quality_gate) &&
    isTRUE(probe_rmse_gate) &&
    isTRUE(theta_rmse_gate)
}

.adaptive_link_reconstruct_identified_from_logs <- function(link_row, controller) {
  row <- tibble::as_tibble(link_row)
  if (nrow(row) != 1L) {
    rlang::abort("`link_row` must have exactly one row.")
  }
  controller <- utils::modifyList(.adaptive_controller_defaults(2L), controller %||% list())
  reliability_val <- as.double(
    row[["reliability_link_global"]][[1L]] %||%
      row[["reliability_EAP_link"]][[1L]] %||%
      NA_real_
  )
  rel_gate <- is.finite(reliability_val) &&
    reliability_val >= as.double(controller$link_identified_reliability_min %||% 0.80)
  rank_gate <- is.finite(row$ts_btl_rank_spearman[[1L]]) &&
    row$ts_btl_rank_spearman[[1L]] >= as.double(controller$link_rank_corr_min %||% 0.90)
  isTRUE(rel_gate) && isTRUE(rank_gate)
}

.adaptive_link_phase_a_theta_map <- function(state, set_id, field) {
  phase_a <- state$linking$phase_a %||% list()
  artifact <- (phase_a$artifacts %||% list())[[as.character(set_id)]] %||% NULL
  if (!is.list(artifact)) {
    rlang::abort(paste0("Missing Phase A artifact for set_id=", as.integer(set_id), "."))
  }
  items_tbl <- tibble::as_tibble(artifact$items %||% tibble::tibble())
  required <- c("global_item_id", field)
  if (!all(required %in% names(items_tbl))) {
    rlang::abort(paste0("Phase A artifact for set_id=", as.integer(set_id), " is missing required columns."))
  }
  item_map <- stats::setNames(
    as.character(state$items$item_id),
    as.character(state$items$global_item_id)
  )
  item_id <- item_map[as.character(items_tbl$global_item_id)]
  keep <- !is.na(item_id)
  vals <- as.double(items_tbl[[field]][keep])
  names(vals) <- as.character(item_id[keep])
  vals
}

.adaptive_link_theta_mean_map <- function(state, set_id) {
  fit <- state$btl_fit %||% NULL
  if (!is.list(fit)) {
    return(stats::setNames(numeric(), character()))
  }
  theta_raw <- fit$theta_mean %||% NULL
  if (!is.numeric(theta_raw) || length(theta_raw) < 1L || is.null(names(theta_raw))) {
    return(stats::setNames(numeric(), character()))
  }
  theta <- as.double(theta_raw)
  names(theta) <- as.character(names(theta_raw))
  set_items <- as.character(state$items$item_id[as.integer(state$items$set_id) == as.integer(set_id)])
  theta <- theta[set_items]
  theta[is.finite(theta)]
}

.adaptive_link_theta_sd_map <- function(state, set_id) {
  fit <- state$btl_fit %||% NULL
  if (!is.list(fit)) {
    return(stats::setNames(numeric(), character()))
  }
  theta_raw <- fit$theta_sd %||% NULL
  if (!is.numeric(theta_raw) || length(theta_raw) < 1L || is.null(names(theta_raw))) {
    return(stats::setNames(numeric(), character()))
  }
  theta <- as.double(theta_raw)
  names(theta) <- as.character(names(theta_raw))
  set_items <- as.character(state$items$item_id[as.integer(state$items$set_id) == as.integer(set_id)])
  theta <- theta[set_items]
  theta[is.finite(theta) & theta >= 0]
}

.adaptive_link_judge_params <- function(state,
                                       controller,
                                       scope = c("link", "within"),
                                       allow_cold_start_fallback = FALSE,
                                       expected_link_params = TRUE) {
  scope <- match.arg(scope)
  fit <- state$btl_fit %||% list()
  mode <- as.character(controller$judge_param_mode %||% "global_shared")

  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  pooled_judge <- (state$linking$phase_a %||% list())$pooled_judge_state %||% NULL
  use_pooled <- identical(mode, "global_shared") &&
    identical(scope, "link") &&
    identical(as.character(phase_ctx$phase %||% "phase_a"), "phase_b") &&
    is.list(pooled_judge)

  beta_shared <- if (isTRUE(use_pooled)) {
    as.double(pooled_judge$beta_mean %||% 0)
  } else {
    as.double(fit$beta_mean %||% 0)
  }
  epsilon_shared <- if (isTRUE(use_pooled)) {
    as.double(pooled_judge$epsilon_mean %||% 0)
  } else {
    as.double(fit$epsilon_mean %||% 0)
  }
  if (!is.finite(beta_shared)) {
    beta_shared <- 0
  }
  if (!is.finite(epsilon_shared)) {
    epsilon_shared <- 0
  }

  beta <- beta_shared
  epsilon <- epsilon_shared
  if (identical(mode, "phase_specific")) {
    if (identical(scope, "link")) {
      beta <- as.double(fit$beta_link_mean %||% NA_real_)
      epsilon <- as.double(fit$epsilon_link_mean %||% NA_real_)
      has_beta <- !is.null(fit$beta_link_mean)
      has_epsilon <- !is.null(fit$epsilon_link_mean)
      if (is.finite(beta) && is.finite(epsilon)) {
        # keep parsed values
      } else if (isTRUE(allow_cold_start_fallback) &&
        !isTRUE(expected_link_params) &&
        !xor(has_beta, has_epsilon)) {
        beta_within <- as.double(fit$beta_within_mean %||% NA_real_)
        epsilon_within <- as.double(fit$epsilon_within_mean %||% NA_real_)
        if (is.finite(beta_within) && is.finite(epsilon_within)) {
          beta <- beta_within
          epsilon <- epsilon_within
        } else {
          beta <- beta_shared
          epsilon <- epsilon_shared
        }
      } else {
        rlang::abort(
          paste0(
            "Phase-specific judge mode requires `beta_link_mean` and ",
            "`epsilon_link_mean` in `state$btl_fit`."
          )
        )
      }
    } else {
      beta <- as.double(fit$beta_within_mean %||% NA_real_)
      epsilon <- as.double(fit$epsilon_within_mean %||% NA_real_)
      if (!is.finite(beta) || !is.finite(epsilon)) {
        rlang::abort(
          paste0(
            "Phase-specific judge mode requires `beta_within_mean` and ",
            "`epsilon_within_mean` in `state$btl_fit`."
          )
        )
      }
    }
  }
  if (!is.finite(beta)) {
    beta <- 0
  }
  if (!is.finite(epsilon)) {
    epsilon <- 0
  }
  epsilon <- max(0, min(1, epsilon))

  list(
    mode = mode,
    scope = as.character(scope),
    beta = as.double(beta),
    epsilon = as.double(epsilon),
    cold_start_fallback_used = as.logical(
      isTRUE(identical(mode, "phase_specific")) &&
        isTRUE(identical(scope, "link")) &&
        isTRUE(allow_cold_start_fallback) &&
        !isTRUE(expected_link_params) &&
        (!is.finite(as.double(fit$beta_link_mean %||% NA_real_)) ||
          !is.finite(as.double(fit$epsilon_link_mean %||% NA_real_)))
    )
  )
}

.adaptive_link_phase_b_startup_gap_for_spoke <- function(state, spoke_id) {
  controller <- .adaptive_controller_resolve(state)
  if (!.adaptive_link_mode_active(controller)) {
    return(FALSE)
  }
  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  if (!identical(phase_ctx$phase, "phase_b")) {
    return(FALSE)
  }
  stats_by_spoke <- controller$link_refit_stats_by_spoke %||% list()
  if (!is.null(stats_by_spoke[[as.character(spoke_id)]])) {
    return(FALSE)
  }
  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  if (nrow(step_log) < 1L || !all(c("pair_id", "is_cross_set", "link_spoke_id") %in% names(step_log))) {
    return(TRUE)
  }
  has_cross_spoke <- any(
    !is.na(step_log$pair_id) &
      step_log$is_cross_set %in% TRUE &
      as.integer(step_log$link_spoke_id) == as.integer(spoke_id),
    na.rm = TRUE
  )
  if (!isTRUE(has_cross_spoke)) {
    return(TRUE)
  }
  link_stage_log <- tibble::as_tibble(state$link_stage_log %||% new_link_stage_log())
  if (nrow(link_stage_log) < 1L || !"spoke_id" %in% names(link_stage_log)) {
    return(TRUE)
  }
  has_refit_row <- any(as.integer(link_stage_log$spoke_id) == as.integer(spoke_id), na.rm = TRUE)
  !isTRUE(has_refit_row)
}

.adaptive_phase_a_artifact_item_field_map <- function(state, set_id, field) {
  phase_a <- state$linking$phase_a %||% list()
  artifact <- (phase_a$artifacts %||% list())[[as.character(set_id)]] %||% NULL
  if (!is.list(artifact)) {
    rlang::abort(paste0("Missing Phase A artifact for set_id=", as.integer(set_id), "."))
  }
  items_tbl <- tibble::as_tibble(artifact$items %||% tibble::tibble())
  required <- c("global_item_id", field)
  if (!all(required %in% names(items_tbl))) {
    rlang::abort(paste0("Phase A artifact for set_id=", as.integer(set_id), " is missing required columns."))
  }
  expected_ids <- as.character(
    state$items$item_id[as.integer(state$items$set_id) == as.integer(set_id)]
  )
  global_to_item <- stats::setNames(
    as.character(state$items$item_id),
    as.character(state$items$global_item_id)
  )
  item_ids <- as.character(global_to_item[as.character(items_tbl$global_item_id)])
  keep <- !is.na(item_ids)
  values <- as.double(items_tbl[[field]][keep])
  names(values) <- item_ids[keep]
  out <- stats::setNames(rep(NA_real_, length(expected_ids)), expected_ids)
  out[names(values)] <- values
  out
}

.adaptive_link_cross_edges <- function(state, spoke_id, last_refit_step = NULL) {
  cross_cache <- .adaptive_link_cross_edges_resolve(state)
  cross <- tibble::as_tibble(cross_cache[[as.character(spoke_id)]] %||% .adaptive_link_cross_edges_empty())
  if (!is.null(last_refit_step) && nrow(cross) > 0L) {
    cross <- cross[as.integer(cross$step_id) > as.integer(last_refit_step), , drop = FALSE]
  }
  cross
}

.adaptive_link_cross_edges_empty <- function() {
  empty <- tibble::tibble(
    spoke_item = character(),
    hub_item = character(),
    y_spoke = integer(),
    step_id = integer(),
    spoke_in_A = logical(),
    run_mode = character(),
    is_probe_step = logical(),
    link_stage = character(),
    fallback_used = character()
  )
  empty
}

.adaptive_link_cross_edges_rebuild <- function(state) {
  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  if (nrow(step_log) < 1L) {
    return(list())
  }
  required <- c("pair_id", "step_id", "is_cross_set", "link_spoke_id")
  if (!all(required %in% names(step_log))) {
    return(list())
  }
  controller <- tryCatch(
    .adaptive_controller_resolve(state),
    error = function(e) (state$controller %||% list())
  )
  hub_id <- as.integer(controller$hub_id %||% 1L)
  item_ids <- as.character(state$item_ids %||% character())
  set_ids <- as.integer(state$set_ids %||% integer())
  set_by_item <- if (length(item_ids) > 0L && length(set_ids) == length(item_ids)) {
    stats::setNames(set_ids, item_ids)
  } else {
    NULL
  }
  link_spoke <- as.integer(step_log$link_spoke_id)
  cross <- step_log[
    !is.na(step_log$pair_id) &
      step_log$is_cross_set %in% TRUE &
      !is.na(link_spoke),
    ,
    drop = FALSE
  ]
  if (nrow(cross) < 1L) {
    return(list())
  }
  link_spoke <- as.integer(cross$link_spoke_id)
  spoke_item <- rep(NA_character_, nrow(cross))
  hub_item <- rep(NA_character_, nrow(cross))
  y_spoke <- rep(NA_integer_, nrow(cross))
  spoke_in_A <- rep(NA, nrow(cross))
  idx_a_col <- if ("A" %in% names(cross) && any(!is.na(cross$A))) {
    "A"
  } else if ("i" %in% names(cross) && any(!is.na(cross$i))) {
    "i"
  } else {
    NA_character_
  }
  idx_b_col <- if ("B" %in% names(cross) && any(!is.na(cross$B))) {
    "B"
  } else if ("j" %in% names(cross) && any(!is.na(cross$j))) {
    "j"
  } else {
    NA_character_
  }
  if (!is.na(idx_a_col) && !is.na(idx_b_col)) {
    A_id <- item_ids[as.integer(cross[[idx_a_col]])]
    B_id <- item_ids[as.integer(cross[[idx_b_col]])]
    if (!is.null(set_by_item)) {
      A_set <- as.integer(set_by_item[A_id])
      B_set <- as.integer(set_by_item[B_id])
      spoke_is_A <- (A_set == link_spoke) & (B_set == hub_id)
      spoke_is_B <- (B_set == link_spoke) & (A_set == hub_id)
      keep <- (spoke_is_A %in% TRUE) | (spoke_is_B %in% TRUE)
      if (!any(keep)) {
        return(list())
      }
      cross <- cross[keep, , drop = FALSE]
      A_id <- A_id[keep]
      B_id <- B_id[keep]
      link_spoke <- link_spoke[keep]
      spoke_is_A <- spoke_is_A[keep] %in% TRUE
      spoke_item <- ifelse(spoke_is_A, A_id, B_id)
      hub_item <- ifelse(spoke_is_A, B_id, A_id)
      spoke_in_A <- as.logical(spoke_is_A)
      if ("Y" %in% names(cross)) {
        y <- as.integer(cross$Y)
        y_spoke <- as.integer(ifelse(spoke_is_A, y, 1L - y))
      } else {
        y_spoke <- rep(NA_integer_, nrow(cross))
      }
    } else {
      hub_item <- as.character(A_id)
      spoke_item <- as.character(B_id)
      spoke_in_A <- rep(FALSE, nrow(cross))
      if ("Y" %in% names(cross)) {
        y_spoke <- as.integer(1L - as.integer(cross$Y))
      } else {
        y_spoke <- rep(NA_integer_, nrow(cross))
      }
    }
  }
  link_stage <- if ("link_stage" %in% names(cross)) {
    as.character(cross$link_stage)
  } else if ("round_stage" %in% names(cross)) {
    as.character(cross$round_stage)
  } else {
    rep(NA_character_, nrow(cross))
  }
  out <- tibble::tibble(
    link_spoke_id = as.integer(link_spoke),
    spoke_item = as.character(spoke_item),
    hub_item = as.character(hub_item),
    y_spoke = as.integer(y_spoke),
    step_id = as.integer(cross$step_id),
    spoke_in_A = as.logical(spoke_in_A),
    run_mode = if ("run_mode" %in% names(cross)) {
      as.character(cross$run_mode)
    } else {
      rep(NA_character_, nrow(cross))
    },
    is_probe_step = if ("is_probe_step" %in% names(cross)) {
      as.logical(cross$is_probe_step %||% FALSE)
    } else {
      rep(FALSE, nrow(cross))
    },
    link_stage = as.character(link_stage),
    fallback_used = if ("fallback_used" %in% names(cross)) {
      as.character(cross$fallback_used)
    } else {
      rep(NA_character_, nrow(cross))
    }
  )
  split_out <- split(out, as.character(out$link_spoke_id))
  lapply(split_out, function(x) {
    x <- tibble::as_tibble(x)
    x[, names(.adaptive_link_cross_edges_empty()), drop = FALSE]
  })
}

.adaptive_link_cross_edges_cache <- function(state) {
  refit_meta <- state$refit_meta %||% list()
  cache <- refit_meta$link_cross_edges_by_spoke %||% NULL
  if (!is.list(cache)) {
    return(NULL)
  }
  required <- names(.adaptive_link_cross_edges_empty())
  valid <- vapply(cache, function(x) {
    tbl <- tibble::as_tibble(x)
    all(required %in% names(tbl))
  }, logical(1))
  if (!all(valid)) {
    return(NULL)
  }
  lapply(cache, function(x) {
    tbl <- tibble::as_tibble(x)
    tbl[, required, drop = FALSE]
  })
}

.adaptive_link_cross_edges_memo_signature <- function(state) {
  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  n_rows <- nrow(step_log)
  if (n_rows < 1L) {
    return(list(n_rows = 0L))
  }
  cols <- intersect(
    c("step_id", "pair_id", "i", "j", "A", "B", "Y", "is_cross_set", "link_spoke_id"),
    names(step_log)
  )
  tail_row <- step_log[n_rows, cols, drop = FALSE]
  list(
    n_rows = as.integer(n_rows),
    cols = as.character(cols),
    tail = lapply(tail_row, function(x) x[[1L]])
  )
}

.adaptive_link_cross_edges_resolve <- function(state) {
  refit_meta <- state$refit_meta %||% list()
  cache_built <- isTRUE(refit_meta$link_cross_edges_cache_built %||% FALSE)
  cache <- .adaptive_link_cross_edges_cache(state)
  if (isTRUE(cache_built) && !is.null(cache)) {
    return(cache)
  }
  memo_env <- tryCatch(.adaptive_link_refit_local_memo_env(state), error = function(e) NULL)
  memo_key <- ".link_cross_edges_by_spoke"
  step_id <- as.integer(.adaptive_link_refit_local_step_id(state))
  refit_id <- as.integer(.adaptive_link_refit_window_id(state))
  step_signature <- .adaptive_link_cross_edges_memo_signature(state)
  if (is.environment(memo_env) && exists(memo_key, envir = memo_env, inherits = FALSE)) {
    entry <- memo_env[[memo_key]] %||% list()
    if (identical(as.integer(entry$step_id %||% NA_integer_), step_id) &&
      identical(as.integer(entry$refit_id %||% NA_integer_), refit_id) &&
      identical(entry$step_signature %||% NULL, step_signature) &&
      is.list(entry$value)) {
      return(entry$value)
    }
  }
  rebuilt <- .adaptive_link_cross_edges_rebuild(state)
  if (is.environment(memo_env)) {
    memo_env[[memo_key]] <- list(
      step_id = as.integer(step_id),
      refit_id = as.integer(refit_id),
      step_signature = step_signature,
      value = rebuilt
    )
  }
  rebuilt
}

.adaptive_link_cross_edges_update <- function(cache, state, step_row, A_id, B_id, Y) {
  step_row <- tibble::as_tibble(step_row)
  if (nrow(step_row) != 1L) {
    return(cache %||% list())
  }

  pair_id <- as.integer(step_row$pair_id[[1L]] %||% NA_integer_)
  spoke_id <- if ("link_spoke_id" %in% names(step_row)) {
    as.integer(step_row$link_spoke_id[[1L]] %||% NA_integer_)
  } else {
    NA_integer_
  }
  if (is.na(pair_id) || !isTRUE(step_row$is_cross_set[[1L]] %||% FALSE) || is.na(spoke_id)) {
    return(cache %||% list())
  }

  hub_id <- as.integer(.adaptive_controller_resolve(state)$hub_id %||% 1L)
  set_by_item <- stats::setNames(as.integer(state$set_ids), as.character(state$item_ids))
  A_set <- as.integer(set_by_item[as.character(A_id)] %||% NA_integer_)
  B_set <- as.integer(set_by_item[as.character(B_id)] %||% NA_integer_)
  spoke_is_A <- identical(A_set, spoke_id) && identical(B_set, hub_id)
  spoke_is_B <- identical(B_set, spoke_id) && identical(A_set, hub_id)
  if (!(isTRUE(spoke_is_A) || isTRUE(spoke_is_B))) {
    return(cache %||% list())
  }

  cache <- cache %||% list()
  set_key <- as.character(spoke_id)
  existing <- tibble::as_tibble(cache[[set_key]] %||% .adaptive_link_cross_edges_empty())
  link_stage <- if ("link_stage" %in% names(step_row)) {
    as.character(step_row$link_stage[[1L]] %||% NA_character_)
  } else if ("round_stage" %in% names(step_row)) {
    as.character(step_row$round_stage[[1L]] %||% NA_character_)
  } else {
    NA_character_
  }
  fallback_used <- if ("fallback_used" %in% names(step_row)) {
    as.character(step_row$fallback_used[[1L]] %||% NA_character_)
  } else {
    NA_character_
  }
  updated <- dplyr::bind_rows(
    existing,
    tibble::tibble(
      spoke_item = if (isTRUE(spoke_is_A)) as.character(A_id) else as.character(B_id),
      hub_item = if (isTRUE(spoke_is_A)) as.character(B_id) else as.character(A_id),
      y_spoke = as.integer(if (isTRUE(spoke_is_A)) Y else 1L - as.integer(Y)),
      step_id = as.integer(step_row$step_id[[1L]] %||% NA_integer_),
      spoke_in_A = as.logical(spoke_is_A),
      run_mode = as.character(step_row$run_mode[[1L]] %||% NA_character_),
      is_probe_step = as.logical(step_row$is_probe_step[[1L]] %||% FALSE),
      link_stage = as.character(link_stage),
      fallback_used = as.character(fallback_used)
    )
  )
  cache[[set_key]] <- updated[order(updated$step_id), names(.adaptive_link_cross_edges_empty()), drop = FALSE]
  cache
}

.adaptive_link_within_edges <- function(state, set_id) {
  evidence <- .adaptive_phase_a_within_set_evidence_resolve(state, set_id = as.integer(set_id))
  if (nrow(evidence) < 1L) {
    return(tibble::tibble(A_item = character(), B_item = character(), y_A = integer(), step_id = integer()))
  }
  tibble::tibble(
    A_item = as.character(evidence$A_item),
    B_item = as.character(evidence$B_item),
    y_A = as.integer(evidence$y_A),
    step_id = as.integer(evidence$step_id)
  )
}

.adaptive_link_fit_summaries_finite <- function(fit) {
  fit <- fit %||% list()

  theta_hub <- fit$theta_hub_post %||% numeric()
  theta_spoke <- fit$theta_spoke_post %||% numeric()
  if (length(theta_hub) > 0L && any(!is.finite(as.double(theta_hub)))) {
    return(FALSE)
  }
  if (length(theta_spoke) > 0L && any(!is.finite(as.double(theta_spoke)))) {
    return(FALSE)
  }

  delta_mean <- as.double(fit$delta_mean %||% NA_real_)
  if (!is.na(delta_mean) && !is.finite(delta_mean)) {
    return(FALSE)
  }

  log_alpha_mean <- as.double(fit$log_alpha_mean %||% NA_real_)
  if (!is.na(log_alpha_mean) && !is.finite(log_alpha_mean)) {
    return(FALSE)
  }

  TRUE
}

.adaptive_link_fit_uncertainty_available <- function(fit) {
  fit <- fit %||% list()

  theta_spoke_sd <- fit$theta_spoke_sd_post %||% NULL
  if (!is.null(theta_spoke_sd)) {
    theta_spoke_sd <- as.double(theta_spoke_sd)
    if (length(theta_spoke_sd) > 0L &&
      all(is.finite(theta_spoke_sd)) &&
      all(theta_spoke_sd >= 0)) {
      return(TRUE)
    }
  }

  delta_sd <- as.double(fit$delta_sd %||% NA_real_)
  log_alpha_sd <- as.double(fit$log_alpha_sd %||% NA_real_)
  delta_ok <- is.na(delta_sd) || (is.finite(delta_sd) && delta_sd >= 0)
  log_alpha_ok <- is.na(log_alpha_sd) || (is.finite(log_alpha_sd) && log_alpha_sd >= 0)

  isTRUE(delta_ok) && isTRUE(log_alpha_ok)
}

.adaptive_link_diagnostics_contract <- function(fit) {
  fit <- fit %||% list()
  fit_contract <- fit$fit_contract %||% list()
  contract_type <- as.character(fit_contract$contract_type %||% NA_character_)
  fit_method <- as.character(fit_contract$estimation_method %||% NA_character_)
  diagnostics <- fit$diagnostics %||% list()
  if (is.na(fit_method) || !nzchar(fit_method)) {
    has_hmc_diag <- any(c(
      "divergences", "max_rhat", "min_ess_bulk",
      "diagnostics_divergences_pass", "diagnostics_rhat_pass",
      "diagnostics_ess_pass"
    ) %in% names(diagnostics))
    has_det_diag <- any(c("converged", "hessian_posdef") %in% names(diagnostics))
    if (isTRUE(has_hmc_diag)) {
      fit_method <- "cmdstan_hmc"
    } else if (isTRUE(has_det_diag)) {
      fit_method <- "map_laplace"
    }
  }
  if ((is.na(fit_method) || !nzchar(fit_method)) &&
    identical(contract_type, "link_refit_frozen_reuse")) {
    fit_method <- "accepted_state_reuse"
  }
  uncertainty <- as.character(fit_contract$uncertainty_approximation %||% NA_character_)
  if ((is.na(uncertainty) || !nzchar(uncertainty)) &&
    identical(fit_method, "cmdstan_hmc")) {
    uncertainty <- "cmdstan_posterior_draws"
  }
  if ((is.na(uncertainty) || !nzchar(uncertainty)) &&
    identical(fit_method, "map_laplace")) {
    uncertainty <- "laplace_hessian"
  }
  if ((is.na(uncertainty) || !nzchar(uncertainty)) &&
    identical(fit_method, "accepted_state_reuse")) {
    uncertainty <- "accepted_state"
  }
  if (identical(fit_method, "cmdstan_hmc")) {
    diagnostics_pass <- isTRUE(diagnostics$diagnostics_divergences_pass %||% NA) &&
      isTRUE(diagnostics$diagnostics_rhat_pass %||% NA) &&
      isTRUE(diagnostics$diagnostics_ess_pass %||% NA)
    return(list(
      link_fit_method = fit_method,
      link_uncertainty_approximation = uncertainty,
      link_diagnostics_pass = diagnostics_pass,
      link_diagnostics_converged_pass = NA,
      link_diagnostics_finite_summary_pass = NA,
      link_diagnostics_uncertainty_pass = NA,
      link_diagnostics_divergences = as.integer(diagnostics$divergences %||% NA_integer_),
      link_diagnostics_max_rhat = as.double(diagnostics$max_rhat %||% NA_real_),
      link_diagnostics_min_ess_bulk = as.double(diagnostics$min_ess_bulk %||% NA_real_),
      link_diagnostics_divergences_pass = as.logical(
        diagnostics$diagnostics_divergences_pass %||% NA
      ),
      link_diagnostics_rhat_pass = as.logical(diagnostics$diagnostics_rhat_pass %||% NA),
      link_diagnostics_ess_pass = as.logical(diagnostics$diagnostics_ess_pass %||% NA)
    ))
  }

  if (identical(fit_method, "map_laplace")) {
    converged_pass <- as.logical(diagnostics$converged %||% NA)
    hessian_posdef <- as.logical(diagnostics$hessian_posdef %||% NA)
    if (is.na(converged_pass) || is.na(hessian_posdef)) {
      rlang::abort(
        paste0(
          "Deterministic linking diagnostics contract requires `converged` and ",
          "`hessian_posdef` for `map_laplace` fits."
        )
      )
    }
    if (!identical(uncertainty, "laplace_hessian")) {
      rlang::abort(
        paste0(
          "Deterministic linking diagnostics contract requires ",
          "`uncertainty_approximation = \"laplace_hessian\"` for `map_laplace` fits."
        )
      )
    }
    finite_summary_pass <- .adaptive_link_fit_summaries_finite(fit)
    uncertainty_pass <- isTRUE(hessian_posdef) &&
      isTRUE(.adaptive_link_fit_uncertainty_available(fit))
    return(list(
      link_fit_method = fit_method,
      link_uncertainty_approximation = uncertainty,
      link_diagnostics_pass = isTRUE(converged_pass) &&
        isTRUE(finite_summary_pass) &&
        isTRUE(uncertainty_pass),
      link_diagnostics_converged_pass = as.logical(converged_pass),
      link_diagnostics_finite_summary_pass = as.logical(finite_summary_pass),
      link_diagnostics_uncertainty_pass = as.logical(uncertainty_pass),
      link_diagnostics_divergences = NA_integer_,
      link_diagnostics_max_rhat = NA_real_,
      link_diagnostics_min_ess_bulk = NA_real_,
      link_diagnostics_divergences_pass = NA,
      link_diagnostics_rhat_pass = NA,
      link_diagnostics_ess_pass = NA
    ))
  }

  if (identical(fit_method, "accepted_state_reuse")) {
    finite_summary_pass <- .adaptive_link_fit_summaries_finite(fit)
    uncertainty_pass <- .adaptive_link_fit_uncertainty_available(fit)
    return(list(
      link_fit_method = fit_method,
      link_uncertainty_approximation = uncertainty,
      link_diagnostics_pass = isTRUE(finite_summary_pass) &&
        isTRUE(uncertainty_pass),
      link_diagnostics_converged_pass = TRUE,
      link_diagnostics_finite_summary_pass = as.logical(finite_summary_pass),
      link_diagnostics_uncertainty_pass = as.logical(uncertainty_pass),
      link_diagnostics_divergences = NA_integer_,
      link_diagnostics_max_rhat = NA_real_,
      link_diagnostics_min_ess_bulk = NA_real_,
      link_diagnostics_divergences_pass = NA,
      link_diagnostics_rhat_pass = NA,
      link_diagnostics_ess_pass = NA
    ))
  }

  rlang::abort(
    paste0(
      "Linking diagnostics contract is undefined for fit method `",
      fit_method,
      "`."
    )
  )
}

.adaptive_link_ppc_brier_cross <- function(cross_edges,
                                           hub_theta,
                                           spoke_theta,
                                           delta_mean,
                                           log_alpha_mean = NA_real_,
                                           posterior_draws = NULL) {
  judge_params <- attr(cross_edges, "judge_params", exact = TRUE) %||% list(beta = 0, epsilon = 0)
  beta <- as.double(judge_params$beta %||% 0)
  epsilon <- as.double(judge_params$epsilon %||% 0)
  if (!is.finite(beta)) {
    beta <- 0
  }
  if (!is.finite(epsilon)) {
    epsilon <- 0
  }
  epsilon <- max(0, min(1, epsilon))
  edges <- tibble::as_tibble(cross_edges)
  if (nrow(edges) < 1L) return(NA_real_)
  h <- as.double(hub_theta[as.character(edges$hub_item)])
  s <- as.double(spoke_theta[as.character(edges$spoke_item)])
  spoke_in_A <- as.logical(edges$spoke_in_A %||% rep(TRUE, nrow(edges)))
  beta_sign <- ifelse(spoke_in_A, 1, -1)
  beta_signed <- beta * as.double(beta_sign)
  y <- as.integer(edges$y_spoke)
  keep <- is.finite(h) & is.finite(s) & y %in% c(0L, 1L) & is.finite(beta_signed)
  if (!any(keep)) return(NA_real_)
  h <- h[keep]
  s <- s[keep]
  y <- y[keep]
  beta_signed <- beta_signed[keep]
  edges_keep <- edges[keep, , drop = FALSE]

  draws <- posterior_draws %||% list()
  delta_draws <- as.double(draws$delta %||% numeric())
  if (length(delta_draws) > 0L) {
    log_alpha_draws <- as.double(draws$log_alpha %||% rep(NA_real_, length(delta_draws)))
    if (length(log_alpha_draws) != length(delta_draws)) {
      log_alpha_draws <- rep(NA_real_, length(delta_draws))
    }
    hub_draws <- as.matrix(draws$theta_hub %||% matrix(numeric(), nrow = 0, ncol = 0))
    spoke_draws <- as.matrix(draws$theta_spoke %||% matrix(numeric(), nrow = 0, ncol = 0))
    n_draws <- length(delta_draws)
    draw_idx <- seq_len(n_draws)
    if (n_draws > 200L) {
      draw_idx <- unique(as.integer(round(seq(1, n_draws, length.out = 200L))))
    }
    brier <- rep(NA_real_, length(draw_idx))
    for (k in seq_along(draw_idx)) {
      d <- draw_idx[[k]]
      h_k <- h
      s_k <- s
      if (nrow(hub_draws) >= d && ncol(hub_draws) > 0L && !is.null(colnames(hub_draws))) {
        h_map <- as.double(hub_draws[d, as.character(edges_keep$hub_item), drop = TRUE])
        if (all(is.finite(h_map))) {
          h_k <- h_map
        }
      }
      if (nrow(spoke_draws) >= d && ncol(spoke_draws) > 0L && !is.null(colnames(spoke_draws))) {
        s_map <- as.double(spoke_draws[d, as.character(edges_keep$spoke_item), drop = TRUE])
        if (all(is.finite(s_map))) {
          s_k <- s_map
        }
      }
      alpha <- if (is.finite(log_alpha_draws[[d]])) exp(log_alpha_draws[[d]]) else 1
      eta <- delta_draws[[d]] + alpha * s_k - h_k + beta_signed
      p <- (1 - epsilon) * stats::plogis(eta) + epsilon * 0.5
      brier[[k]] <- mean((as.double(y) - p)^2)
    }
    out <- mean(brier[is.finite(brier)])
    if (is.finite(out)) {
      return(as.double(out))
    }
  }

  alpha <- if (is.finite(log_alpha_mean)) exp(log_alpha_mean) else 1
  eta <- as.double(delta_mean) + alpha * s - h + beta_signed
  p <- (1 - epsilon) * stats::plogis(eta) + epsilon * 0.5
  as.double(mean((as.double(y) - p)^2))
}

.adaptive_link_probe_edges_realized <- function(state, spoke_id, epoch_id) {
  panel <- .adaptive_link_probe_panel_for_spoke(state, spoke_id = spoke_id, epoch_id = epoch_id)
  if (nrow(panel) < 1L) {
    return(tibble::tibble())
  }
  realized_log <- .adaptive_link_probe_realized_log_for_panel(
    state = state,
    spoke_id = as.integer(spoke_id),
    epoch_id = as.integer(epoch_id),
    panel = panel
  )
  if (nrow(realized_log) < 1L) {
    return(tibble::tibble())
  }
  panel_keys <- as.character(realized_log$pair_key)
  cross <- .adaptive_link_cross_edges(state, spoke_id = spoke_id, last_refit_step = NULL)
  if (nrow(cross) < 1L) {
    return(tibble::tibble())
  }
  cross <- tibble::as_tibble(cross)
  cross$pair_key <- make_unordered_key(cross$hub_item, cross$spoke_item)
  cross[cross$pair_key %in% panel_keys & cross$is_probe_step %in% TRUE, , drop = FALSE]
}

.adaptive_link_probe_prior_realized_max <- function(link_stage_log, spoke_id, epoch_id, refit_id) {
  link_stage_log <- tibble::as_tibble(link_stage_log %||% new_link_stage_log())
  if (nrow(link_stage_log) < 1L) {
    return(NA_integer_)
  }
  rows <- link_stage_log[
    as.integer(link_stage_log$spoke_id) == as.integer(spoke_id) &
      as.integer(link_stage_log$link_epoch_id) == as.integer(epoch_id) &
      as.integer(link_stage_log$refit_id) < as.integer(refit_id),
    ,
    drop = FALSE
  ]
  if (nrow(rows) < 1L || !"probe_edges_realized" %in% names(rows)) {
    return(NA_integer_)
  }
  vals <- as.integer(rows$probe_edges_realized)
  vals <- vals[is.finite(vals) & !is.na(vals)]
  if (length(vals) < 1L) {
    return(NA_integer_)
  }
  as.integer(max(vals))
}

.adaptive_link_probe_brier_for_fit <- function(edges,
                                               hub_theta,
                                               spoke_theta,
                                               delta_mean,
                                               log_alpha_mean = NA_real_,
                                               judge_params = list(beta = 0, epsilon = 0)) {
  edges <- tibble::as_tibble(edges)
  if (nrow(edges) < 1L) {
    return(NA_real_)
  }
  p <- .adaptive_link_cross_probabilities(
    edges = edges,
    hub_theta = hub_theta,
    spoke_theta = spoke_theta,
    delta_mean = delta_mean,
    log_alpha_mean = log_alpha_mean,
    judge_params = judge_params
  )
  y <- as.integer(edges$y_spoke)
  keep <- y %in% c(0L, 1L) & is.finite(p)
  if (!any(keep)) {
    return(NA_real_)
  }
  as.double(mean((y[keep] - p[keep])^2))
}

.adaptive_link_probe_calibration_ece <- function(p, y, n_bins = 5L) {
  keep <- is.finite(p) & y %in% c(0L, 1L)
  p <- as.double(p[keep])
  y <- as.double(y[keep])
  if (length(p) < 1L) {
    return(NA_real_)
  }
  n_bins <- max(1L, as.integer(n_bins %||% 5L))
  bins <- cut(p, breaks = seq(0, 1, length.out = n_bins + 1L), include.lowest = TRUE, labels = FALSE)
  as.double(sum(vapply(seq_len(n_bins), function(bin_id) {
    idx <- which(bins == bin_id)
    if (length(idx) < 1L) {
      return(0)
    }
    (length(idx) / length(p)) * abs(mean(y[idx]) - mean(p[idx]))
  }, numeric(1L))))
}

.adaptive_link_probe_quality_metrics <- function(edges, panel, hub_theta, spoke_theta,
                                                 delta_mean, log_alpha_mean = NA_real_,
                                                 judge_params = list(beta = 0, epsilon = 0),
                                                 controller = list()) {
  edges <- tibble::as_tibble(edges)
  panel <- tibble::as_tibble(panel)
  p <- .adaptive_link_cross_probabilities(edges, hub_theta, spoke_theta,
    delta_mean, log_alpha_mean, judge_params)
  keep <- edges$y_spoke %in% c(0L, 1L) & is.finite(p)
  edges <- edges[keep, , drop = FALSE]
  match_panel <- match(make_unordered_key(edges$hub_item, edges$spoke_item), panel$pair_key)
  .link_probe_quality(p[keep], edges$y_spoke, edges$hub_item, edges$spoke_item,
    unique(stats::na.omit(panel$hub_bin[match_panel])),
    unique(stats::na.omit(panel$spoke_bin[match_panel])), controller)
}

.adaptive_link_cross_probabilities <- function(edges,
                                               hub_theta,
                                               spoke_theta,
                                               delta_mean,
                                               log_alpha_mean = NA_real_,
                                               judge_params = list(beta = 0, epsilon = 0)) {
  edges <- tibble::as_tibble(edges)
  if (nrow(edges) < 1L) {
    return(numeric())
  }
  beta <- as.double(judge_params$beta %||% 0)
  epsilon <- max(0, min(1, as.double(judge_params$epsilon %||% 0)))
  alpha <- if (is.finite(log_alpha_mean)) exp(log_alpha_mean) else 1
  h <- as.double(hub_theta[as.character(edges$hub_item)])
  s <- as.double(spoke_theta[as.character(edges$spoke_item)])
  spoke_in_A <- as.logical(edges$spoke_in_A %||% TRUE)
  beta_signed <- ifelse(spoke_in_A, beta, -beta)
  eta <- as.double(delta_mean) + alpha * s - h + beta_signed
  p <- (1 - epsilon) * stats::plogis(eta) + epsilon * 0.5
  p[!is.finite(h) | !is.finite(s)] <- NA_real_
  as.double(p)
}

.adaptive_link_probe_pred_rmse_lagged_for_fit <- function(edges,
                                                           hub_theta,
                                                           spoke_theta,
                                                           delta_mean,
                                                           log_alpha_mean,
                                                           lag_delta_mean,
                                                           lag_log_alpha_mean,
                                                           judge_params = list(beta = 0, epsilon = 0)) {
  edges <- tibble::as_tibble(edges)
  if (nrow(edges) < 1L) {
    return(NA_real_)
  }
  p_now <- .adaptive_link_cross_probabilities(
    edges = edges,
    hub_theta = hub_theta,
    spoke_theta = spoke_theta,
    delta_mean = delta_mean,
    log_alpha_mean = log_alpha_mean,
    judge_params = judge_params
  )
  p_lag <- .adaptive_link_cross_probabilities(
    edges = edges,
    hub_theta = hub_theta,
    spoke_theta = spoke_theta,
    delta_mean = lag_delta_mean,
    log_alpha_mean = lag_log_alpha_mean,
    judge_params = judge_params
  )
  keep <- is.finite(p_now) & is.finite(p_lag)
  if (!any(keep)) {
    return(NA_real_)
  }
  sqrt(mean((p_now[keep] - p_lag[keep])^2))
}

.adaptive_link_concurrent_targets <- function(spoke_stats, total_pairs, floor_pairs) {
  if (length(spoke_stats) < 1L) {
    return(integer())
  }
  keys <- names(spoke_stats)
  total_pairs <- as.integer(max(0L, total_pairs))
  floor_pairs <- as.integer(max(0L, floor_pairs))
  candidate_count <- vapply(
    spoke_stats,
    function(x) as.integer(x$concurrent_candidate_count %||% x$candidate_count %||% NA_integer_),
    integer(1L)
  )
  utility_mass <- vapply(
    spoke_stats,
    function(x) as.double(x$concurrent_utility_mass %||% x$utility_mass %||% x$uncertainty %||% 0),
    numeric(1L)
  )
  utility_mass[!is.finite(utility_mass) | utility_mass < 0] <- 0
  non_starved <- is.na(candidate_count) | candidate_count > 0L

  out <- stats::setNames(rep.int(0L, length(keys)), keys)
  active_keys <- keys[non_starved]
  if (length(active_keys) < 1L || total_pairs <= 0L) {
    return(out)
  }
  finite_capacity <- candidate_count[active_keys][!is.na(candidate_count[active_keys])]
  if (length(finite_capacity) == length(active_keys)) {
    total_capacity <- as.integer(sum(pmax(0L, finite_capacity), na.rm = TRUE))
    total_pairs <- as.integer(min(total_pairs, total_capacity))
    if (total_pairs <= 0L) {
      return(out)
    }
  }

  if (total_pairs <= floor_pairs * length(active_keys)) {
    ord <- order(active_keys)
    cursor <- 1L
    stalled_cycles <- 0L
    while (sum(out) < total_pairs) {
      key <- active_keys[[ord[[cursor]]]]
      cap <- candidate_count[[key]]
      allocated <- FALSE
      if (is.na(cap) || out[[key]] < cap) {
        out[[key]] <- out[[key]] + 1L
        allocated <- TRUE
      }
      cursor <- if (cursor >= length(ord)) 1L else cursor + 1L
      stalled_cycles <- if (isTRUE(allocated)) 0L else stalled_cycles + 1L
      if (stalled_cycles >= length(ord)) {
        break
      }
    }
    return(stats::setNames(as.integer(out), keys))
  }

  out[active_keys] <- floor_pairs
  capped_keys <- active_keys[!is.na(candidate_count[active_keys])]
  if (length(capped_keys) > 0L) {
    out[capped_keys] <- pmin(out[capped_keys], candidate_count[capped_keys])
  }

  rem <- max(0L, total_pairs - sum(out))
  weight_keys <- active_keys
  weights <- utility_mass[weight_keys]
  if (sum(weights) <= 0) {
    weights[] <- 1
  }
  add <- floor(rem * weights / sum(weights))
  names(add) <- weight_keys
  out[weight_keys] <- out[weight_keys] + as.integer(add)
  left <- rem - sum(add)
  if (left > 0L) {
    ord <- order(-weights, as.integer(weight_keys))
    for (key in weight_keys[ord]) {
      if (left <= 0L) {
        break
      }
      out[[key]] <- out[[key]] + 1L
      left <- left - 1L
    }
  }

  redistribute <- TRUE
  while (isTRUE(redistribute)) {
    redistribute <- FALSE
    capped <- keys[!is.na(candidate_count)]
    over <- capped[out[capped] > candidate_count[capped]]
    if (length(over) > 0L) {
      unused <- sum(out[over] - candidate_count[over])
      out[over] <- candidate_count[over]
      if (unused > 0L) {
        receivers <- active_keys[
          is.na(candidate_count[active_keys]) | out[active_keys] < candidate_count[active_keys]
        ]
        if (length(receivers) > 0L) {
          receiver_weights <- utility_mass[receivers]
          if (sum(receiver_weights) <= 0) {
            receiver_weights[] <- 1
          }
          ord <- order(-receiver_weights, as.integer(receivers))
          allocated_unused <- 0L
          for (idx in seq_len(unused)) {
            receiver <- receivers[[ord[[(idx - 1L) %% length(ord) + 1L]]]]
            cap <- candidate_count[[receiver]]
            if (is.na(cap) || out[[receiver]] < cap) {
              out[[receiver]] <- out[[receiver]] + 1L
              allocated_unused <- allocated_unused + 1L
            }
          }
          redistribute <- allocated_unused > 0L
        }
      }
    }
  }

  out <- stats::setNames(as.integer(out), keys)
  out
}

#' @keywords internal
#' @noRd
.adaptive_link_stage_backfill_audit_columns <- function(link_rows) {
  rows <- tibble::as_tibble(link_rows)
  if (nrow(rows) < 1L) {
    return(rows)
  }
  stage_target_or_na <- function(col) {
    if (col %in% names(rows)) {
      return(rows[[col]])
    }
    rep(NA_integer_, nrow(rows))
  }
  defaults <- list(
    feasible_stage_capacity_anchor_link = stage_target_or_na("stage_target_anchor_link"),
    feasible_stage_capacity_long_link = stage_target_or_na("stage_target_long_link"),
    feasible_stage_capacity_mid_link = stage_target_or_na("stage_target_mid_link"),
    feasible_stage_capacity_local_link = stage_target_or_na("stage_target_local_link"),
    feasibility_budget_released = rep(0L, nrow(rows)),
    feasibility_reallocation_used = rep(FALSE, nrow(rows)),
    feasibility_reallocation_rule = rep("none", nrow(rows)),
    stop_blocker_codes = rep(NA_character_, nrow(rows)),
    probe_edges_realized_before_refit = rep(NA_integer_, nrow(rows)),
    probe_edges_realized_delta_since_last_refit = rep(NA_integer_, nrow(rows)),
    probe_shortfall_reason = rep(NA_character_, nrow(rows)),
    stop_recent_pass_count = rep(NA_integer_, nrow(rows)),
    stop_recent_window_size = rep(NA_integer_, nrow(rows)),
    stability_window_refits_used = rep(NA_integer_, nrow(rows)),
    stability_passes_required_used = rep(NA_integer_, nrow(rows)),
    escalation_recent_pass_count = rep(NA_integer_, nrow(rows)),
    escalation_recent_window_size = rep(NA_integer_, nrow(rows)),
    link_transform_escalation_window_refits_used = rep(NA_integer_, nrow(rows)),
    link_transform_escalation_passes_required_used = rep(NA_integer_, nrow(rows)),
    link_diagnostics_pass = rep(NA, nrow(rows)),
    link_diagnostics_converged_pass = rep(NA, nrow(rows)),
    link_diagnostics_finite_summary_pass = rep(NA, nrow(rows)),
    link_diagnostics_uncertainty_pass = rep(NA, nrow(rows)),
    link_stop_reliability_min_used = rep(NA_real_, nrow(rows)),
    probe_brier_max_used = rep(NA_real_, nrow(rows)),
    probe_brier_pass = rep(NA, nrow(rows)),
    probe_near_boundary_frac = rep(NA_real_, nrow(rows)),
    probe_near_boundary_min_frac_used = rep(NA_real_, nrow(rows)),
    probe_near_boundary_pass = rep(NA, nrow(rows)),
    probe_extreme_frac = rep(NA_real_, nrow(rows)),
    probe_extreme_max_frac_used = rep(NA_real_, nrow(rows)),
    probe_extreme_frac_pass = rep(NA, nrow(rows)),
    probe_midrange_frac = rep(NA_real_, nrow(rows)),
    probe_midrange_min_frac_used = rep(NA_real_, nrow(rows)),
    probe_midrange_pass = rep(NA, nrow(rows)),
    probe_unique_hub_items = rep(NA_integer_, nrow(rows)),
    probe_unique_hub_min_used = rep(NA_integer_, nrow(rows)),
    probe_unique_hub_pass = rep(NA, nrow(rows)),
    probe_unique_spoke_items = rep(NA_integer_, nrow(rows)),
    probe_unique_spoke_min_used = rep(NA_integer_, nrow(rows)),
    probe_unique_spoke_pass = rep(NA, nrow(rows)),
    probe_rank_bins_hub_covered = rep(NA_integer_, nrow(rows)),
    probe_rank_bins_hub_min_used = rep(NA_integer_, nrow(rows)),
    probe_rank_bins_hub_pass = rep(NA, nrow(rows)),
    probe_rank_bins_spoke_covered = rep(NA_integer_, nrow(rows)),
    probe_rank_bins_spoke_min_used = rep(NA_integer_, nrow(rows)),
    probe_rank_bins_spoke_pass = rep(NA, nrow(rows)),
    probe_brier_near_boundary = rep(NA_real_, nrow(rows)),
    probe_brier_near_boundary_max_used = rep(NA_real_, nrow(rows)),
    probe_brier_near_boundary_pass = rep(NA, nrow(rows)),
    probe_ece = rep(NA_real_, nrow(rows)),
    probe_ece_max_used = rep(NA_real_, nrow(rows)),
    probe_ece_pass = rep(NA, nrow(rows)),
    probe_quality_pass = rep(NA, nrow(rows)),
    probe_quality_blocker_codes = rep(NA_character_, nrow(rows)),
    probe_pred_rmse_max_used = rep(NA_real_, nrow(rows)),
    probe_pred_rmse_pass = rep(NA, nrow(rows)),
    theta_global_rmse_max_used = rep(NA_real_, nrow(rows)),
    theta_global_rmse_pass = rep(NA, nrow(rows)),
    lag_domain_reset_reason = rep(NA_character_, nrow(rows)),
    resumed_from_session = rep(NA, nrow(rows))
  )
  for (col in names(defaults)) {
    if (!col %in% names(rows)) {
      rows[[col]] <- defaults[[col]]
    }
  }
  rows
}

#' @keywords internal
#' @noRd
.adaptive_assert_link_stage_budget_invariants <- function(link_rows) {
  rows <- .adaptive_link_stage_backfill_audit_columns(link_rows)
  if (nrow(rows) < 1L) {
    return(invisible(TRUE))
  }
  required <- c(
    "B_spoke_refit_budget",
    "stage_target_anchor_link",
    "stage_target_long_link",
    "stage_target_mid_link",
    "stage_target_local_link",
    "feasible_stage_capacity_anchor_link",
    "feasible_stage_capacity_long_link",
    "feasible_stage_capacity_mid_link",
    "feasible_stage_capacity_local_link",
    "feasibility_budget_released",
    "feasibility_reallocation_used",
    "feasibility_reallocation_rule",
    "stage_realized_anchor_link",
    "stage_realized_long_link",
    "stage_realized_mid_link",
    "stage_realized_local_link",
    "stage_shortfall_anchor_link",
    "stage_shortfall_long_link",
    "stage_shortfall_mid_link",
    "stage_shortfall_local_link",
    "stage_reallocation_used",
    "stage_reallocation_rule_used",
    "stage_budget_unfilled"
  )
  missing <- setdiff(required, names(rows))
  if (length(missing) > 0L) {
    rlang::abort(
      paste0(
        "link_stage_log budget invariant failure: missing columns: ",
        paste(missing, collapse = ", "),
        "."
      )
    )
  }

  target_sum <- rows$stage_target_anchor_link +
    rows$stage_target_long_link +
    rows$stage_target_mid_link +
    rows$stage_target_local_link
  realized_sum <- rows$stage_realized_anchor_link +
    rows$stage_realized_long_link +
    rows$stage_realized_mid_link +
    rows$stage_realized_local_link
  if (any(target_sum != rows$B_spoke_refit_budget, na.rm = TRUE)) {
    rlang::abort("link_stage_log budget invariant failure: targets must sum to the per-spoke budget.")
  }
  if (any(realized_sum + rows$stage_budget_unfilled != rows$B_spoke_refit_budget, na.rm = TRUE)) {
    bad_idx <- which(realized_sum + rows$stage_budget_unfilled != rows$B_spoke_refit_budget)[[1L]]
    rlang::abort(
      paste0(
        "link_stage_log budget invariant failure: realized counts plus ",
        "unfilled budget must sum to the per-spoke budget. ",
        "refit_id=", rows$refit_id[[bad_idx]],
        ", spoke_id=", rows$spoke_id[[bad_idx]],
        ", budget=", rows$B_spoke_refit_budget[[bad_idx]],
        ", realized_sum=", realized_sum[[bad_idx]],
        ", unfilled=", rows$stage_budget_unfilled[[bad_idx]],
        "."
      )
    )
  }

  nonneg_cols <- c(
    "B_spoke_refit_budget",
    "stage_target_anchor_link",
    "stage_target_long_link",
    "stage_target_mid_link",
    "stage_target_local_link",
    "feasible_stage_capacity_anchor_link",
    "feasible_stage_capacity_long_link",
    "feasible_stage_capacity_mid_link",
    "feasible_stage_capacity_local_link",
    "feasibility_budget_released",
    "stage_realized_anchor_link",
    "stage_realized_long_link",
    "stage_realized_mid_link",
    "stage_realized_local_link",
    "stage_shortfall_anchor_link",
    "stage_shortfall_long_link",
    "stage_shortfall_mid_link",
    "stage_shortfall_local_link",
    "stage_budget_unfilled"
  )
  for (col in nonneg_cols) {
    if (any(rows[[col]] < 0L, na.rm = TRUE)) {
      rlang::abort(
        paste0("link_stage_log budget invariant failure: `", col, "` must be non-negative.")
      )
    }
  }

  inactive_feasibility <- rows$feasibility_reallocation_used %in% FALSE
  if (any(inactive_feasibility, na.rm = TRUE) &&
    any(
      as.character(rows$feasibility_reallocation_rule[inactive_feasibility]) != "none",
      na.rm = TRUE
    )) {
    rlang::abort(
      "link_stage_log budget invariant failure: non-reallocated feasibility rows must use rule `none`."
    )
  }
  active_feasibility <- rows$feasibility_reallocation_used %in% TRUE
  if (any(active_feasibility, na.rm = TRUE) &&
    any(
      as.character(rows$feasibility_reallocation_rule[active_feasibility]) != "pooled_utility_backfill",
      na.rm = TRUE
    )) {
    rlang::abort(
      "link_stage_log budget invariant failure: reallocated feasibility rows must use rule `pooled_utility_backfill`."
    )
  }

  no_backfill <- rows$stage_reallocation_used %in% FALSE
  if (any(no_backfill, na.rm = TRUE)) {
    subset <- rows[no_backfill, , drop = FALSE]
    if (any(subset$stage_realized_anchor_link > subset$stage_target_anchor_link, na.rm = TRUE) ||
      any(subset$stage_realized_long_link > subset$stage_target_long_link, na.rm = TRUE) ||
      any(subset$stage_realized_mid_link > subset$stage_target_mid_link, na.rm = TRUE) ||
      any(subset$stage_realized_local_link > subset$stage_target_local_link, na.rm = TRUE)) {
      rlang::abort(
        "link_stage_log budget invariant failure: no-backfill rows cannot exceed stage targets."
      )
    }
    if (any(as.character(subset$stage_reallocation_rule_used) != "none", na.rm = TRUE)) {
      rlang::abort(
        "link_stage_log budget invariant failure: no-backfill rows must use reallocation rule `none`."
      )
    }
  }

  backfill <- rows$stage_reallocation_used %in% TRUE
  if (any(backfill, na.rm = TRUE) &&
    any(as.character(rows$stage_reallocation_rule_used[backfill]) != "pooled_utility_backfill", na.rm = TRUE)) {
    rlang::abort(
      "link_stage_log budget invariant failure: backfill rows must use reallocation rule `pooled_utility_backfill`."
    )
  }

  invisible(TRUE)
}

#' @keywords internal
#' @noRd
.adaptive_link_budget_map_for_refit <- function(state,
                                                controller = NULL,
                                                eligible_spoke_ids = NULL,
                                                compact_for_feasibility = TRUE,
                                                seed = 1L) {
  compact_budget_source <- function(source) {
    source <- as.character(source %||% "single_spoke_default")
    if (endsWith(source, "_feasible_capacity")) {
      return(source)
    }
    paste0(source, "_feasible_capacity")
  }
  compact_budget_entry <- function(entry, spoke_id) {
    entry <- entry %||% list()
    base_budget <- as.integer(entry$B_spoke_refit_budget %||% 0L)
    if (!is.finite(base_budget) || base_budget < 1L) {
      return(entry)
    }
    quota_controller <- controller
    quota_controller$current_link_spoke_id <- as.integer(spoke_id)
    quota_controller$B_spoke_refit_budget <- as.integer(base_budget)
    quota_controller$B_spoke_refit_budget_source <- as.character(
      entry$B_spoke_refit_budget_source %||% "single_spoke_default"
    )
    stage_quotas <- .adaptive_round_compute_quotas(
      round_id = as.integer((state$round %||% list())$round_id %||% 1L),
      n_items = as.integer(state$n_items),
      controller = quota_controller
    )
    stage_quotas <- .adaptive_link_adjust_stage_quotas_for_feasibility(
      state = state,
      controller = controller,
      spoke_id = as.integer(spoke_id),
      stage_quotas = stage_quotas,
      stage_order = .adaptive_stage_order(),
      refit_id = refit_id
    )
    compacted_budget <- as.integer(sum(as.integer(stage_quotas), na.rm = TRUE))
    if (!is.finite(compacted_budget) || compacted_budget < 0L) {
      compacted_budget <- 0L
    }
    if (compacted_budget < base_budget) {
      entry$B_spoke_refit_budget <- as.integer(compacted_budget)
      entry$B_spoke_refit_budget_source <- compact_budget_source(
        entry$B_spoke_refit_budget_source %||% "single_spoke_default"
      )
      if (!is.null(entry$concurrent_target_pairs)) {
        entry$concurrent_target_pairs <- as.integer(
          min(as.integer(entry$concurrent_target_pairs %||% compacted_budget), compacted_budget)
        )
      }
      if (!is.null(entry$concurrent_floor_pairs)) {
        entry$concurrent_floor_pairs <- as.integer(
          min(as.integer(entry$concurrent_floor_pairs %||% compacted_budget), compacted_budget)
        )
      }
    }
    entry
  }
  zero_budget_entry <- function(source = "independent_inactive_spoke") {
    list(
      B_spoke_refit_budget = 0L,
      B_spoke_refit_budget_source = as.character(source),
      concurrent_target_pairs = NA_integer_,
      concurrent_floor_pairs = NA_integer_,
      concurrent_floor_met = NA,
      concurrent_target_met = NA,
      concurrent_utility_mass = NA_real_,
      concurrent_top_k_used = NA_integer_,
      concurrent_candidate_count = NA_integer_
    )
  }
  controller <- controller %||% .adaptive_controller_resolve(state)
  refit_id <- as.integer(.adaptive_link_refit_window_id(state))
  refit_context <- list(last_refit_step = as.integer(state$refit_meta$last_refit_step %||% 0L))
  current_window_cross_total <- function(spoke_id) {
    summary <- .adaptive_link_refit_summary_current(
      state = state,
      refit_id = refit_id,
      spoke_id = as.integer(spoke_id),
      refit_context = refit_context
    )
    as.integer(summary$n_cross_edges_total_since_last_refit %||% 0L)
  }
  cached_refit_id <- as.integer(controller$link_budget_refit_id %||% NA_integer_)
  cached_map <- controller$link_budget_map %||% list()
  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  spoke_ids <- as.integer(eligible_spoke_ids %||% phase_ctx$active_spokes %||% integer())
  spoke_ids <- sort(unique(spoke_ids[!is.na(spoke_ids)]))
  if (length(spoke_ids) < 1L) {
    return(list())
  }
  run_mode <- as.character(controller$run_mode %||% "within_set")
  concurrent_mode <- identical(run_mode, "link_multi_spoke") &&
    identical(as.character(controller$multi_spoke_mode %||% "independent"), "concurrent")
  if (!is.na(cached_refit_id) &&
    identical(cached_refit_id, refit_id) &&
    length(cached_map) > 0L) {
    if (!isTRUE(concurrent_mode)) {
      cached_active <- names(cached_map)[vapply(
        cached_map,
        function(entry) as.integer(entry$B_spoke_refit_budget %||% 0L) > 0L,
        logical(1L)
      )]
      out <- lapply(as.character(spoke_ids), function(key) {
        if (key %in% names(cached_map)) {
          cached_map[[key]]
        } else if (length(cached_active) > 0L) {
          zero_budget_entry()
        } else {
          NULL
        }
      })
      names(out) <- as.character(spoke_ids)
      out <- out[!vapply(out, is.null, logical(1L))]
      return(out)
    }
    if (all(as.character(spoke_ids) %in% names(cached_map))) {
      cached_map <- lapply(as.character(spoke_ids), function(key) {
        entry <- cached_map[[key]] %||% list()
        obs <- current_window_cross_total(as.integer(key))
        target_pairs <- as.integer(entry$concurrent_target_pairs %||% entry$B_spoke_refit_budget %||% 0L)
        floor_pairs <- as.integer(entry$concurrent_floor_pairs %||% 0L)
        entry$concurrent_target_met <- as.logical(obs >= target_pairs)
        entry$concurrent_floor_met <- as.logical(obs >= floor_pairs)
        entry
      })
      names(cached_map) <- as.character(spoke_ids)
      return(cached_map[as.character(spoke_ids)])
    }
  }

  single_budget <- .adaptive_link_refit_budget_default(as.integer(state$n_items), controller = controller)
  if (!isTRUE(concurrent_mode)) {
    current_spoke_id <- as.integer(controller$current_link_spoke_id %||% NA_integer_)
    active_spoke_id <- if (!is.na(current_spoke_id) && current_spoke_id %in% spoke_ids) {
      current_spoke_id
    } else if (identical(as.character(controller$run_mode %||% "within_set"), "link_multi_spoke")) {
      .adaptive_link_active_spoke(
        state = state,
        controller = controller,
        eligible_spoke_ids = spoke_ids
      )
    } else {
      as.integer(spoke_ids[[1L]])
    }
    if (is.na(active_spoke_id) || !active_spoke_id %in% spoke_ids) {
      return(list())
    }
    out <- lapply(as.character(spoke_ids), function(key) {
      if (!identical(as.integer(key), as.integer(active_spoke_id))) {
        return(zero_budget_entry())
      }
      entry <- list(
        B_spoke_refit_budget = as.integer(single_budget),
        B_spoke_refit_budget_source = "single_spoke_controller",
        concurrent_target_pairs = NA_integer_,
        concurrent_floor_pairs = NA_integer_,
        concurrent_floor_met = NA,
        concurrent_target_met = NA,
        concurrent_utility_mass = NA_real_,
        concurrent_top_k_used = NA_integer_,
        concurrent_candidate_count = NA_integer_
      )
      if (isTRUE(compact_for_feasibility)) {
        compact_budget_entry(entry, spoke_id = as.integer(key))
      } else {
        entry
      }
    })
    names(out) <- as.character(spoke_ids)
    return(out)
  }

  floor_pairs <- as.integer(controller$min_cross_set_pairs_per_spoke_per_refit %||% 5L)
  top_k <- as.integer(controller$multi_spoke_budget_top_k %||% 10L)
  spoke_stats <- lapply(spoke_ids, function(spoke_id) {
    mass <- .adaptive_link_spoke_utility_mass(
      state = state,
      controller = controller,
      spoke_id = as.integer(spoke_id),
      top_k = top_k,
      seed = as.integer(seed + spoke_id)
    )
    list(
      concurrent_utility_mass = as.double(mass$utility_mass),
      concurrent_top_k_used = as.integer(mass$top_k_used),
      concurrent_candidate_count = as.integer(mass$candidate_count)
    )
  })
  names(spoke_stats) <- as.character(spoke_ids)
  targets <- .adaptive_link_concurrent_targets(
    spoke_stats = spoke_stats,
    total_pairs = as.integer(single_budget),
    floor_pairs = as.integer(floor_pairs)
  )
  out <- lapply(as.character(spoke_ids), function(key) {
    stat <- spoke_stats[[key]] %||% list()
    target_pairs <- as.integer(targets[[key]] %||% 0L)
    obs <- current_window_cross_total(as.integer(key))
    entry <- list(
      B_spoke_refit_budget = as.integer(target_pairs),
      B_spoke_refit_budget_source = "concurrent_allocator",
      concurrent_target_pairs = as.integer(target_pairs),
      concurrent_floor_pairs = as.integer(floor_pairs),
      concurrent_floor_met = as.logical(obs >= floor_pairs),
      concurrent_target_met = as.logical(obs >= target_pairs),
      concurrent_utility_mass = as.double(stat$concurrent_utility_mass %||% 0),
      concurrent_top_k_used = as.integer(stat$concurrent_top_k_used %||% 0L),
      concurrent_candidate_count = as.integer(stat$concurrent_candidate_count %||% 0L)
    )
    if (isTRUE(compact_for_feasibility)) {
      entry <- compact_budget_entry(entry, spoke_id = as.integer(key))
    }
    compacted_target <- as.integer(entry$concurrent_target_pairs %||% entry$B_spoke_refit_budget %||% 0L)
    compacted_floor <- as.integer(entry$concurrent_floor_pairs %||% 0L)
    entry$concurrent_target_met <- as.logical(obs >= compacted_target)
    entry$concurrent_floor_met <- as.logical(obs >= compacted_floor)
    entry
  })
  names(out) <- as.character(spoke_ids)
  out
}

#' @keywords internal
#' @noRd




.adaptive_linking_refit_update_state <- function(state, refit_context) {
  .link_guard_adaptive_selection(state)
  state
}

.adaptive_link_stage_refit_rows <- function(state, refit_id, refit_context) {
  .link_guard_adaptive_selection(state)
  new_link_stage_log()
}

#' @keywords internal
#' @noRd
.adaptive_assert_link_stage_rows_completeness <- function(link_rows) {
  rows <- .adaptive_link_stage_backfill_audit_columns(link_rows)
  if (nrow(rows) < 1L) {
    return(invisible(TRUE))
  }
  required <- c(
    "refit_id", "spoke_id", "hub_id", "link_epoch_id", "link_estimation_mode",
    "link_transform_policy", "link_transform_state", "link_refit_mode",
    "hub_lock_mode", "reliability_link_global", "linking_identified", "link_stop_eligible", "link_stop_pass",
    "link_state_frozen",
    "stop_recent_pass_count", "stop_recent_window_size",
    "stability_window_refits_used", "stability_passes_required_used",
    "escalation_recent_pass_count", "escalation_recent_window_size",
    "link_transform_escalation_window_refits_used",
    "link_transform_escalation_passes_required_used",
    "link_diagnostics_pass", "link_diagnostics_converged_pass",
    "link_diagnostics_finite_summary_pass", "link_diagnostics_uncertainty_pass",
    "n_pairs_cross_set_done", "n_unique_cross_pairs_seen", "n_cross_edges_active_since_last_refit",
    "n_cross_edges_probe_since_last_refit", "n_cross_edges_total_since_last_refit", "coverage_bins_used",
    "B_spoke_refit_budget", "B_spoke_refit_budget_source",
    "stage_target_anchor_link", "stage_target_long_link", "stage_target_mid_link", "stage_target_local_link",
    "feasible_stage_capacity_anchor_link", "feasible_stage_capacity_long_link",
    "feasible_stage_capacity_mid_link", "feasible_stage_capacity_local_link",
    "feasibility_budget_released", "feasibility_reallocation_used", "feasibility_reallocation_rule",
    "stage_realized_anchor_link", "stage_realized_long_link", "stage_realized_mid_link", "stage_realized_local_link",
    "stage_shortfall_anchor_link", "stage_shortfall_long_link", "stage_shortfall_mid_link",
    "stage_shortfall_local_link", "stage_reallocation_used", "stage_reallocation_rule_used",
    "stage_budget_unfilled",
    "probe_edges_realized_before_refit", "probe_edges_realized_delta_since_last_refit",
    "probe_shortfall_reason",
    "probe_brier", "probe_brier_max_used", "probe_brier_pass",
    "probe_near_boundary_frac", "probe_near_boundary_min_frac_used", "probe_near_boundary_pass",
    "probe_extreme_frac", "probe_extreme_max_frac_used", "probe_extreme_frac_pass",
    "probe_midrange_frac", "probe_midrange_min_frac_used", "probe_midrange_pass",
    "probe_unique_hub_items", "probe_unique_hub_min_used", "probe_unique_hub_pass",
    "probe_unique_spoke_items", "probe_unique_spoke_min_used", "probe_unique_spoke_pass",
    "probe_rank_bins_hub_covered", "probe_rank_bins_hub_min_used", "probe_rank_bins_hub_pass",
    "probe_rank_bins_spoke_covered", "probe_rank_bins_spoke_min_used", "probe_rank_bins_spoke_pass",
    "probe_brier_near_boundary", "probe_brier_near_boundary_max_used",
    "probe_brier_near_boundary_pass",
    "probe_ece", "probe_ece_max_used", "probe_ece_pass",
    "probe_quality_pass", "probe_quality_blocker_codes",
    "probe_pred_rmse_lagged", "probe_pred_rmse_max_used", "probe_pred_rmse_pass",
    "phase_a_within_edges_hub_used", "phase_a_within_edges_spoke_used",
    "phase_b_active_edges_used",
    "theta_global_rmse_lagged", "theta_global_rmse_max_used", "theta_global_rmse_pass",
    "resumed_from_session"
  )
  missing <- setdiff(required, names(rows))
  if (length(missing) > 0L) {
    rlang::abort(paste0(
      "link_stage_log append completeness failure: missing required columns: ",
      paste(missing, collapse = ", "),
      "."
    ))
  }

  key_na <- rows[is.na(rows$refit_id) | is.na(rows$spoke_id) | is.na(rows$hub_id), , drop = FALSE]
  if (nrow(key_na) > 0L) {
    rlang::abort("link_stage_log append completeness failure: key fields refit_id/spoke_id/hub_id must be non-NA.")
  }
  mode <- as.character(rows$link_estimation_mode %||% rep_len("transform", nrow(rows)))
  transform_idx <- is.na(mode) | mode == "transform"
  mode_na <- rows[
    is.na(rows$hub_lock_mode) |
      (transform_idx & (
        is.na(rows$link_transform_policy) |
          is.na(rows$link_transform_state) |
          is.na(rows$link_refit_mode)
      )),
    ,
    drop = FALSE
  ]
  if (nrow(mode_na) > 0L) {
    rlang::abort(
      "link_stage_log append completeness failure: mode fields must be populated for linking rows."
    )
  }
  if (any(is.na(rows$linking_identified)) || any(is.na(rows$link_stop_eligible)) || any(is.na(rows$link_stop_pass))) {
    rlang::abort(
      paste0(
        "link_stage_log append completeness failure: ",
        "linking_identified/link_stop_eligible/link_stop_pass must be populated."
      )
    )
  }
  if (any(is.na(rows$link_state_frozen))) {
    rlang::abort("link_stage_log append completeness failure: `link_state_frozen` must be populated.")
  }
  .adaptive_assert_link_stage_budget_invariants(rows)

  invisible(TRUE)
}

#' @keywords internal
#' @noRd
.adaptive_link_reconstruct_taper_from_logs <- function(link_row) {
  row <- tibble::as_tibble(link_row)
  if (nrow(row) < 1L) {
    return(NA)
  }
  if ("quota_taper_applied" %in% names(row) &&
    !is.na(row$quota_taper_applied[[1L]])) {
    return(as.logical(row$quota_taper_applied[[1L]]))
  }
  raw <- as.integer(row$quota_long_link_raw[[1L]] %||% NA_integer_)
  eff <- as.integer(row$quota_long_link_effective[[1L]] %||% NA_integer_)
  if (is.na(raw) || is.na(eff)) {
    return(NA)
  }
  as.logical(eff < raw)
}

.adaptive_round_log_deferred_audit_columns <- function() {
  c(
    "ci95_theta_width_mean",
    "ci95_theta_width_median",
    "ci95_theta_width_p90",
    "ci95_theta_width_max",
    "near_tie_adj_frac",
    "near_tie_adj_count",
    "p_adj_median",
    "cov_trace_theta",
    "cov_logdet_diag_theta",
    "post_sd_theta_p10",
    "post_sd_theta_p50",
    "post_sd_theta_p90",
    "top20_boundary_entropy_mean",
    "top20_boundary_entropy_p90",
    "nn_diff_sd_mean",
    "nn_diff_sd_p90"
  )
}

.adaptive_round_log_deferred_audit_na_values <- function() {
  list(
    ci95_theta_width_mean = NA_real_,
    ci95_theta_width_median = NA_real_,
    ci95_theta_width_p90 = NA_real_,
    ci95_theta_width_max = NA_real_,
    near_tie_adj_frac = NA_real_,
    near_tie_adj_count = NA_integer_,
    p_adj_median = NA_real_,
    cov_trace_theta = NA_real_,
    cov_logdet_diag_theta = NA_real_,
    post_sd_theta_p10 = NA_real_,
    post_sd_theta_p50 = NA_real_,
    post_sd_theta_p90 = NA_real_,
    top20_boundary_entropy_mean = NA_real_,
    top20_boundary_entropy_p90 = NA_real_,
    nn_diff_sd_mean = NA_real_,
    nn_diff_sd_p90 = NA_real_
  )
}

.adaptive_round_log_deferred_audit_payload <- function(draws,
                                                       near_tie_p_low,
                                                       near_tie_p_high,
                                                       max_draws = 400L) {
  if (!is.matrix(draws) || !is.numeric(draws) || nrow(draws) < 2L || ncol(draws) < 1L) {
    return(NULL)
  }
  n_draws_total <- nrow(draws)
  draw_idx <- .adaptive_deferred_audit_draw_index(nrow(draws), max_draws = max_draws)
  draws <- draws[draw_idx, , drop = FALSE]
  max_draws_logged <- if (is.null(max_draws) || is.infinite(max_draws)) {
    NA_integer_
  } else {
    as.integer(max_draws)
  }
  list(
    summary = .adaptive_round_log_deferred_audit_from_draws(
      draws = draws,
      near_tie_p_low = near_tie_p_low,
      near_tie_p_high = near_tie_p_high
    ),
    near_tie_p_low = as.double(near_tie_p_low),
    near_tie_p_high = as.double(near_tie_p_high),
    max_draws = as.integer(max_draws_logged),
    n_draws_total = as.integer(n_draws_total),
    n_draws_used = as.integer(nrow(draws))
  )
}

.adaptive_deferred_audit_draw_index <- function(n_draws, max_draws = 400L) {
  n_draws <- as.integer(n_draws %||% NA_integer_)
  if (!is.finite(n_draws) || is.na(n_draws) || n_draws < 2L) {
    rlang::abort("Deferred audit draw indexing requires at least two draws.")
  }
  if (is.null(max_draws) || is.infinite(max_draws)) {
    return(seq_len(n_draws))
  }
  if (!is.numeric(max_draws) || length(max_draws) != 1L || is.na(max_draws)) {
    rlang::abort("`deferred_audit_max_draws` must be a positive integer or Inf.")
  }
  max_draws <- as.integer(max_draws)
  if (!is.finite(max_draws) || max_draws < 2L) {
    rlang::abort("`deferred_audit_max_draws` must be >= 2 or Inf.")
  }
  if (n_draws <= max_draws) {
    return(seq_len(n_draws))
  }
  unique(as.integer(round(seq(1L, n_draws, length.out = max_draws))))
}

.adaptive_round_log_deferred_audit_from_draws <- function(draws,
                                                          near_tie_p_low,
                                                          near_tie_p_high) {
  out <- .adaptive_round_log_deferred_audit_na_values()
  if (!is.matrix(draws) || !is.numeric(draws) || nrow(draws) < 2L || ncol(draws) < 1L) {
    return(out)
  }
  draws <- .pairwiseLLM_sanitize_draws_matrix(draws, name = "round_log_deferred_audit_draws")

  ci_bounds <- .pairwiseLLM_col_quantiles(
    draws,
    probs = c(0.025, 0.975),
    names = FALSE
  )
  ci_widths <- ci_bounds[2L, ] - ci_bounds[1L, ]
  out$ci95_theta_width_mean <- mean(ci_widths)
  out$ci95_theta_width_median <- stats::median(ci_widths)
  out$ci95_theta_width_p90 <- stats::quantile(ci_widths, probs = 0.90, names = FALSE)
  out$ci95_theta_width_max <- max(ci_widths)

  theta_for_draws <- as.double(colMeans(draws))
  cov_diag <- .pairwiseLLM_col_sds(draws, center = theta_for_draws)^2
  out$cov_trace_theta <- sum(cov_diag)
  out$cov_logdet_diag_theta <- sum(log(pmax(cov_diag, .Machine$double.eps)))
  post_sd <- sqrt(pmax(cov_diag, 0))
  out$post_sd_theta_p10 <- stats::quantile(post_sd, probs = 0.10, names = FALSE)
  out$post_sd_theta_p50 <- stats::quantile(post_sd, probs = 0.50, names = FALSE)
  out$post_sd_theta_p90 <- stats::quantile(post_sd, probs = 0.90, names = FALSE)

  rank_draws <- t(apply(draws, 1, function(row) rank(-row, ties.method = "average")))
  top_k <- min(20L, ncol(rank_draws))
  if (top_k >= 1L) {
    in_top <- rank_draws <= top_k
    p_top <- colMeans(in_top)
    entropy <- -(p_top * log(pmax(p_top, .Machine$double.eps)) +
      (1 - p_top) * log(pmax(1 - p_top, .Machine$double.eps)))
    boundary_lo <- max(1L, top_k - 2L)
    boundary_hi <- min(length(entropy), top_k + 2L)
    boundary_idx <- boundary_lo:boundary_hi
    out$top20_boundary_entropy_mean <- mean(entropy[boundary_idx])
    out$top20_boundary_entropy_p90 <- stats::quantile(
      entropy[boundary_idx],
      probs = 0.90,
      names = FALSE
    )
  }

  draw_ids <- as.character(colnames(draws) %||% seq_len(ncol(draws)))
  if (length(theta_for_draws) >= 2L) {
    rank_order <- order(-theta_for_draws, draw_ids)
    lhs <- rank_order[-length(rank_order)]
    rhs <- rank_order[-1L]
    p_adj <- as.double(colMeans(draws[, lhs, drop = FALSE] > draws[, rhs, drop = FALSE]))
    near_low <- as.double(near_tie_p_low %||% 0.40)
    near_high <- as.double(near_tie_p_high %||% 0.60)
    near_tie <- p_adj >= near_low & p_adj <= near_high
    out$near_tie_adj_frac <- mean(near_tie)
    out$near_tie_adj_count <- as.integer(sum(near_tie))
    out$p_adj_median <- stats::median(p_adj)

    nn_diff_draws <- draws[, rank_order[-length(rank_order)], drop = FALSE] -
      draws[, rank_order[-1L], drop = FALSE]
    nn_diff_sd <- .pairwiseLLM_col_sds(nn_diff_draws)
    out$nn_diff_sd_mean <- mean(nn_diff_sd)
    out$nn_diff_sd_p90 <- stats::quantile(nn_diff_sd, probs = 0.90, names = FALSE)
  }

  out
}

.adaptive_round_log_deferred_audit_from_payload <- function(payload) {
  out <- .adaptive_round_log_deferred_audit_na_values()
  if (!is.list(payload)) {
    return(out)
  }

  summary <- payload$summary %||% NULL
  if (is.list(summary)) {
    for (nm in intersect(names(out), names(summary))) {
      out[[nm]] <- summary[[nm]]
    }
    return(out)
  }

  .adaptive_round_log_deferred_audit_from_draws(
    draws = payload$draws %||% NULL,
    near_tie_p_low = payload$near_tie_p_low %||% 0.40,
    near_tie_p_high = payload$near_tie_p_high %||% 0.60
  )
}

.adaptive_btl_refit_context <- function(state, last_refit_M_done, last_refit_step) {
  step_id_at_refit <- as.integer(nrow(state$step_log))
  list(
    step_id_at_refit = step_id_at_refit,
    timestamp = (state$meta$now_fn %||% function() Sys.time())(),
    last_refit_M_done = as.integer(last_refit_M_done),
    last_refit_step = as.integer(last_refit_step)
  )
}

.adaptive_round_log_row <- function(state, metrics, stop_decision, stop_reason, refit_context, config) {
  ids <- as.character(state$item_ids)
  scope <- .adaptive_stop_metric_scope(state, ids = ids)
  scope_ids <- as.character(scope$scope_ids %||% ids)
  metric_ids <- as.character(scope_ids)
  if (length(metric_ids) < 1L) {
    metric_ids <- ids
  }
  history_state <- .adaptive_history_state_resolve(state, ids = ids)
  counts <- .adaptive_history_state_counts(history_state, ids)

  deg_vals <- as.double(counts$deg[ids])
  mean_degree <- if (length(deg_vals) > 0L) mean(deg_vals) else NA_real_
  min_degree <- if (length(deg_vals) > 0L) min(deg_vals) else NA_integer_
  deg_vals_scope <- as.double(counts$deg[scope_ids])
  mean_degree_scope <- if (length(deg_vals_scope) > 0L) mean(deg_vals_scope) else NA_real_
  min_degree_scope <- if (length(deg_vals_scope) > 0L) min(deg_vals_scope) else NA_integer_
  pos_balance <- as.double(counts$posA[ids] - counts$posB[ids])
  pos_balance_sd <- if (length(pos_balance) > 1L) stats::sd(pos_balance) else 0

  step_log <- tibble::as_tibble(state$step_log)
  committed_all <- step_log[!is.na(step_log$pair_id), , drop = FALSE]
  n_unique_pairs_seen <- sum(counts$pair_count >= 1L)
  total_pairs_done <- nrow(committed_all)

  last_step <- refit_context$last_refit_step
  step_id_at_refit <- refit_context$step_id_at_refit
  step_subset <- step_log[step_log$step_id > last_step &
    step_log$step_id <= step_id_at_refit, , drop = FALSE]
  controller <- .adaptive_controller_resolve(state)
  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  phase_b_linking <- .adaptive_link_mode_active(controller) && identical(phase_ctx$phase, "phase_b")
  round_stop_decision <- if (isTRUE(phase_b_linking)) {
    FALSE
  } else {
    as.logical(stop_decision)
  }
  round_stop_reason <- if (isTRUE(phase_b_linking)) {
    NA_character_
  } else if (isTRUE(stop_decision)) {
    as.character(stop_reason)
  } else {
    NA_character_
  }
  if (!"pair_id" %in% names(step_subset)) {
    step_subset$pair_id <- NA_integer_
  }
  if (!"is_cross_set" %in% names(step_subset)) {
    step_subset$is_cross_set <- FALSE
  }
  if (!"run_mode" %in% names(step_subset)) {
    step_subset$run_mode <- NA_character_
  }
  if (!"is_probe_step" %in% names(step_subset)) {
    step_subset$is_probe_step <- FALSE
  }
  committed_subset <- step_subset[!is.na(step_subset$pair_id), , drop = FALSE]
  new_pairs_since_last_refit <- as.integer(nrow(committed_subset))
  refit_id <- as.integer(nrow(state$round_log %||% tibble::tibble()) + 1L)
  summary_cache <- .adaptive_link_refit_summary_cache(state)
  cache_spokes <- vapply(
    summary_cache,
    function(entry) {
      entry_refit_id <- as.integer(entry$refit_id %||% NA_integer_)
      if (!identical(entry_refit_id, refit_id)) {
        return(NA_integer_)
      }
      as.integer(entry$spoke_id %||% NA_integer_)
    },
    integer(1L)
  )
  summary_spokes <- sort(unique(c(
    as.integer(phase_ctx$active_spokes %||% integer()),
    cache_spokes[is.finite(cache_spokes) & !is.na(cache_spokes)]
  )))
  summary_spokes <- summary_spokes[is.finite(summary_spokes) & !is.na(summary_spokes)]
  refit_summaries <- lapply(summary_spokes, function(spoke_id) {
    .adaptive_link_refit_summary_current(
      state = state,
      refit_id = refit_id,
      spoke_id = as.integer(spoke_id),
      refit_context = refit_context
    )
  })
  new_active_pairs_since_last_refit <- if (isTRUE(phase_b_linking)) {
    as.integer(sum(vapply(
      refit_summaries,
      function(summary) as.integer(summary$n_cross_edges_active_since_last_refit %||% 0L),
      integer(1L)
    )))
  } else {
    NA_integer_
  }
  new_probe_pairs_since_last_refit <- if (isTRUE(phase_b_linking)) {
    as.integer(sum(vapply(
      refit_summaries,
      function(summary) as.integer(summary$n_cross_edges_probe_since_last_refit %||% 0L),
      integer(1L)
    )))
  } else {
    NA_integer_
  }
  new_total_cross_pairs_since_last_refit <- if (isTRUE(phase_b_linking)) {
    as.integer(sum(vapply(
      refit_summaries,
      function(summary) as.integer(summary$n_cross_edges_total_since_last_refit %||% 0L),
      integer(1L)
    )))
  } else {
    NA_integer_
  }

  proposed_pairs <- step_subset$n_candidates_scored
  proposed_pairs_mode <- if (length(proposed_pairs) > 0L && any(!is.na(proposed_pairs))) {
    stats::median(proposed_pairs, na.rm = TRUE)
  } else {
    NA_real_
  }

  starve_rate <- if (nrow(step_subset) > 0L) {
    mean(step_subset$candidate_starved %in% TRUE, na.rm = TRUE)
  } else {
    NA_real_
  }
  fallback_rate <- if (nrow(step_subset) > 0L) {
    mean(!step_subset$fallback_used %in% c("base", "warm_start"), na.rm = TRUE)
  } else {
    NA_real_
  }

  fallback_used_mode <- .adaptive_mode_value(step_subset$fallback_used)
  starved_rows <- step_subset[step_subset$candidate_starved %in% TRUE, , drop = FALSE]
  starvation_reason_mode <- .adaptive_mode_value(starved_rows$starvation_reason)
  total_after_dup <- sum(step_subset$n_candidates_after_duplicates, na.rm = TRUE)
  total_star_cap_rejects <- sum(step_subset$star_cap_rejects, na.rm = TRUE)
  star_cap_reject_rate <- if (is.finite(total_after_dup) && total_after_dup > 0) {
    total_star_cap_rejects / total_after_dup
  } else {
    NA_real_
  }

  fit <- state$btl_fit %||% list()
  model_variant <- fit$model_variant %||% NA_character_

  ts_sigma_mean <- NA_real_
  ts_sigma_max <- NA_real_
  ts_degree_sigma_corr <- NA_real_
  ts_btl_theta_corr <- NA_real_
  ts_btl_rank_spearman <- NA_real_
  deferred_audit <- .adaptive_round_log_deferred_audit_na_values()

  trueskill_state <- state$trueskill_state %||% NULL
  defaults <- adaptive_defaults(length(ids))
  recent_deg_summary <- .adaptive_history_state_recent_deg(history_state, ids, defaults$W_cap)
  recent_deg_vals <- as.double(recent_deg_summary[ids])
  recent_deg_median <- if (length(recent_deg_vals) > 0L) {
    stats::median(recent_deg_vals)
  } else {
    NA_real_
  }
  recent_deg_max <- if (length(recent_deg_vals) > 0L) {
    as.integer(max(recent_deg_vals))
  } else {
    NA_integer_
  }
  if (!is.null(trueskill_state) && is.data.frame(trueskill_state$items)) {
    ts_items <- trueskill_state$items
    ts_ids <- as.character(ts_items$item_id)
    idx <- match(ids, ts_ids)
    ts_sigma <- ts_items$sigma[idx]
    ts_mu <- ts_items$mu[idx]
    if (length(ts_sigma) > 0L && all(is.finite(ts_sigma))) {
      ts_sigma_mean <- mean(ts_sigma)
      ts_sigma_max <- max(ts_sigma)
      if (length(ts_sigma) > 1L) {
        sigma_sd <- stats::sd(ts_sigma)
        deg_sd <- stats::sd(deg_vals)
        if (is.finite(sigma_sd) && is.finite(deg_sd) && sigma_sd > 0 && deg_sd > 0) {
          ts_degree_sigma_corr <- stats::cor(ts_sigma, deg_vals, use = "pairwise.complete.obs")
        }
      }
    }
  }

  theta_mean <- .adaptive_btl_fit_theta_mean(fit)
  theta_map <- NULL
  if (is.numeric(theta_mean) && length(theta_mean) > 0L) {
    if (!is.null(names(theta_mean))) {
      theta_map <- stats::setNames(as.double(theta_mean), as.character(names(theta_mean)))
    } else if (length(theta_mean) == length(ids)) {
      theta_map <- stats::setNames(as.double(theta_mean), ids)
    }
  }

  if (!is.null(theta_map) && !is.null(trueskill_state) && is.data.frame(trueskill_state$items)) {
    theta_ids <- intersect(metric_ids, names(theta_map))
    if (length(theta_ids) >= 2L) {
      ts_ids <- as.character(trueskill_state$items$item_id)
      ts_mu <- as.double(trueskill_state$items$mu[match(theta_ids, ts_ids)])
      theta_vals <- as.double(theta_map[theta_ids])
      if (all(is.finite(ts_mu)) && all(is.finite(theta_vals))) {
        ts_btl_theta_corr <- .adaptive_safe_cor(ts_mu, theta_vals)
        rank_theta <- rank(theta_vals, ties.method = "average")
        rank_mu <- rank(ts_mu, ties.method = "average")
        ts_btl_rank_spearman <- .adaptive_safe_cor(rank_mu, rank_theta, method = "spearman")
      }
    }
  }

  mcmc_config_used <- fit$mcmc_config_used %||% list()

  round_id_current <- as.integer(state$round$round_id %||% NA_integer_)
  round_committed <- as.integer(state$round$round_committed %||% NA_integer_)
  round_id_at_refit <- if (!is.na(round_id_current) &&
    !is.na(round_committed) &&
    round_committed == 0L &&
    total_pairs_done > 0L) {
    as.integer(max(1L, round_id_current - 1L))
  } else {
    as.integer(round_id_current)
  }
  max_pairs_after_stop <- as.integer(controller$max_pairs_after_stop %||% 0L)
  if (!is.finite(max_pairs_after_stop) || is.na(max_pairs_after_stop) || max_pairs_after_stop < 0L) {
    max_pairs_after_stop <- 0L
  }
  pairs_committed_after_stop <- as.integer(state$meta$pairs_committed_after_stop %||% 0L)
  if (!is.finite(pairs_committed_after_stop) || is.na(pairs_committed_after_stop) || pairs_committed_after_stop < 0L) {
    pairs_committed_after_stop <- 0L
  }
  round_summary <- state$refit_meta$last_completed_round_summary %||% list()
  if (!is.na(round_id_at_refit) && !is.na(round_summary$round_id %||% NA_integer_) &&
    as.integer(round_summary$round_id) == round_id_at_refit) {
    quota_source <- round_summary
  } else {
    quota_source <- state$round %||% list()
  }

  row <- list(
    refit_id = as.integer(nrow(state$round_log) + 1L),
    round_id_at_refit = round_id_at_refit,
    step_id_at_refit = as.integer(step_id_at_refit),
    timestamp = refit_context$timestamp,
    predictive_prior_digest = state$meta$predictive_prior_digest %||% NA_character_,
    model_variant = as.character(model_variant),
    n_items = as.integer(state$n_items),
    total_pairs_done = as.integer(total_pairs_done),
    new_pairs_since_last_refit = as.integer(new_pairs_since_last_refit),
    new_active_pairs_since_last_refit = as.integer(new_active_pairs_since_last_refit),
    new_probe_pairs_since_last_refit = as.integer(new_probe_pairs_since_last_refit),
    new_total_cross_pairs_since_last_refit = as.integer(new_total_cross_pairs_since_last_refit),
    n_unique_pairs_seen = as.integer(n_unique_pairs_seen),
    proposed_pairs_mode = as.double(proposed_pairs_mode),
    starve_rate_since_last_refit = as.double(starve_rate),
    fallback_rate_since_last_refit = as.double(fallback_rate),
    fallback_used_mode = as.character(fallback_used_mode),
    starvation_reason_mode = as.character(starvation_reason_mode),
    global_identified = as.logical(controller$global_identified %||% FALSE),
    global_identified_reliability_min = as.double(controller$global_identified_reliability_min %||% NA_real_),
    global_identified_rank_corr_min = as.double(controller$global_identified_rank_corr_min %||% NA_real_),
    long_quota_raw = as.integer(quota_source$long_quota_raw %||% NA_integer_),
    long_quota_effective = as.integer(quota_source$long_quota_effective %||% NA_integer_),
    long_quota_removed = as.integer(quota_source$long_quota_removed %||% NA_integer_),
    realloc_to_mid = as.integer(quota_source$realloc_to_mid %||% NA_integer_),
    realloc_to_local = as.integer(quota_source$realloc_to_local %||% NA_integer_),
    phase_scope = as.character(metrics$phase_scope %||% scope$phase_scope %||% "global"),
    phase_scope_set_id = as.integer(metrics$phase_scope_set_id %||% scope$phase_scope_set_id %||% NA_integer_),
    phase_scope_n_items = as.integer(metrics$phase_scope_n_items %||% length(scope_ids)),
    mean_degree = as.double(mean_degree),
    min_degree = as.integer(min_degree),
    mean_degree_scope = as.double(mean_degree_scope),
    min_degree_scope = as.integer(min_degree_scope),
    pos_balance_sd = as.double(pos_balance_sd),
    epsilon_mean = as.double(fit$epsilon_mean %||% NA_real_),
    epsilon_p2.5 = as.double(fit$epsilon_p2.5 %||% NA_real_),
    epsilon_p5 = as.double(fit$epsilon_p5 %||% NA_real_),
    epsilon_p50 = as.double(fit$epsilon_p50 %||% NA_real_),
    epsilon_p95 = as.double(fit$epsilon_p95 %||% NA_real_),
    epsilon_p97.5 = as.double(fit$epsilon_p97.5 %||% NA_real_),
    b_mean = as.double(fit$beta_mean %||% NA_real_),
    b_p2.5 = as.double(fit$beta_p2.5 %||% NA_real_),
    b_p5 = as.double(fit$beta_p5 %||% NA_real_),
    b_p50 = as.double(fit$beta_p50 %||% NA_real_),
    b_p95 = as.double(fit$beta_p95 %||% NA_real_),
    b_p97.5 = as.double(fit$beta_p97.5 %||% NA_real_),
    ts_sigma_mean = as.double(ts_sigma_mean),
    ts_sigma_max = as.double(ts_sigma_max),
    ts_degree_sigma_corr = as.double(ts_degree_sigma_corr),
    ts_btl_theta_corr = as.double(ts_btl_theta_corr),
    ts_btl_rank_spearman = as.double(ts_btl_rank_spearman),
    star_cap_rejects_since_last_refit = as.integer(total_star_cap_rejects),
    star_cap_reject_rate_since_last_refit = as.double(star_cap_reject_rate),
    recent_deg_median_since_last_refit = as.double(recent_deg_median),
    recent_deg_max_since_last_refit = as.integer(recent_deg_max),
    ci95_theta_width_mean = as.double(deferred_audit$ci95_theta_width_mean),
    ci95_theta_width_median = as.double(deferred_audit$ci95_theta_width_median),
    ci95_theta_width_p90 = as.double(deferred_audit$ci95_theta_width_p90),
    ci95_theta_width_max = as.double(deferred_audit$ci95_theta_width_max),
    near_tie_adj_frac = as.double(deferred_audit$near_tie_adj_frac),
    near_tie_adj_count = as.integer(deferred_audit$near_tie_adj_count),
    p_adj_median = as.double(deferred_audit$p_adj_median),
    cov_trace_theta = as.double(deferred_audit$cov_trace_theta),
    cov_logdet_diag_theta = as.double(deferred_audit$cov_logdet_diag_theta),
    post_sd_theta_p10 = as.double(deferred_audit$post_sd_theta_p10),
    post_sd_theta_p50 = as.double(deferred_audit$post_sd_theta_p50),
    post_sd_theta_p90 = as.double(deferred_audit$post_sd_theta_p90),
    top20_boundary_entropy_mean = as.double(deferred_audit$top20_boundary_entropy_mean),
    top20_boundary_entropy_p90 = as.double(deferred_audit$top20_boundary_entropy_p90),
    nn_diff_sd_mean = as.double(deferred_audit$nn_diff_sd_mean),
    nn_diff_sd_p90 = as.double(deferred_audit$nn_diff_sd_p90),
    diagnostics_pass = as.logical(metrics$diagnostics_pass %||% NA),
    diagnostics_divergences_pass = as.logical(metrics$diagnostics_divergences_pass %||% NA),
    diagnostics_rhat_pass = as.logical(metrics$diagnostics_rhat_pass %||% NA),
    diagnostics_ess_pass = as.logical(metrics$diagnostics_ess_pass %||% NA),
    divergences = as.integer(metrics$divergences %||% NA_integer_),
    divergences_max_allowed = as.integer(metrics$divergences_max_allowed %||% NA_integer_),
    max_rhat = as.double(metrics$max_rhat %||% NA_real_),
    max_rhat_allowed = as.double(metrics$max_rhat_allowed %||% NA_real_),
    min_ess_bulk = as.double(metrics$min_ess_bulk %||% NA_real_),
    ess_bulk_required = as.double(metrics$ess_bulk_required %||% NA_real_),
    near_stop_active = as.logical(metrics$near_stop_active %||% NA),
    reliability_EAP = as.double(metrics$reliability_EAP %||% NA_real_),
    reliability_EAP_scope = as.double(metrics$reliability_EAP_scope %||% NA_real_),
    eap_reliability_min = as.double(metrics$eap_reliability_min %||% NA_real_),
    eap_pass = as.logical(metrics$eap_pass %||% NA),
    eap_pass_scope = as.logical(metrics$eap_pass_scope %||% NA),
    theta_sd_eap = as.double(metrics$theta_sd_eap %||% NA_real_),
    theta_sd_eap_scope = as.double(metrics$theta_sd_eap_scope %||% NA_real_),
    rho_theta = as.double(metrics$rho_theta %||% NA_real_),
    rho_theta_scope = as.double(metrics$rho_theta_scope %||% NA_real_),
    lag_eligible = as.logical(metrics$lag_eligible %||% NA),
    lag_eligible_scope = as.logical(metrics$lag_eligible_scope %||% NA),
    theta_corr_min = as.double(metrics$theta_corr_min %||% NA_real_),
    theta_corr_pass = as.logical(metrics$theta_corr_pass %||% NA),
    theta_corr_pass_scope = as.logical(metrics$theta_corr_pass_scope %||% NA),
    delta_sd_theta = as.double(metrics$delta_sd_theta %||% NA_real_),
    delta_sd_theta_scope = as.double(metrics$delta_sd_theta_scope %||% NA_real_),
    theta_sd_rel_change_max = as.double(metrics$theta_sd_rel_change_max %||% NA_real_),
    delta_sd_theta_pass = as.logical(metrics$delta_sd_theta_pass %||% NA),
    delta_sd_theta_pass_scope = as.logical(metrics$delta_sd_theta_pass_scope %||% NA),
    rho_rank = as.double(metrics$rho_rank %||% NA_real_),
    rho_rank_scope = as.double(metrics$rho_rank_scope %||% NA_real_),
    rank_spearman_min = as.double(metrics$rank_spearman_min %||% NA_real_),
    rho_rank_pass = as.logical(metrics$rho_rank_pass %||% NA),
    rho_rank_pass_scope = as.logical(metrics$rho_rank_pass_scope %||% NA),
    mcmc_chains = as.integer(mcmc_config_used$chains %||% NA_integer_),
    mcmc_parallel_chains = as.integer(mcmc_config_used$parallel_chains %||% NA_integer_),
    mcmc_core_fraction = as.double(mcmc_config_used$core_fraction %||% NA_real_),
    mcmc_cores_detected_physical = as.integer(mcmc_config_used$cores_detected_physical %||% NA_integer_),
    mcmc_cores_detected_logical = as.integer(mcmc_config_used$cores_detected_logical %||% NA_integer_),
    mcmc_threads_per_chain = as.integer(mcmc_config_used$threads_per_chain %||% NA_integer_),
    mcmc_cores_available = as.integer(mcmc_config_used$cores_available %||% NA_integer_),
    mcmc_parallel_chains_requested = as.integer(mcmc_config_used$parallel_chains_requested %||% NA_integer_),
    mcmc_concurrency_budget = as.integer(mcmc_config_used$concurrency_budget %||% NA_integer_),
    mcmc_concurrency_used = as.integer(mcmc_config_used$concurrency_used %||% NA_integer_),
    mcmc_cmdstanr_version = as.character(mcmc_config_used$cmdstanr_version %||% NA_character_),
    stop_decision = as.logical(round_stop_decision),
    stop_reason = as.character(round_stop_reason),
    max_pairs_after_stop = as.integer(max_pairs_after_stop),
    pairs_committed_after_stop = as.integer(pairs_committed_after_stop)
  )

  row
}

#' @keywords internal
#' @noRd
default_btl_fit_fn <- function(state, config) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }
  .warm_start_adaptive_validate(state)
  config <- .adaptive_btl_resolve_config(state, config)
  scope <- .adaptive_stop_metric_scope(state, ids = state$item_ids)
  ids_fit <- as.character(scope$scope_ids %||% state$item_ids)
  results <- .adaptive_results_from_step_log(state, scope_ids = ids_fit)
  if (nrow(results) < 1L) {
    rlang::abort("BTL refit requires at least one committed comparison.")
  }

  fit_out <- fit_bayes_btl_mcmc(
    results = results,
    ids = ids_fit,
    model_variant = config$model_variant %||% "btl_e_b",
    cmdstan = config[["cmdstan"]] %||% list(),
    warm_start_prior = .warm_start_prior_scope(.warm_start_btl_prior_for_state(state), ids_fit)
  )

  fit_contract <- .adaptive_btl_extract_fit_contract(fit_out)

  .adaptive_btl_adapt_fit(fit_contract)
}

#' @keywords internal
#' @noRd
maybe_refit_btl <- function(state, config, fit_fn = NULL) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }
  config <- .adaptive_btl_resolve_config(state, config)

  scope_counts <- .adaptive_refit_scope_counts(state)
  M_done <- as.integer(scope_counts$M_done)
  last_refit_M_done <- as.integer(scope_counts$last_refit_M_done)
  last_refit_step <- as.integer(scope_counts$last_refit_step)
  scope_set_id <- as.integer(scope_counts$scope_set_id %||% NA_integer_)

  refit_pairs_target <- .adaptive_refit_pairs_target(state, config)
  config$refit_pairs_target <- refit_pairs_target
  eligibility <- .adaptive_refit_eligibility(
    total_committed = M_done,
    last_refit_committed = last_refit_M_done,
    refit_pairs_target = refit_pairs_target
  )
  if (!isTRUE(eligibility$eligible)) {
    controller <- .adaptive_controller_resolve(state)
    phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
    step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
    latest_starved <- if (nrow(step_log) > 0L && "candidate_starved" %in% names(step_log)) {
      isTRUE(step_log$candidate_starved[[nrow(step_log)]])
    } else {
      FALSE
    }
    phase_b_window_exhausted <- if (.adaptive_link_mode_active(controller) &&
      identical(as.character(phase_ctx$phase %||% "phase_a"), "phase_b") &&
      isTRUE(M_done > last_refit_M_done)) {
      isTRUE(.adaptive_link_phase_b_window_exhausted(state, controller = controller))
    } else {
      FALSE
    }
    if (.adaptive_link_mode_active(controller) &&
      identical(as.character(phase_ctx$phase %||% "phase_a"), "phase_b") &&
      isTRUE(latest_starved || phase_b_window_exhausted) &&
      isTRUE(M_done > last_refit_M_done)) {
      eligibility$eligible <- TRUE
    }
  }
  if (!isTRUE(eligibility$eligible)) {
    return(list(
      state = state,
      refit_performed = FALSE,
      config = config
    ))
  }

  fit_fn <- fit_fn %||% default_btl_fit_fn
  if (!is.function(fit_fn)) {
    rlang::abort("`fit_fn` must be a function.")
  }

  fit <- fit_fn(state, config)
  if (!is.list(fit) || is.null(fit$btl_posterior_draws)) {
    rlang::abort("`fit_fn` must return a list with `btl_posterior_draws`.")
  }

  theta_mean <- .adaptive_btl_fit_theta_mean(fit)
  history <- state$refit_meta$theta_mean_history %||% list()
  state$refit_meta$theta_mean_history <- c(history, list(theta_mean))
  if (!is.na(scope_set_id)) {
    key <- as.character(scope_set_id)
    hist_by_set <- state$refit_meta$theta_mean_history_by_phase_a_set %||% list()
    set_history <- hist_by_set[[key]] %||% list()
    hist_by_set[[key]] <- c(set_history, list(theta_mean))
    state$refit_meta$theta_mean_history_by_phase_a_set <- hist_by_set
    prior_set <- as.integer(state$refit_meta$phase_a_lag_domain_last_set_id %||% NA_integer_)
    state$refit_meta$phase_a_lag_domain_last_set_id <- as.integer(scope_set_id)
    if (!identical(prior_set, as.integer(scope_set_id))) {
      reset_map <- state$refit_meta$phase_a_lag_domain_reset_refit_id_by_set %||% list()
      reset_map[[key]] <- as.integer(nrow(state$round_log) + 1L)
      state$refit_meta$phase_a_lag_domain_reset_refit_id_by_set <- reset_map
    }
  }

  refit_context <- .adaptive_btl_refit_context(state, last_refit_M_done, last_refit_step)

  state$btl_fit <- fit
  if (!is.na(scope_set_id)) {
    key <- as.character(scope_set_id)
    m_done_map <- state$refit_meta$last_refit_M_done_by_phase_a_set %||% list()
    step_map <- state$refit_meta$last_refit_step_by_phase_a_set %||% list()
    m_done_map[[key]] <- as.integer(M_done)
    step_map[[key]] <- as.integer(refit_context$step_id_at_refit)
    state$refit_meta$last_refit_M_done_by_phase_a_set <- m_done_map
    state$refit_meta$last_refit_step_by_phase_a_set <- step_map
  } else {
    state$refit_meta$last_refit_M_done <- M_done
    state$refit_meta$last_refit_step <- refit_context$step_id_at_refit
  }
  state$refit_meta$last_refit_round_id <- as.integer(nrow(state$round_log) + 1L)
  state <- .adaptive_update_identifiability_state(state, config)

  list(
    state = state,
    refit_performed = TRUE,
    refit_context = refit_context,
    config = config
  )
}

#' @keywords internal
#' @noRd
compute_stop_metrics <- function(state, config, phase_b_global_draws = NULL) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }
  config <- .adaptive_btl_resolve_config(state, config)

  fit <- state$btl_fit %||% NULL
  if (is.null(fit)) {
    return(NULL)
  }

  draws <- fit$btl_posterior_draws %||% NULL
  if (!is.matrix(draws) || !is.numeric(draws)) {
    rlang::abort("`btl_posterior_draws` must be a numeric matrix.")
  }
  if (nrow(draws) < 2L) {
    rlang::abort("`btl_posterior_draws` must have at least two draws.")
  }

  ids <- as.character(state$item_ids)
  theta_mean_named <- .adaptive_btl_fit_theta_mean(fit)
  theta_history <- state$refit_meta$theta_mean_history %||% list()
  controller <- .adaptive_controller_resolve(state)
  if (isTRUE(.adaptive_link_phase_b_active(state, controller = controller))) {
    combined_draws <- phase_b_global_draws %||%
      .adaptive_phase_b_global_metric_draws(state, controller = controller)
    if (is.matrix(combined_draws) && is.numeric(combined_draws)) {
      draws <- combined_draws
      theta_mean_named <- stats::setNames(as.double(colMeans(draws)), as.character(colnames(draws)))
      theta_history <- state$refit_meta$phase_b_global_theta_mean_history %||% list()
      expected_refit_id <- as.integer(nrow(state$round_log %||% tibble::tibble()) + 1L)
      if (length(theta_history) < expected_refit_id) {
        theta_history <- c(
          theta_history,
          rep_len(list(NULL), expected_refit_id - length(theta_history))
        )
        theta_history[[expected_refit_id]] <- theta_mean_named
      }
    }
  }
  theta_mean <- as.double(theta_mean_named)
  names(theta_mean) <- as.character(names(theta_mean_named))
  theta_sd_eap <- stats::sd(theta_mean)
  reliability_EAP <- compute_reliability_EAP(draws)

  scope <- .adaptive_stop_metric_scope(state, ids = ids)
  scope_ids <- as.character(scope$scope_ids %||% ids)
  if (!all(scope_ids %in% colnames(draws))) {
    scope_ids <- as.character(intersect(scope_ids, colnames(draws)))
  }
  draws_scope <- if (length(scope_ids) >= 2L) {
    draws[, scope_ids, drop = FALSE]
  } else {
    draws
  }
  theta_mean_scope <- as.double(colMeans(draws_scope))
  theta_sd_eap_scope <- stats::sd(theta_mean_scope)
  reliability_EAP_scope <- compute_reliability_EAP(draws_scope)

  diagnostics <- fit$diagnostics %||% list()
  divergences <- as.integer(diagnostics$divergences %||% NA_integer_)
  max_rhat <- as.double(diagnostics$max_rhat %||% NA_real_)
  min_ess_bulk <- as.double(diagnostics$min_ess_bulk %||% NA_real_)
  near_stop_active <- isTRUE(state$refit_meta$near_stop)

  ess_bulk_required <- if (isTRUE(near_stop_active)) {
    as.double(config$ess_bulk_min_near_stop)
  } else {
    as.double(config$ess_bulk_min)
  }

  max_rhat_allowed <- as.double(config$max_rhat)
  divergences_max <- as.integer(config$divergences_max)
  diagnostics_divergences_pass <- !is.na(divergences) && divergences <= divergences_max
  diagnostics_rhat_pass <- !is.na(max_rhat) && max_rhat <= max_rhat_allowed
  diagnostics_ess_pass <- !is.na(min_ess_bulk) && !is.na(ess_bulk_required) && min_ess_bulk >= ess_bulk_required

  diagnostics_pass <- isTRUE(diagnostics_divergences_pass) &&
    isTRUE(diagnostics_rhat_pass) &&
    isTRUE(diagnostics_ess_pass)

  eap_min <- as.double(config$eap_reliability_min)
  eap_pass <- isTRUE(diagnostics_pass) &&
    is.finite(reliability_EAP) &&
    reliability_EAP >= eap_min

  history <- theta_history
  current_refit <- length(history)
  use_scope_history <- identical(as.character(scope$phase_scope %||% "global"), "phase_a_set") &&
    is.finite(as.integer(scope$phase_scope_set_id %||% NA_integer_))
  history_scope <- history
  if (isTRUE(use_scope_history)) {
    scope_key <- as.character(as.integer(scope$phase_scope_set_id))
    scoped_history <- state$refit_meta$theta_mean_history_by_phase_a_set[[scope_key]] %||% list()
    if (length(scoped_history) > 0L) {
      history_scope <- scoped_history
    }
  }
  current_refit_scope <- length(history_scope)
  stability_lag <- as.integer(config$stability_lag)
  lag_eligible <- !is.na(stability_lag) &&
    stability_lag >= 1L &&
    current_refit > stability_lag

  rho_theta <- NA_real_
  theta_corr_pass <- NA
  delta_sd_theta <- NA_real_
  delta_sd_theta_pass <- NA
  rho_rank <- NA_real_
  rho_rank_pass <- NA

  rho_theta_scope <- NA_real_
  theta_corr_pass_scope <- NA
  delta_sd_theta_scope <- NA_real_
  delta_sd_theta_pass_scope <- NA
  rho_rank_scope <- NA_real_
  rho_rank_pass_scope <- NA
  lag_eligible_scope <- !is.na(stability_lag) &&
    stability_lag >= 1L &&
    current_refit_scope > stability_lag

  if (isTRUE(lag_eligible)) {
    lag_idx <- current_refit - stability_lag
    lag_theta <- history[[lag_idx]]
    lag_theta <- as.double(lag_theta)
    if (length(lag_theta) == length(theta_mean)) {
      names(lag_theta) <- names(theta_mean)
    }
    if (length(lag_theta) == length(theta_mean)) {
      rho_theta <- stats::cor(theta_mean, lag_theta, use = "pairwise.complete.obs")
      sd_current <- stats::sd(theta_mean)
      sd_lag <- stats::sd(lag_theta)
      if (is.finite(sd_current) && is.finite(sd_lag) && sd_lag > 0) {
        delta_sd_theta <- abs(sd_current - sd_lag) / sd_lag
      }
      rank_current <- rank(theta_mean, ties.method = "average")
      rank_lag <- rank(lag_theta, ties.method = "average")
      rho_rank <- stats::cor(rank_current, rank_lag, method = "spearman", use = "pairwise.complete.obs")
    }

    lag_scope <- NULL
    if (isTRUE(lag_eligible_scope)) {
      lag_scope_idx <- current_refit_scope - stability_lag
      lag_scope_theta <- history_scope[[lag_scope_idx]] %||% NULL
      lag_scope_theta_names <- names(lag_scope_theta)
      lag_scope_theta <- as.double(lag_scope_theta)
      if (!is.null(lag_scope_theta_names)) {
        names(lag_scope_theta) <- as.character(lag_scope_theta_names)
      }
      if (!is.null(names(lag_scope_theta)) && all(scope_ids %in% names(lag_scope_theta))) {
        lag_scope <- as.double(lag_scope_theta[scope_ids])
      } else if (length(lag_scope_theta) == length(scope_ids) && length(scope_ids) >= 2L) {
        lag_scope <- lag_scope_theta
      } else if (length(lag_scope_theta) == length(ids) && length(scope_ids) >= 2L) {
        names(lag_scope_theta) <- ids
        if (all(scope_ids %in% names(lag_scope_theta))) {
          lag_scope <- as.double(lag_scope_theta[scope_ids])
        }
      }
    }
    if (!is.null(lag_scope) && length(lag_scope) == length(theta_mean_scope) && length(lag_scope) >= 2L) {
      rho_theta_scope <- stats::cor(theta_mean_scope, lag_scope, use = "pairwise.complete.obs")
      sd_scope <- stats::sd(theta_mean_scope)
      sd_scope_lag <- stats::sd(lag_scope)
      if (is.finite(sd_scope) && is.finite(sd_scope_lag) && sd_scope_lag > 0) {
        delta_sd_theta_scope <- abs(sd_scope - sd_scope_lag) / sd_scope_lag
      }
      rank_scope <- rank(theta_mean_scope, ties.method = "average")
      rank_scope_lag <- rank(lag_scope, ties.method = "average")
      rho_rank_scope <- stats::cor(rank_scope, rank_scope_lag, method = "spearman", use = "pairwise.complete.obs")
    }

    theta_corr_pass <- if (is.finite(rho_theta)) {
      rho_theta >= as.double(config$theta_corr_min)
    } else {
      NA
    }
    delta_sd_theta_pass <- if (is.finite(delta_sd_theta)) {
      delta_sd_theta <= as.double(config$theta_sd_rel_change_max)
    } else {
      NA
    }
    rho_rank_pass <- is.finite(rho_rank) && rho_rank >= as.double(config$rank_spearman_min)

    theta_corr_pass_scope <- if (is.finite(rho_theta_scope)) {
      rho_theta_scope >= as.double(config$theta_corr_min)
    } else {
      NA
    }
    delta_sd_theta_pass_scope <- if (is.finite(delta_sd_theta_scope)) {
      delta_sd_theta_scope <= as.double(config$theta_sd_rel_change_max)
    } else {
      NA
    }
    rho_rank_pass_scope <- is.finite(rho_rank_scope) &&
      rho_rank_scope >= as.double(config$rank_spearman_min)
  }

  eap_min <- as.double(config$eap_reliability_min)
  eap_pass_scope <- isTRUE(diagnostics_pass) &&
    is.finite(reliability_EAP_scope) &&
    reliability_EAP_scope >= eap_min

  list(
    phase_scope = as.character(scope$phase_scope %||% "global"),
    phase_scope_set_id = as.integer(scope$phase_scope_set_id %||% NA_integer_),
    phase_scope_n_items = as.integer(length(scope_ids)),
    round_log_deferred_audit_payload = .adaptive_round_log_deferred_audit_payload(
      draws = draws_scope,
      near_tie_p_low = config$near_tie_p_low %||% 0.40,
      near_tie_p_high = config$near_tie_p_high %||% 0.60,
      max_draws = config$deferred_audit_max_draws %||% 400L
    ),
    diagnostics_pass = diagnostics_pass,
    diagnostics_divergences_pass = diagnostics_divergences_pass,
    diagnostics_rhat_pass = diagnostics_rhat_pass,
    diagnostics_ess_pass = diagnostics_ess_pass,
    divergences = divergences,
    divergences_max_allowed = divergences_max,
    max_rhat = max_rhat,
    max_rhat_allowed = max_rhat_allowed,
    min_ess_bulk = min_ess_bulk,
    ess_bulk_required = ess_bulk_required,
    near_stop_active = as.logical(near_stop_active),
    reliability_EAP = reliability_EAP,
    reliability_EAP_scope = reliability_EAP_scope,
    eap_reliability_min = eap_min,
    eap_pass = eap_pass,
    eap_pass_scope = eap_pass_scope,
    theta_sd_eap = theta_sd_eap,
    theta_sd_eap_scope = theta_sd_eap_scope,
    rho_theta = rho_theta,
    rho_theta_scope = rho_theta_scope,
    theta_corr_min = as.double(config$theta_corr_min),
    theta_corr_pass = theta_corr_pass,
    theta_corr_pass_scope = theta_corr_pass_scope,
    delta_sd_theta = delta_sd_theta,
    delta_sd_theta_scope = delta_sd_theta_scope,
    theta_sd_rel_change_max = as.double(config$theta_sd_rel_change_max),
    delta_sd_theta_pass = delta_sd_theta_pass,
    delta_sd_theta_pass_scope = delta_sd_theta_pass_scope,
    rho_rank = rho_rank,
    rho_rank_scope = rho_rank_scope,
    rank_spearman_min = as.double(config$rank_spearman_min),
    rho_rank_pass = rho_rank_pass,
    rho_rank_pass_scope = rho_rank_pass_scope,
    lag_eligible = lag_eligible,
    lag_eligible_scope = lag_eligible_scope
  )
}

#' @keywords internal
#' @noRd
.adaptive_maybe_enter_phase3 <- function(state, metrics, config) {
  if (isTRUE(state$refit_meta$near_stop)) {
    return(state)
  }
  if (!isTRUE(metrics$diagnostics_pass)) {
    return(state)
  }
  eap_min <- as.double(config$eap_reliability_min)
  reliability_value <- as.double(metrics$reliability_EAP %||% NA_real_)
  if (identical(as.character(metrics$phase_scope %||% "global"), "phase_a_set")) {
    reliability_value <- as.double(metrics$reliability_EAP_scope %||% reliability_value)
  }
  threshold <- eap_min - 0.05
  if (is.finite(reliability_value) && reliability_value >= threshold) {
    state$refit_meta$near_stop <- TRUE
  }
  state
}

#' @keywords internal
#' @noRd
should_stop <- function(metrics, config) {
  if (is.null(metrics) || !is.list(metrics)) {
    return(FALSE)
  }
  if (is.null(config) || !is.list(config)) {
    return(FALSE)
  }

  if (!isTRUE(metrics$diagnostics_pass)) {
    return(FALSE)
  }
  use_scope <- identical(as.character(metrics$phase_scope %||% "global"), "phase_a_set")
  reliability <- as.double(if (isTRUE(use_scope)) {
    metrics$reliability_EAP_scope %||% metrics$reliability_EAP
  } else {
    metrics$reliability_EAP
  })
  lag_eligible <- as.logical(if (isTRUE(use_scope)) {
    metrics$lag_eligible_scope %||% metrics$lag_eligible
  } else {
    metrics$lag_eligible
  })
  rho_theta <- as.double(if (isTRUE(use_scope)) {
    metrics$rho_theta_scope %||% metrics$rho_theta
  } else {
    metrics$rho_theta
  })
  delta_sd_theta <- as.double(if (isTRUE(use_scope)) {
    metrics$delta_sd_theta_scope %||% metrics$delta_sd_theta
  } else {
    metrics$delta_sd_theta
  })
  rho_rank <- as.double(if (isTRUE(use_scope)) {
    metrics$rho_rank_scope %||% metrics$rho_rank
  } else {
    metrics$rho_rank
  })

  eap_min <- as.double(config$eap_reliability_min)
  if (!is.finite(reliability) || reliability < eap_min) {
    return(FALSE)
  }
  if (!isTRUE(lag_eligible)) {
    return(FALSE)
  }

  theta_corr_min <- as.double(config$theta_corr_min)
  theta_sd_rel_change_max <- as.double(config$theta_sd_rel_change_max)
  rank_spearman_min <- as.double(config$rank_spearman_min)

  if (!is.finite(rho_theta) || rho_theta < theta_corr_min) {
    return(FALSE)
  }
  if (!is.finite(delta_sd_theta) || delta_sd_theta > theta_sd_rel_change_max) {
    return(FALSE)
  }
  if (!is.finite(rho_rank) || rho_rank < rank_spearman_min) {
    return(FALSE)
  }

  TRUE
}
