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
    near_tie_p_high = 0.60
  )
}

.adaptive_btl_resolve_config <- function(state, config) {
  defaults <- .adaptive_btl_defaults(state$n_items)
  if (is.null(config)) {
    return(defaults)
  }
  if (!is.list(config)) {
    rlang::abort("`config` must be a list when provided.")
  }
  utils::modifyList(defaults, config)
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
  refit_pairs_target <- config$refit_pairs_target %||% .btl_mcmc_clamp(
    20L,
    5000L,
    as.integer(ceiling(effective_n / 2))
  )
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
  history <- .adaptive_history_tbl(state)
  M_done <- 0L
  if (nrow(history) > 0L) {
    set_map <- stats::setNames(as.integer(state$items$set_id), as.character(state$items$item_id))
    a_set <- as.integer(set_map[as.character(history$A_id)])
    b_set <- as.integer(set_map[as.character(history$B_id)])
    M_done <- as.integer(sum(a_set == set_id & b_set == set_id, na.rm = TRUE))
  }

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
  if (identical(as.character(controller$link_estimation_mode %||% "transform"), "anchored_joint")) {
    accepted_state <- .adaptive_link_anchored_joint_resolve_state(
      state = state,
      spoke_id = as.integer(spoke_id),
      controller = controller
    )
    return(list(
      link_transform_state = NA_character_,
      delta_spoke_mean = NA_real_,
      log_alpha_spoke_mean = NA_real_,
      theta_spoke_global_mean = accepted_state$theta_spoke_global_mean
    ))
  }
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
.adaptive_phase_b_global_metric_draws <- function(state, controller = NULL) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  if (!isTRUE(.adaptive_link_phase_b_active(state, controller = controller))) {
    return(NULL)
  }

  phase_a <- state$linking$phase_a %||% list()
  required_sets <- as.integer(
    phase_a$required_sets %||% .adaptive_phase_a_required_sets(state, controller = controller)
  )
  required_sets <- sort(unique(required_sets[!is.na(required_sets)]))
  if (length(required_sets) < 1L) {
    rlang::abort("Phase B global metric reconstruction requires non-empty `required_sets`.")
  }

  hub_id <- as.integer(controller$hub_id %||% 1L)
  per_set_draws <- vector("list", length(required_sets))
  names(per_set_draws) <- as.character(required_sets)
  common_n_draws <- Inf

  for (set_id in required_sets) {
    set_draws <- .adaptive_phase_a_artifact_draws_for_phase_b_global(state, set_id = set_id)
    per_set_draws[[as.character(set_id)]] <- set_draws
    common_n_draws <- min(common_n_draws, nrow(set_draws))
  }

  common_n_draws <- as.integer(common_n_draws %||% NA_integer_)
  if (!is.finite(common_n_draws) || common_n_draws < 2L) {
    rlang::abort(
      "Phase B global metric reconstruction requires at least two aligned Phase A posterior draws."
    )
  }

  combined <- vector("list", length(required_sets))
  names(combined) <- names(per_set_draws)
  for (set_id in required_sets) {
    key <- as.character(set_id)
    set_draws <- per_set_draws[[key]][seq_len(common_n_draws), , drop = FALSE]
    if (!identical(as.integer(set_id), hub_id)) {
      transform <- .adaptive_phase_b_global_metric_transform_stats(
        state = state,
        spoke_id = as.integer(set_id),
        controller = controller
      )
      if (identical(as.character(controller$link_estimation_mode %||% "transform"), "anchored_joint")) {
        theta_mean <- as.double(transform$theta_spoke_global_mean %||% numeric())
        set_draws <- matrix(
          rep(theta_mean, each = common_n_draws),
          nrow = common_n_draws,
          byrow = FALSE,
          dimnames = list(NULL, names(transform$theta_spoke_global_mean))
        )
      } else {
        alpha <- if (identical(transform$link_transform_state, "shift_scale")) {
          exp(as.double(transform$log_alpha_spoke_mean))
        } else {
          1
        }
        set_draws <- as.double(transform$delta_spoke_mean) + alpha * set_draws
        dim(set_draws) <- c(common_n_draws, ncol(per_set_draws[[key]]))
        colnames(set_draws) <- colnames(per_set_draws[[key]])
      }
    }
    combined[[key]] <- set_draws
  }

  combined_draws <- do.call(cbind, combined)
  ids <- as.character(state$item_ids)
  if (!all(ids %in% colnames(combined_draws))) {
    rlang::abort(
      "Phase B global metric reconstruction failed to cover the full runtime item domain."
    )
  }

  .pairwiseLLM_sanitize_draws_matrix(
    combined_draws[, ids, drop = FALSE],
    name = "phase_b_global_metric_draws"
  )
}

#' @keywords internal
#' @noRd
.adaptive_phase_b_global_metric_history_update <- function(state, refit_id = NULL) {
  controller <- .adaptive_controller_resolve(state)
  if (!isTRUE(.adaptive_link_phase_b_active(state, controller = controller))) {
    return(state)
  }

  draws <- .adaptive_phase_b_global_metric_draws(state, controller = controller)
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
  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  if (nrow(step_log) == 0L) {
    return(tibble::tibble())
  }
  ok <- !is.na(step_log$pair_id)
  step_log <- step_log[ok, , drop = FALSE]
  if (nrow(step_log) == 0L) {
    return(tibble::tibble())
  }

  ids <- as.character(state$item_ids)
  A_id <- ids[step_log$A]
  B_id <- ids[step_log$B]
  if (!is.null(scope_ids)) {
    scope_ids <- as.character(scope_ids)
    in_scope <- A_id %in% scope_ids & B_id %in% scope_ids
    step_log <- step_log[in_scope, , drop = FALSE]
    A_id <- A_id[in_scope]
    B_id <- B_id[in_scope]
    if (nrow(step_log) == 0L) {
      return(tibble::tibble())
    }
  }
  y_vals <- as.integer(step_log$Y)
  if (any(is.na(y_vals) | !y_vals %in% c(0L, 1L))) {
    rlang::abort(
      "Adaptive refit invariant failed: committed step rows must encode Y in {0,1} with Y=1 meaning A wins."
    )
  }
  winner_pos <- ifelse(step_log$Y == 1L, 1L, 2L)
  better_id <- ifelse(step_log$Y == 1L, A_id, B_id)
  controller <- .adaptive_controller_resolve(state)
  run_mode <- as.character(controller$run_mode %||% "within_set")
  is_link_mode <- run_mode %in% c("link_one_spoke", "link_multi_spoke")
  phase_a <- state$linking$phase_a %||% list()
  phase_b_ready <- isTRUE(phase_a$ready_for_phase_b %||% FALSE)
  phase_b_start_step <- as.integer(phase_a$phase_b_started_at_step %||% NA_integer_)
  has_cross <- "is_cross_set" %in% names(step_log)
  is_cross <- if (isTRUE(has_cross)) step_log$is_cross_set %in% TRUE else rep(FALSE, nrow(step_log))
  phase_is_b <- rep(FALSE, nrow(step_log))
  if (isTRUE(is_link_mode) && isTRUE(has_cross) && is.finite(phase_b_start_step)) {
    # Prefer explicit phase metadata when available.
    phase_is_b <- as.integer(step_log$step_id) >= phase_b_start_step
  } else if (isTRUE(is_link_mode) && isTRUE(has_cross)) {
    # Guarded legacy fallback for resumed sessions without explicit boundary metadata.
    phase_is_b <- cumsum(is_cross) > 0L
  } else if (isTRUE(is_link_mode) && isTRUE(phase_b_ready)) {
    phase_is_b <- rep(TRUE, nrow(step_log))
  }
  phase <- rep("phase2", nrow(step_log))
  if (isTRUE(is_link_mode)) {
    phase <- ifelse(phase_is_b, "phase3", "phase2")
  }
  judge_mode <- as.character(controller$judge_param_mode %||% "global_shared")
  judge_scope <- rep("shared", nrow(step_log))
  if (identical(judge_mode, "phase_specific")) {
    judge_scope <- ifelse(phase_is_b, "link", "within")
  }

  tibble::tibble(
    pair_uid = paste0("pair_", step_log$pair_id),
    unordered_key = make_unordered_key(A_id, B_id),
    ordered_key = make_ordered_key(A_id, B_id),
    A_id = as.character(A_id),
    B_id = as.character(B_id),
    better_id = as.character(better_id),
    winner_pos = as.integer(winner_pos),
    phase = as.character(phase),
    judge_scope = as.character(judge_scope),
    iter = as.integer(step_log$step_id),
    received_at = step_log$timestamp,
    backend = rep("adaptive", nrow(step_log)),
    model = rep("adaptive", nrow(step_log))
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
  if (identical(as.character(controller$link_estimation_mode %||% "transform"), "anchored_joint")) {
    return(NA_character_)
  }
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
  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  hub_active_cross <- character()
  if (nrow(step_log) > 0L &&
    all(
      c("pair_id", "is_cross_set", "link_spoke_id", "set_i", "set_j", "i", "j", "is_probe_step") %in%
        names(step_log)
    )) {
    link_spoke <- as.integer(step_log$link_spoke_id)
    cumulative <- step_log[
      !is.na(step_log$pair_id) &
        step_log$is_cross_set %in% TRUE &
        !is.na(link_spoke) &
        link_spoke == as.integer(spoke_id) &
        !(step_log$is_probe_step %in% TRUE),
      ,
      drop = FALSE
    ]
    if (nrow(cumulative) > 0L) {
      hub_active_cross <- unique(vapply(seq_len(nrow(cumulative)), function(k) {
        if (as.integer(cumulative$set_i[[k]]) == as.integer(hub_id)) {
          state$item_ids[[as.integer(cumulative$i[[k]])]]
        } else if (as.integer(cumulative$set_j[[k]]) == as.integer(hub_id)) {
          state$item_ids[[as.integer(cumulative$j[[k]])]]
        } else {
          NA_character_
        }
      }, character(1L)))
      hub_active_cross <- hub_active_cross[!is.na(hub_active_cross)]
    }
  }
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

  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  if (nrow(step_log) > 0L &&
    all(c("pair_id", "step_id", "is_cross_set", "link_spoke_id", "is_probe_step") %in% names(step_log))) {
    rows <- step_log[
      !is.na(step_log$pair_id) &
        step_log$is_cross_set %in% TRUE &
        as.integer(step_log$link_spoke_id) == as.integer(spoke_id) &
        !(step_log$is_probe_step %in% TRUE),
      ,
      drop = FALSE
    ]
    if (nrow(rows) > 0L) {
      return(as.integer(min(as.integer(rows$step_id), na.rm = TRUE)))
    }
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
  if (identical(as.character(link_estimation_mode %||% "transform"), "anchored_joint")) {
    accepted_state <- accepted_state %||% .adaptive_link_anchored_joint_resolve_state(
      state = state,
      spoke_id = as.integer(spoke_id)
    )
    hub_mu <- as.double(accepted_state$theta_hub_fixed)
    names(hub_mu) <- names(accepted_state$theta_hub_fixed)
    hub_sd <- .adaptive_phase_a_artifact_item_field_map(state, hub_id, "theta_raw_sd")
    hub_sd[!is.finite(hub_sd) | hub_sd < 0] <- 0
    spoke_mu <- as.double(accepted_state$theta_spoke_global_mean)
    names(spoke_mu) <- names(accepted_state$theta_spoke_global_mean)
    spoke_sd <- as.double(accepted_state$theta_spoke_global_sd)
    names(spoke_sd) <- names(accepted_state$theta_spoke_global_sd)
    spoke_sd[!is.finite(spoke_sd) | spoke_sd < 0] <- 0
    mean_map <- stats::setNames(rep(NA_real_, length(active_ids)), active_ids)
    var_map <- stats::setNames(rep(NA_real_, length(active_ids)), active_ids)
    for (item_id in active_ids) {
      if (item_id %in% names(hub_mu)) {
        mean_map[[item_id]] <- as.double(hub_mu[[item_id]])
        var_map[[item_id]] <- as.double((hub_sd[[item_id]] %||% 0)^2)
      } else if (item_id %in% names(spoke_mu)) {
        mean_map[[item_id]] <- as.double(spoke_mu[[item_id]])
        var_map[[item_id]] <- as.double((spoke_sd[[item_id]] %||% 0)^2)
      }
    }
    decomp <- .adaptive_link_reliability_decomposition(
      mu_vals = unname(mean_map),
      var_vals = unname(var_map),
      var_mu_epsilon = var_mu_epsilon,
      total_var_epsilon = total_var_epsilon
    )
    return(c(
      decomp,
      list(
        mean_map = mean_map,
        var_map = var_map
      )
    ))
  }
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
    if (!identical(as.character(hub_lock_mode), "hard_lock")) {
      hub_mu <- fit$theta_hub_post %||% hub_mu
      if (is.matrix(fit_post$theta_hub) && !is.null(colnames(fit_post$theta_hub))) {
        hub_sd_draw <- apply(fit_post$theta_hub, 2L, stats::sd)
        hub_sd <- as.double(hub_sd_draw)
        names(hub_sd) <- names(hub_sd_draw)
      }
    }
    spoke_mu <- fit$theta_spoke_post %||% spoke_mu
    if (is.matrix(fit_post$theta_spoke) && !is.null(colnames(fit_post$theta_spoke))) {
      spoke_sd_draw <- apply(fit_post$theta_spoke, 2L, stats::sd)
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
        !identical(as.character(hub_lock_mode), "hard_lock") &&
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
  probe <- .adaptive_link_probe_state(state)
  cache <- tibble::as_tibble(probe$prediction_cache %||% tibble::tibble())
  realized <- tibble::as_tibble(probe$realized_edges %||% tibble::tibble())
  if (nrow(cache) < 1L || nrow(realized) < 1L) {
    return(list(probe_brier = NA_real_, realized_n = 0L))
  }
  current <- cache[
    as.integer(cache$refit_id) == as.integer(refit_id) &
      as.integer(cache$spoke_id) == as.integer(spoke_id),
    ,
    drop = FALSE
  ]
  if (nrow(current) < 1L) {
    return(list(probe_brier = NA_real_, realized_n = 0L))
  }
  joined <- dplyr::inner_join(
    current,
    realized[, c("spoke_id", "link_epoch_id", "pair_key", "Y"), drop = FALSE],
    by = c("spoke_id", "link_epoch_id", "pair_key")
  )
  if (nrow(joined) < 1L) {
    return(list(probe_brier = NA_real_, realized_n = 0L))
  }
  y_spoke <- as.integer(joined$Y)
  pred_spoke <- 1 - as.double(joined$pred_prob)
  keep <- y_spoke %in% c(0L, 1L) & is.finite(pred_spoke)
  if (!any(keep)) {
    return(list(probe_brier = NA_real_, realized_n = 0L))
  }
  list(
    probe_brier = as.double(mean((y_spoke[keep] - pred_spoke[keep])^2)),
    realized_n = as.integer(sum(keep))
  )
}

.adaptive_link_probe_pred_rmse_lagged <- function(state, refit_id, spoke_id, lag_refit_id, epoch_id) {
  probe <- .adaptive_link_probe_state(state)
  cache <- tibble::as_tibble(probe$prediction_cache %||% tibble::tibble())
  if (nrow(cache) < 1L) {
    return(NA_real_)
  }
  current <- cache[
    as.integer(cache$refit_id) == as.integer(refit_id) &
      as.integer(cache$spoke_id) == as.integer(spoke_id) &
      as.integer(cache$link_epoch_id) == as.integer(epoch_id),
    ,
    drop = FALSE
  ]
  lagged <- cache[
    as.integer(cache$refit_id) == as.integer(lag_refit_id) &
      as.integer(cache$spoke_id) == as.integer(spoke_id) &
      as.integer(cache$link_epoch_id) == as.integer(epoch_id),
    ,
    drop = FALSE
  ]
  if (nrow(current) < 1L || nrow(lagged) < 1L) {
    return(NA_real_)
  }
  joined <- dplyr::inner_join(
    current[, c("pair_key", "pred_prob"), drop = FALSE],
    lagged[, c("pair_key", "pred_prob"), drop = FALSE],
    by = "pair_key",
    suffix = c("_t", "_lag")
  )
  if (nrow(joined) < 1L) {
    return(NA_real_)
  }
  diff <- as.double(joined$pred_prob_t) - as.double(joined$pred_prob_lag)
  diff <- diff[is.finite(diff)]
  if (length(diff) < 1L) {
    return(NA_real_)
  }
  sqrt(mean(diff^2))
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
.adaptive_link_probe_pred_rmse_lagged_anchored_joint <- function(edges,
                                                                 current_theta,
                                                                 lag_theta,
                                                                 judge_params = list(
                                                                   beta = 0,
                                                                   epsilon = 0
                                                                 )) {
  edges <- tibble::as_tibble(edges)
  if (nrow(edges) < 1L) {
    return(NA_real_)
  }
  if (!is.numeric(current_theta) || is.null(names(current_theta)) ||
    !is.numeric(lag_theta) || is.null(names(lag_theta))) {
    return(NA_real_)
  }

  current_names <- as.character(names(current_theta))
  current_theta <- as.double(current_theta)
  names(current_theta) <- current_names
  lag_names <- as.character(names(lag_theta))
  lag_theta <- as.double(lag_theta)
  names(lag_theta) <- lag_names

  hub_items <- unique(as.character(edges$hub_item))
  spoke_items <- unique(as.character(edges$spoke_item))
  if (!all(hub_items %in% names(current_theta)) || !all(hub_items %in% names(lag_theta)) ||
    !all(spoke_items %in% names(current_theta)) || !all(spoke_items %in% names(lag_theta))) {
    return(NA_real_)
  }

  p_now <- .adaptive_link_cross_probabilities(
    edges = edges,
    hub_theta = current_theta[hub_items],
    spoke_theta = current_theta[spoke_items],
    delta_mean = 0,
    log_alpha_mean = NA_real_,
    judge_params = judge_params
  )
  p_lag <- .adaptive_link_cross_probabilities(
    edges = edges,
    hub_theta = lag_theta[hub_items],
    spoke_theta = lag_theta[spoke_items],
    delta_mean = 0,
    log_alpha_mean = NA_real_,
    judge_params = judge_params
  )
  keep <- is.finite(p_now) & is.finite(p_lag)
  if (!any(keep)) {
    return(NA_real_)
  }
  sqrt(mean((p_now[keep] - p_lag[keep])^2))
}

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
    row$reliability_link_global[[1L]] %||%
      row$reliability_EAP_link[[1L]] %||%
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
  vals[is.finite(vals)]
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

  beta_shared <- as.double(fit$beta_mean %||% 0)
  epsilon_shared <- as.double(fit$epsilon_mean %||% 0)
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

.adaptive_link_anchored_joint_resolve_state <- function(state, spoke_id, controller = NULL) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  if (!identical(as.character(controller$link_estimation_mode %||% "transform"), "anchored_joint")) {
    rlang::abort("Anchored-joint accepted-state lookup requires `link_estimation_mode = anchored_joint`.")
  }
  anchored <- (state$linking %||% list())$anchored_joint %||% .adaptive_anchored_joint_empty_state()
  accepted_state <- (anchored$accepted_state_by_spoke %||% list())[[as.character(spoke_id)]] %||% NULL
  if (is.null(accepted_state)) {
    accepted_state <- .adaptive_anchored_joint_artifact_copy_init(
      state = state,
      spoke_id = as.integer(spoke_id),
      controller = controller
    )
  } else {
    accepted_state <- .adaptive_anchored_joint_validate_current_state(
      state_obj = accepted_state,
      state = state,
      spoke_id = as.integer(spoke_id),
      controller = controller
    )
  }
  accepted_state
}

.adaptive_link_anchored_joint_free_block_dim <- function(state, spoke_id, controller = NULL) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  fisher_map <- ((state$linking %||% list())$anchored_joint %||% list())$fisher_t0_by_spoke %||% list()
  dim_n <- as.integer((fisher_map[[as.character(spoke_id)]] %||% list())$free_block_dim %||% NA_integer_)
  if (is.finite(dim_n) && !is.na(dim_n) && dim_n >= 1L) {
    return(dim_n)
  }
  accepted_state <- .adaptive_link_anchored_joint_resolve_state(
    state = state,
    spoke_id = as.integer(spoke_id),
    controller = controller
  )
  as.integer(length(accepted_state$theta_spoke_global_mean %||% numeric()))
}

.adaptive_link_anchored_joint_global_theta_map <- function(state,
                                                           spoke_id,
                                                           controller = NULL,
                                                           accepted_state = NULL) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  accepted_state <- accepted_state %||% .adaptive_link_anchored_joint_resolve_state(
    state = state,
    spoke_id = as.integer(spoke_id),
    controller = controller
  )
  c(
    accepted_state$theta_hub_fixed %||% stats::setNames(numeric(), character()),
    accepted_state$theta_spoke_global_mean %||% stats::setNames(numeric(), character())
  )
}

.adaptive_link_anchored_joint_judge_params <- function(state,
                                                       spoke_id,
                                                       controller = NULL,
                                                       accepted_state = NULL) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  accepted_state <- accepted_state %||% .adaptive_link_anchored_joint_resolve_state(
    state = state,
    spoke_id = as.integer(spoke_id),
    controller = controller
  )
  accepted_state$judge_params %||% list(
    mode = as.character(controller$judge_param_mode %||% "global_shared"),
    scope = "link",
    beta = 0,
    epsilon = 0,
    cold_start_fallback_used = FALSE
  )
}

.adaptive_link_anchored_joint_prior_sd <- function(spoke_sd, controller) {
  raw_sd <- as.double(spoke_sd)
  names(raw_sd) <- names(spoke_sd)
  scale_mult <- as.double(controller$anchored_joint_spoke_prior_scale %||% 1.0)
  sd_floor <- as.double(controller$anchored_joint_sd_floor %||% 0.02)
  fallback_sd <- as.double(controller$anchored_joint_spoke_prior_fallback_sd %||% 1.0)
  if (!is.finite(scale_mult) || scale_mult <= 0) {
    scale_mult <- 1.0
  }
  if (!is.finite(sd_floor) || sd_floor < 0) {
    sd_floor <- 0.02
  }
  if (!is.finite(fallback_sd) || fallback_sd <= 0) {
    fallback_sd <- 1.0
  }
  fallback_used <- !is.finite(raw_sd)
  prior_sd <- ifelse(
    fallback_used,
    fallback_sd,
    scale_mult * pmax(raw_sd, sd_floor)
  )
  prior_sd[!is.finite(prior_sd) | prior_sd <= 0] <- fallback_sd
  list(
    prior_sd = as.double(prior_sd),
    fallback_used = as.logical(fallback_used),
    fallback_items = as.character(names(raw_sd)[fallback_used]),
    fallback_sd = as.double(fallback_sd),
    scale_mult = as.double(scale_mult),
    sd_floor = as.double(sd_floor)
  )
}

.adaptive_link_fit_anchored_joint <- function(state,
                                              spoke_id,
                                              controller = NULL,
                                              cross_edges,
                                              judge_params = NULL,
                                              accepted_state = NULL) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  if (!identical(as.character(controller$link_estimation_mode %||% "transform"), "anchored_joint")) {
    rlang::abort("Anchored-joint fitting requires `link_estimation_mode = anchored_joint`.")
  }
  hub_id <- as.integer(controller$hub_id %||% 1L)
  accepted_state <- accepted_state %||% .adaptive_link_anchored_joint_resolve_state(
    state = state,
    spoke_id = as.integer(spoke_id),
    controller = controller
  )
  judge_params <- judge_params %||% .adaptive_link_judge_params(
    state = state,
    controller = controller,
    scope = "link",
    allow_cold_start_fallback = TRUE,
    expected_link_params = FALSE
  )
  hub_evidence <- .adaptive_phase_a_artifact_resolve_within_set_evidence(
    artifact = state$linking$phase_a$artifacts[[as.character(hub_id)]],
    state = state,
    set_id = hub_id,
    controller = controller
  )
  spoke_evidence <- .adaptive_phase_a_artifact_resolve_within_set_evidence(
    artifact = state$linking$phase_a$artifacts[[as.character(spoke_id)]],
    state = state,
    set_id = as.integer(spoke_id),
    controller = controller
  )

  prior_mean <- .adaptive_phase_a_artifact_item_field_map(state, spoke_id, "theta_raw_mean")
  prior_sd_info <- .adaptive_link_anchored_joint_prior_sd(
    spoke_sd = .adaptive_phase_a_artifact_item_field_map(state, spoke_id, "theta_raw_sd"),
    controller = controller
  )
  prior_sd <- prior_sd_info$prior_sd
  names(prior_sd) <- names(prior_mean)
  if (any(prior_sd_info$fallback_used)) {
    rlang::warn(
      paste0(
        "Anchored-joint spoke prior SD fallback applied for spoke_id=",
        as.integer(spoke_id),
        ": ",
        paste(prior_sd_info$fallback_items, collapse = ", "),
        "."
      )
    )
  }

  theta_hub_fixed <- accepted_state$theta_hub_fixed
  spoke_items <- as.character(names(prior_mean))
  par_init <- as.double(accepted_state$theta_spoke_global_mean[spoke_items])
  if (length(par_init) != length(spoke_items) || any(!is.finite(par_init))) {
    par_init <- as.double(prior_mean[spoke_items])
  }
  names(par_init) <- spoke_items
  idx_map <- stats::setNames(seq_along(spoke_items), spoke_items)

  cross_edges <- tibble::as_tibble(cross_edges)
  cross_h <- as.character(cross_edges$hub_item %||% character())
  cross_x <- as.character(cross_edges$spoke_item %||% character())
  cross_y <- as.integer(cross_edges$y_spoke %||% integer())
  cross_spoke_in_a <- as.logical(cross_edges$spoke_in_A %||% logical())
  cross_keep <- cross_y %in% c(0L, 1L) &
    cross_h %in% names(theta_hub_fixed) &
    cross_x %in% names(idx_map) &
    !is.na(cross_spoke_in_a)
  cross_h <- cross_h[cross_keep]
  cross_x <- cross_x[cross_keep]
  cross_y <- cross_y[cross_keep]
  cross_beta_signed <- ifelse(cross_spoke_in_a[cross_keep], 1, -1) *
    as.double(judge_params$beta %||% 0)

  within_a <- as.character(spoke_evidence$A_item %||% character())
  within_b <- as.character(spoke_evidence$B_item %||% character())
  within_y <- as.integer(spoke_evidence$y_A %||% integer())
  within_keep <- within_y %in% c(0L, 1L) &
    within_a %in% names(idx_map) &
    within_b %in% names(idx_map)
  within_a <- within_a[within_keep]
  within_b <- within_b[within_keep]
  within_y <- within_y[within_keep]

  beta_val <- as.double(judge_params$beta %||% 0)
  epsilon_val <- max(0, min(1, as.double(judge_params$epsilon %||% 0)))
  neg_log_post <- function(par) {
    theta_spoke <- as.double(par)
    names(theta_spoke) <- spoke_items
    nll <- 0
    if (length(cross_y) > 0L) {
      eta_cross <- theta_spoke[cross_x] - theta_hub_fixed[cross_h] + cross_beta_signed
      p_cross <- (1 - epsilon_val) * stats::plogis(eta_cross) + epsilon_val * 0.5
      p_cross <- pmax(1e-10, pmin(1 - 1e-10, p_cross))
      nll <- nll - sum(stats::dbinom(cross_y, size = 1L, prob = p_cross, log = TRUE))
    }
    if (length(within_y) > 0L) {
      eta_within <- theta_spoke[within_a] - theta_spoke[within_b] + beta_val
      p_within <- (1 - epsilon_val) * stats::plogis(eta_within) + epsilon_val * 0.5
      p_within <- pmax(1e-10, pmin(1 - 1e-10, p_within))
      nll <- nll - sum(stats::dbinom(within_y, size = 1L, prob = p_within, log = TRUE))
    }
    prior_z <- (theta_spoke - prior_mean[spoke_items]) / prior_sd[spoke_items]
    nll + 0.5 * sum(prior_z^2 + log(2 * pi * prior_sd[spoke_items]^2))
  }

  opt <- tryCatch(
    stats::optim(
      par = unname(par_init),
      fn = neg_log_post,
      method = "BFGS",
      hessian = TRUE,
      control = list(maxit = 500, reltol = 1e-10)
    ),
    error = function(e) NULL
  )
  if (is.null(opt) || !is.list(opt) || opt$convergence != 0L || !all(is.finite(opt$par))) {
    theta_spoke_post <- stats::setNames(as.double(par_init), spoke_items)
    theta_spoke_sd_post <- stats::setNames(prior_sd[spoke_items], spoke_items)
    hessian_posdef <- FALSE
  } else {
    theta_spoke_post <- stats::setNames(as.double(opt$par), spoke_items)
    hessian <- opt$hessian %||% matrix(NA_real_, nrow = length(spoke_items), ncol = length(spoke_items))
    vcov <- tryCatch(
      solve(hessian),
      error = function(e) matrix(NA_real_, nrow = length(spoke_items), ncol = length(spoke_items))
    )
    eig_vals <- tryCatch(eigen((hessian + t(hessian)) / 2, symmetric = TRUE, only.values = TRUE)$values,
      error = function(e) rep(NA_real_, length(spoke_items))
    )
    hessian_posdef <- length(eig_vals) == length(spoke_items) && all(is.finite(eig_vals)) && all(eig_vals > 0)
    theta_spoke_sd_post <- stats::setNames(rep(NA_real_, length(spoke_items)), spoke_items)
    if (all(dim(vcov) == c(length(spoke_items), length(spoke_items))) && all(is.finite(diag(vcov))) &&
      all(diag(vcov) >= 0)) {
      theta_spoke_sd_post <- stats::setNames(sqrt(diag(vcov)), spoke_items)
    }
  }

  fit_contract <- list(
    contract_type = "link_refit",
    estimation_method = "map_laplace",
    uncertainty_approximation = "laplace_hessian",
    link_refit_mode = NA_character_,
    link_transform_policy = NA_character_,
    link_transform_state = NA_character_,
    parameters = "theta_spoke_global",
    priors = list(
      anchored_joint_spoke_prior_scale = as.double(prior_sd_info$scale_mult),
      anchored_joint_sd_floor = as.double(prior_sd_info$sd_floor),
      anchored_joint_spoke_prior_fallback_sd = as.double(prior_sd_info$fallback_sd),
      prior_sd_fallback_used = any(prior_sd_info$fallback_used),
      prior_sd_fallback_items = as.character(prior_sd_info$fallback_items)
    ),
    judge = list(
      mode = as.character(judge_params$mode %||% "global_shared"),
      scope = as.character(judge_params$scope %||% "link"),
      beta = beta_val,
      epsilon = epsilon_val,
      cold_start_fallback_used = as.logical(judge_params$cold_start_fallback_used %||% FALSE)
    ),
    anchored_joint = list(
      hub_fixed = TRUE,
      free_block_dim = as.integer(length(spoke_items)),
      within_hub_edges = as.integer(nrow(hub_evidence)),
      within_spoke_edges = as.integer(nrow(spoke_evidence)),
      cross_active_edges = as.integer(sum(cross_keep)),
      full_evidence_link_domain = TRUE
    )
  )

  list(
    delta_mean = 0,
    delta_sd = NA_real_,
    log_alpha_mean = NA_real_,
    log_alpha_sd = NA_real_,
    theta_hub_post = theta_hub_fixed,
    theta_spoke_post = theta_spoke_post,
    theta_spoke_sd_post = theta_spoke_sd_post,
    fit_contract = fit_contract,
    diagnostics = list(
      converged = isTRUE(!is.null(opt) && is.list(opt) && opt$convergence == 0L),
      hessian_posdef = isTRUE(hessian_posdef),
      diagnostics_divergences_pass = NA,
      diagnostics_rhat_pass = NA,
      diagnostics_ess_pass = NA
    ),
    posterior_draws = list(
      theta_spoke = matrix(
        theta_spoke_post,
        nrow = 1L,
        dimnames = list(NULL, names(theta_spoke_post))
      )
    )
  )
}

.adaptive_link_cross_edges <- function(state, spoke_id, last_refit_step = NULL) {
  empty <- tibble::tibble(
    spoke_item = character(),
    hub_item = character(),
    y_spoke = integer(),
    step_id = integer(),
    spoke_in_A = logical(),
    run_mode = character(),
    is_probe_step = logical()
  )
  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  if (nrow(step_log) < 1L) {
    return(empty)
  }
  required <- c("pair_id", "step_id", "is_cross_set", "link_spoke_id", "A", "B", "Y", "run_mode", "is_probe_step")
  if (!all(required %in% names(step_log))) {
    return(empty)
  }
  hub_id <- as.integer(.adaptive_controller_resolve(state)$hub_id %||% 1L)
  set_by_item <- stats::setNames(as.integer(state$set_ids), as.character(state$item_ids))
  link_spoke <- as.integer(step_log$link_spoke_id)
  cross <- step_log[
    !is.na(step_log$pair_id) &
      step_log$is_cross_set %in% TRUE &
      !is.na(link_spoke) &
      link_spoke == as.integer(spoke_id),
    ,
    drop = FALSE
  ]
  if (!is.null(last_refit_step)) {
    cross <- cross[as.integer(cross$step_id) > as.integer(last_refit_step), , drop = FALSE]
  }
  if (nrow(cross) < 1L) {
    return(empty)
  }
  ids <- as.character(state$item_ids)
  A_id <- ids[as.integer(cross$A)]
  B_id <- ids[as.integer(cross$B)]
  A_set <- as.integer(set_by_item[A_id])
  B_set <- as.integer(set_by_item[B_id])
  y <- as.integer(cross$Y)
  spoke_is_A <- A_set == as.integer(spoke_id) & B_set == hub_id
  spoke_is_B <- B_set == as.integer(spoke_id) & A_set == hub_id
  keep <- spoke_is_A | spoke_is_B
  if (!any(keep)) {
    return(empty)
  }
  cross <- cross[keep, , drop = FALSE]
  A_id <- A_id[keep]
  B_id <- B_id[keep]
  y <- y[keep]
  spoke_is_A <- spoke_is_A[keep]
  tibble::tibble(
    spoke_item = ifelse(spoke_is_A, A_id, B_id),
    hub_item = ifelse(spoke_is_A, B_id, A_id),
    y_spoke = as.integer(ifelse(spoke_is_A, y, 1L - y)),
    step_id = as.integer(cross$step_id),
    spoke_in_A = as.logical(spoke_is_A),
    run_mode = as.character(cross$run_mode),
    is_probe_step = as.logical(cross$is_probe_step %||% FALSE)
  )
}

.adaptive_link_within_edges <- function(state, set_id) {
  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  if (nrow(step_log) < 1L) {
    return(tibble::tibble(A_item = character(), B_item = character(), y_A = integer(), step_id = integer()))
  }
  required <- c("pair_id", "A", "B", "Y", "set_i", "set_j", "step_id")
  if (!all(required %in% names(step_log))) {
    return(tibble::tibble(A_item = character(), B_item = character(), y_A = integer(), step_id = integer()))
  }
  rows <- step_log[
    !is.na(step_log$pair_id) &
      as.integer(step_log$set_i) == as.integer(set_id) &
      as.integer(step_log$set_j) == as.integer(set_id),
    ,
    drop = FALSE
  ]
  if (nrow(rows) < 1L) {
    return(tibble::tibble(A_item = character(), B_item = character(), y_A = integer(), step_id = integer()))
  }
  ids <- as.character(state$item_ids)
  A_item <- ids[as.integer(rows$A)]
  B_item <- ids[as.integer(rows$B)]
  y_A <- as.integer(rows$Y)
  keep <- !is.na(A_item) & !is.na(B_item) & y_A %in% c(0L, 1L)
  if (!any(keep)) {
    return(tibble::tibble(A_item = character(), B_item = character(), y_A = integer(), step_id = integer()))
  }
  tibble::tibble(
    A_item = as.character(A_item[keep]),
    B_item = as.character(B_item[keep]),
    y_A = as.integer(y_A[keep]),
    step_id = as.integer(rows$step_id[keep])
  )
}

.adaptive_link_cmdstan_file <- function() {
  path <- system.file("stan", "link_transform_refit.stan", package = "pairwiseLLM")
  if (nzchar(path)) {
    return(path)
  }
  fallback <- file.path("inst", "stan", "link_transform_refit.stan")
  if (!file.exists(fallback)) {
    rlang::abort("Stan model file for authoritative linking refit not found.")
  }
  fallback
}

.adaptive_link_cmdstan_summary_vars <- function(joint_used, estimate_hub, use_scale) {
  vars <- "delta"
  if (isTRUE(use_scale)) {
    vars <- c(vars, "log_alpha")
  }
  if (isTRUE(estimate_hub)) {
    vars <- c(vars, "theta_hub")
  }
  if (isTRUE(joint_used)) {
    vars <- c(vars, "theta_spoke")
  }
  vars
}

.adaptive_link_cmdstan_collect_diagnostics <- function(fit, variables) {
  diagnostics <- list(
    divergences = NA_integer_,
    max_rhat = NA_real_,
    min_ess_bulk = NA_real_
  )
  notes <- character()

  diag_tbl <- tryCatch(fit$diagnostic_summary(), error = function(e) NULL)
  if (!is.null(diag_tbl) && "num_divergent" %in% names(diag_tbl)) {
    divergences <- sum(diag_tbl$num_divergent, na.rm = TRUE)
    diagnostics$divergences <- if (is.finite(divergences)) as.integer(divergences) else NA_integer_
    if (!is.finite(divergences)) {
      notes <- c(notes, "Divergence count not finite.")
    }
  } else {
    notes <- c(notes, "CmdStan diagnostics missing num_divergent.")
  }

  summary_tbl <- tryCatch(
    withCallingHandlers(
      fit$summary(variables = variables),
      warning = function(w) invokeRestart("muffleWarning")
    ),
    error = function(e) NULL
  )
  if (!is.null(summary_tbl) && nrow(summary_tbl) > 0L) {
    if ("rhat" %in% names(summary_tbl)) {
      rhat_vals <- summary_tbl$rhat[is.finite(summary_tbl$rhat)]
      if (length(rhat_vals) > 0L) {
        diagnostics$max_rhat <- max(rhat_vals)
      } else {
        notes <- c(notes, "Rhat values missing or non-finite.")
      }
    } else {
      notes <- c(notes, "CmdStan summary missing rhat.")
    }
    if ("ess_bulk" %in% names(summary_tbl)) {
      ess_vals <- summary_tbl$ess_bulk[is.finite(summary_tbl$ess_bulk)]
      if (length(ess_vals) > 0L) {
        diagnostics$min_ess_bulk <- min(ess_vals)
      } else {
        notes <- c(notes, "ESS bulk values missing or non-finite.")
      }
    } else {
      notes <- c(notes, "CmdStan summary missing ess_bulk.")
    }
  } else {
    notes <- c(notes, "CmdStan summary not available.")
  }

  if (length(notes) > 0L) {
    diagnostics$notes <- notes
  }
  diagnostics
}

.adaptive_link_cmdstan_validate_diagnostics <- function(diagnostics, thresholds) {
  diagnostics <- diagnostics %||% list()
  divergences <- as.integer(diagnostics$divergences %||% NA_integer_)
  max_rhat <- as.double(diagnostics$max_rhat %||% NA_real_)
  min_ess_bulk <- as.double(diagnostics$min_ess_bulk %||% NA_real_)
  if (is.na(divergences) || !is.finite(max_rhat) || !is.finite(min_ess_bulk)) {
    details <- paste(
      c(
        paste0("divergences=", diagnostics$divergences %||% "NULL"),
        paste0("max_rhat=", diagnostics$max_rhat %||% "NULL"),
        paste0("min_ess_bulk=", diagnostics$min_ess_bulk %||% "NULL")
      ),
      collapse = ", "
    )
    rlang::abort(
      paste0(
        "Authoritative linking CmdStan diagnostics are missing or malformed. ",
        details,
        "."
      )
    )
  }
  list(
    divergences = as.integer(divergences),
    max_rhat = as.double(max_rhat),
    min_ess_bulk = as.double(min_ess_bulk),
    diagnostics_divergences_pass = as.logical(divergences <= as.integer(thresholds$divergences_max)),
    diagnostics_rhat_pass = as.logical(max_rhat <= as.double(thresholds$max_rhat)),
    diagnostics_ess_pass = as.logical(min_ess_bulk >= as.double(thresholds$min_ess_bulk))
  )
}

.adaptive_link_cmdstan_schedule <- function(attempt, n_param, joint_used = FALSE) {
  attempt <- max(1L, as.integer(attempt %||% 1L))
  n_param <- max(1L, as.integer(n_param %||% 1L))
  joint_used <- isTRUE(joint_used)

  base_warmup <- if (joint_used) 400L else 300L
  base_sampling <- if (joint_used) 500L else 400L
  warmup_mult <- c(1L, 2L, 3L)
  sampling_mult <- c(1L, 2L, 3L)
  idx <- min(attempt, length(warmup_mult))

  list(
    chains = 4L,
    iter_warmup = as.integer(base_warmup * warmup_mult[[idx]] + max(0L, n_param - 2L) * 10L),
    iter_sampling = as.integer(base_sampling * sampling_mult[[idx]] + max(0L, n_param - 2L) * 15L)
  )
}

.adaptive_link_cmdstan_draws_matrix <- function(fit, variables) {
  tryCatch(
    fit$draws(variables = variables, format = "matrix"),
    error = function(e) {
      rlang::abort(
        paste0("Authoritative linking CmdStan fit did not return draws: ", conditionMessage(e))
      )
    }
  )
}

.adaptive_link_cmdstan_output_basename <- function(output_dir) {
  basename(tempfile(pattern = "link_transform_refit-", tmpdir = output_dir))
}

.adaptive_link_fit_transform_cmdstan <- function(stan_data,
                                                 variable_names,
                                                 cmdstan,
                                                 seed,
                                                 model_fn = NULL) {
  resolved_cmdstan <- .btl_mcmc_resolve_cmdstan_config(cmdstan %||% list())
  if (is.null(model_fn)) {
    .btl_mcmc_require_cmdstanr()
    model_fn <- cmdstanr::cmdstan_model
  }
  if (!is.function(model_fn)) {
    rlang::abort("`model_fn` must be a function when provided.")
  }

  model <- model_fn(
    .adaptive_link_cmdstan_file(),
    cpp_options = list(stan_threads = TRUE)
  )

  sample_args <- list(
    data = stan_data,
    chains = as.integer(resolved_cmdstan$chains),
    iter_warmup = as.integer(cmdstan$iter_warmup),
    iter_sampling = as.integer(cmdstan$iter_sampling),
    parallel_chains = as.integer(resolved_cmdstan$parallel_chains),
    threads_per_chain = as.integer(resolved_cmdstan$threads_per_chain),
    refresh = 0,
    seed = as.integer(seed)
  )
  output_dir <- cmdstan$output_dir %||% file.path(tempdir(), "pairwiseLLM-cmdstan-link")
  if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir)) {
    rlang::abort("`cmdstan$output_dir` must be a length-1 character path.")
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  sample_args$output_dir <- output_dir
  sample_args$output_basename <- .adaptive_link_cmdstan_output_basename(output_dir)

  fit <- do.call(model$sample, sample_args)
  list(
    fit = fit,
    draws_matrix = .adaptive_link_cmdstan_draws_matrix(fit, variable_names),
    diagnostics = .adaptive_link_cmdstan_collect_diagnostics(fit, variable_names),
    mcmc_config_used = resolved_cmdstan
  )
}

.adaptive_link_refit_seed <- function(cross_edges, transform_mode, link_refit_mode) {
  edges <- tibble::as_tibble(cross_edges)
  step_id <- suppressWarnings(as.numeric(edges$step_id %||% seq_len(nrow(edges))))
  step_id[!is.finite(step_id)] <- 0
  step_id <- floor(abs(step_id))
  y <- as.integer(edges$y_spoke %||% integer())
  y[!y %in% c(0L, 1L)] <- 0L
  mode_code <- if (identical(transform_mode, "shift_scale")) 31 else 17
  refit_code <- if (identical(link_refit_mode, "joint_refit")) 53 else 19
  modulus <- as.double(.Machine$integer.max - 1L)
  acc <- 0
  for (idx in seq_along(step_id)) {
    acc <- (acc * 131 + step_id[[idx]] + as.double(y[[idx]]) * 17) %% modulus
  }
  acc <- (acc + mode_code + refit_code) %% modulus
  seed <- as.integer(acc) + 1L
  if (!is.finite(seed) || is.na(seed) || seed < 1L) {
    seed <- 1L
  }
  seed
}

.adaptive_link_fit_transform <- function(cross_edges,
                                         hub_theta,
                                         spoke_theta,
                                         transform_mode) {
  use_scale <- identical(transform_mode, "shift_scale")
  edge_attrs <- attributes(cross_edges)
  refit_contract_ctx <- edge_attrs$refit_contract %||% list()
  judge_params <- edge_attrs$judge_params %||% list(
    mode = "global_shared",
    scope = "link",
    beta = 0,
    epsilon = 0
  )
  beta <- as.double(judge_params$beta %||% 0)
  epsilon <- as.double(judge_params$epsilon %||% 0)
  if (!is.finite(beta)) {
    beta <- 0
  }
  if (!is.finite(epsilon)) {
    epsilon <- 0
  }
  epsilon <- max(0, min(1, epsilon))

  link_refit_mode <- as.character(refit_contract_ctx$link_refit_mode %||% "shift_only")
  lock_mode <- as.character(refit_contract_ctx$hub_lock_mode %||% NA_character_)
  lock_kappa <- as.double(refit_contract_ctx$hub_lock_kappa %||% NA_real_)
  diagnostics_thresholds <- refit_contract_ctx$link_diagnostics_thresholds %||% list(
    divergences_max = 0L,
    max_rhat = 1.01,
    min_ess_bulk = 400
  )
  diagnostics_thresholds$divergences_max <- as.integer(
    diagnostics_thresholds$divergences_max %||% 0L
  )
  diagnostics_thresholds$max_rhat <- as.double(diagnostics_thresholds$max_rhat %||% 1.01)
  diagnostics_thresholds$min_ess_bulk <- as.double(
    diagnostics_thresholds$min_ess_bulk %||% 400
  )
  hub_sd_map <- attr(hub_theta, "theta_sd", exact = TRUE) %||% stats::setNames(numeric(), character())
  spoke_sd_map <- attr(spoke_theta, "theta_sd", exact = TRUE) %||% stats::setNames(numeric(), character())
  edges <- tibble::as_tibble(cross_edges)

  empty_result <- function() {
    empty <- list(
      delta_mean = 0,
      delta_sd = 1,
      log_alpha_mean = if (isTRUE(use_scale)) 0 else NA_real_,
      log_alpha_sd = if (isTRUE(use_scale)) 0.2 else NA_real_
    )
    empty$fit_contract <- list(
      contract_type = "link_refit",
      estimation_method = "cmdstan_hmc",
      uncertainty_approximation = "cmdstan_posterior_draws",
      link_refit_mode = as.character(link_refit_mode),
      link_transform_policy = as.character(
        .adaptive_normalize_link_transform_policy(refit_contract_ctx$link_transform_policy %||% "auto")
      ),
      link_transform_state = as.character(transform_mode),
      parameters = if (isTRUE(use_scale)) c("delta_s", "log_alpha_s") else c("delta_s"),
      priors = list(delta_sd = 1, log_alpha_sd = if (isTRUE(use_scale)) 0.2 else NA_real_),
      judge = list(mode = as.character(judge_params$mode), scope = as.character(judge_params$scope))
    )
    empty$joint_refit <- list(
      used = FALSE,
      lock_mode = lock_mode,
      hub_lock_kappa = as.double(lock_kappa)
    )
    empty$diagnostics <- list(
      divergences = NA_integer_,
      max_rhat = NA_real_,
      min_ess_bulk = NA_real_,
      diagnostics_divergences_pass = NA,
      diagnostics_rhat_pass = NA,
      diagnostics_ess_pass = NA
    )
    empty$posterior_draws <- list(delta = numeric(), log_alpha = numeric())
    empty
  }
  if (nrow(edges) < 1L) {
    return(empty_result())
  }

  hub_theta_names <- as.character(names(hub_theta))
  spoke_theta_names <- as.character(names(spoke_theta))
  hub_ref <- as.double(hub_theta)
  spoke_ref <- as.double(spoke_theta)
  names(hub_ref) <- hub_theta_names
  names(spoke_ref) <- spoke_theta_names

  h <- as.double(hub_ref[as.character(edges$hub_item)])
  s <- as.double(spoke_ref[as.character(edges$spoke_item)])
  hub_sd <- as.double(hub_sd_map[as.character(edges$hub_item)])
  spoke_sd <- as.double(spoke_sd_map[as.character(edges$spoke_item)])
  spoke_in_A <- as.logical(edges$spoke_in_A %||% rep(TRUE, nrow(edges)))
  beta_sign <- ifelse(spoke_in_A, 1, -1)
  beta_signed <- beta * as.double(beta_sign)
  hub_sd[!is.finite(hub_sd) | hub_sd < 0] <- 0
  spoke_sd[!is.finite(spoke_sd) | spoke_sd < 0] <- 0
  y <- as.integer(edges$y_spoke)
  keep <- is.finite(h) & is.finite(s) & y %in% c(0L, 1L) & is.finite(beta_signed)
  if (!any(keep)) {
    return(empty_result())
  }
  edges_obs <- edges[keep, , drop = FALSE]
  h <- h[keep]
  s <- s[keep]
  hub_sd <- hub_sd[keep]
  spoke_sd <- spoke_sd[keep]
  beta_signed <- beta_signed[keep]
  y <- y[keep]

  hub_prior_center_raw <- attr(hub_theta, "theta_prior_center", exact = TRUE) %||% hub_ref
  hub_init_raw <- attr(hub_theta, "theta_init", exact = TRUE) %||% hub_ref
  spoke_init_raw <- attr(spoke_theta, "theta_init", exact = TRUE) %||% spoke_ref
  hub_prior_center <- as.double(hub_prior_center_raw[hub_theta_names])
  names(hub_prior_center) <- hub_theta_names
  hub_prior_center[!is.finite(hub_prior_center)] <- hub_ref[!is.finite(hub_prior_center)]
  hub_init <- as.double(hub_init_raw[hub_theta_names])
  names(hub_init) <- hub_theta_names
  hub_init[!is.finite(hub_init)] <- hub_ref[!is.finite(hub_init)]
  spoke_init <- as.double(spoke_init_raw[spoke_theta_names])
  names(spoke_init) <- spoke_theta_names
  spoke_init[!is.finite(spoke_init)] <- spoke_ref[!is.finite(spoke_init)]
  hub_ref_sd <- as.double(hub_sd_map[hub_theta_names])
  spoke_ref_sd <- as.double(spoke_sd_map[spoke_theta_names])
  hub_ref_sd[!is.finite(hub_ref_sd) | hub_ref_sd <= 0] <- 1
  spoke_ref_sd[!is.finite(spoke_ref_sd) | spoke_ref_sd <= 0] <- 1

  theta_hub_post <- hub_ref
  theta_spoke_post <- spoke_ref
  joint_used <- identical(link_refit_mode, "joint_refit")
  estimate_hub <- isTRUE(joint_used) && !identical(lock_mode, "hard_lock")
  n_hub_items_estimated <- 0L
  n_spoke_items_estimated <- 0L

  fit_hub_idx <- integer()
  fit_spoke_idx <- integer()
  within_hub <- tibble::as_tibble(edge_attrs$within_hub_edges %||% tibble::tibble())
  within_spoke <- tibble::as_tibble(edge_attrs$within_spoke_edges %||% tibble::tibble())
  if (isTRUE(joint_used)) {
    fit_hub_idx <- if (identical(lock_mode, "hard_lock")) {
      integer()
    } else {
      seq_along(hub_ref)
    }
    fit_spoke_idx <- seq_along(spoke_ref)
    n_hub_items_estimated <- as.integer(length(fit_hub_idx))
    n_spoke_items_estimated <- as.integer(length(fit_spoke_idx))
  }

  hub_lut <- stats::setNames(seq_along(hub_ref), names(hub_ref))
  spoke_lut <- stats::setNames(seq_along(spoke_ref), names(spoke_ref))
  hub_cross_idx <- as.integer(hub_lut[as.character(edges_obs$hub_item)])
  spoke_cross_idx <- as.integer(spoke_lut[as.character(edges_obs$spoke_item)])
  if (any(is.na(hub_cross_idx)) || any(is.na(spoke_cross_idx))) {
    rlang::abort("Linking authoritative CmdStan refit could not resolve cross-edge item indices.")
  }

  within_hub_a <- if ("A_item" %in% names(within_hub)) as.character(within_hub$A_item) else character()
  within_hub_b <- if ("B_item" %in% names(within_hub)) as.character(within_hub$B_item) else character()
  within_hub_y <- if ("y_A" %in% names(within_hub)) as.integer(within_hub$y_A) else integer()
  within_hub_idx_a <- as.integer(hub_lut[within_hub_a])
  within_hub_idx_b <- as.integer(hub_lut[within_hub_b])
  keep_within_hub <- !is.na(within_hub_idx_a) & !is.na(within_hub_idx_b) & within_hub_y %in% c(0L, 1L)
  within_hub_idx_a <- within_hub_idx_a[keep_within_hub]
  within_hub_idx_b <- within_hub_idx_b[keep_within_hub]
  within_hub_y <- within_hub_y[keep_within_hub]

  within_spoke_a <- if ("A_item" %in% names(within_spoke)) as.character(within_spoke$A_item) else character()
  within_spoke_b <- if ("B_item" %in% names(within_spoke)) as.character(within_spoke$B_item) else character()
  within_spoke_y <- if ("y_A" %in% names(within_spoke)) as.integer(within_spoke$y_A) else integer()
  within_spoke_idx_a <- as.integer(spoke_lut[within_spoke_a])
  within_spoke_idx_b <- as.integer(spoke_lut[within_spoke_b])
  keep_within_spoke <- !is.na(within_spoke_idx_a) & !is.na(within_spoke_idx_b) & within_spoke_y %in% c(0L, 1L)
  within_spoke_idx_a <- within_spoke_idx_a[keep_within_spoke]
  within_spoke_idx_b <- within_spoke_idx_b[keep_within_spoke]
  within_spoke_y <- within_spoke_y[keep_within_spoke]

  if (isTRUE(joint_used) && !lock_mode %in% c("hard_lock", "soft_lock")) {
    rlang::abort(
      paste0(
        "Unsupported `hub_lock_mode` in linking joint refit: ",
        lock_mode,
        ". Expected `hard_lock` or `soft_lock`."
      )
    )
  }
  hub_prior_sd <- if (isTRUE(estimate_hub) && identical(lock_mode, "soft_lock")) {
    pmax(hub_ref_sd / max(lock_kappa, 1e-8), 1e-8)
  } else {
    pmax(hub_ref_sd, 1e-8)
  }
  spoke_prior_sd <- pmax(spoke_ref_sd, 1e-8)

  stan_data_base <- list(
    N_cross = as.integer(length(y)),
    y_spoke = as.integer(y),
    hub_ref_cross = as.double(h),
    spoke_ref_cross = as.double(s),
    cross_hub_idx = as.integer(hub_cross_idx),
    cross_spoke_idx = as.integer(spoke_cross_idx),
    beta_signed = as.double(beta_signed),
    epsilon = as.double(epsilon),
    beta_within = as.double(beta),
    joint_used = as.integer(joint_used),
    estimate_hub = as.integer(estimate_hub),
    use_scale = as.integer(use_scale),
    N_hub = as.integer(length(hub_ref)),
    N_spoke = as.integer(length(spoke_ref)),
    hub_ref = as.double(hub_ref),
    spoke_ref = as.double(spoke_ref),
    hub_prior_center = as.double(hub_prior_center),
    hub_prior_sd = as.double(hub_prior_sd),
    spoke_prior_sd = as.double(spoke_prior_sd),
    N_within_hub = as.integer(length(within_hub_y)),
    hub_within_A_idx = as.integer(within_hub_idx_a),
    hub_within_B_idx = as.integer(within_hub_idx_b),
    hub_within_y_A = as.integer(within_hub_y),
    N_within_spoke = as.integer(length(within_spoke_y)),
    spoke_within_A_idx = as.integer(within_spoke_idx_a),
    spoke_within_B_idx = as.integer(within_spoke_idx_b),
    spoke_within_y_A = as.integer(within_spoke_y)
  )
  variable_names <- .adaptive_link_cmdstan_summary_vars(
    joint_used = joint_used,
    estimate_hub = estimate_hub,
    use_scale = use_scale
  )
  seed <- .adaptive_link_refit_seed(
    cross_edges = edges_obs,
    transform_mode = transform_mode,
    link_refit_mode = link_refit_mode
  )
  cmdstan_fit <- NULL
  draws_matrix <- NULL
  diagnostics <- NULL
  mcmc_config_used <- NULL
  cmdstan_schedule_used <- NULL
  cmdstan_fit_fn <- refit_contract_ctx[["cmdstan_fit_fn"]] %||% .adaptive_link_fit_transform_cmdstan
  if (!is.function(cmdstan_fit_fn)) {
    rlang::abort("`refit_contract$cmdstan_fit_fn` must be a function when provided.")
  }
  repair_attempts <- 0L
  max_attempts <- 3L
  for (attempt in seq_len(max_attempts)) {
    repair_attempts <- as.integer(attempt)
    cmdstan_schedule_used <- .adaptive_link_cmdstan_schedule(
      attempt = attempt,
      n_param = as.integer(
        1L +
          if (isTRUE(use_scale)) 1L else 0L +
          if (isTRUE(estimate_hub)) length(hub_ref) else 0L +
          if (isTRUE(joint_used)) length(spoke_ref) else 0L
      ),
      joint_used = joint_used
    )
    cmdstan_fit <- cmdstan_fit_fn(
      stan_data = stan_data_base,
      variable_names = variable_names,
      cmdstan = utils::modifyList(
        refit_contract_ctx[["cmdstan"]] %||% list(),
        list(
          chains = as.integer(cmdstan_schedule_used$chains),
          iter_warmup = as.integer(cmdstan_schedule_used$iter_warmup),
          iter_sampling = as.integer(cmdstan_schedule_used$iter_sampling)
        )
      ),
      seed = as.integer((seed + attempt * 1009L) %% .Machine$integer.max),
      model_fn = refit_contract_ctx[["cmdstan_model_fn"]] %||% NULL
    )
    draws_matrix <- as.matrix(cmdstan_fit$draws_matrix)
    diagnostics <- .adaptive_link_cmdstan_validate_diagnostics(
      diagnostics = cmdstan_fit$diagnostics,
      thresholds = diagnostics_thresholds
    )
    mcmc_config_used <- cmdstan_fit$mcmc_config_used
    if (isTRUE(diagnostics$diagnostics_rhat_pass) &&
      isTRUE(diagnostics$diagnostics_ess_pass)) {
      break
    }
  }

  if (!"delta" %in% colnames(draws_matrix)) {
    rlang::abort("Authoritative linking CmdStan output missing delta draws.")
  }
  delta_draws <- as.double(draws_matrix[, "delta", drop = TRUE])
  delta_mean <- as.double(mean(delta_draws))
  delta_sd <- as.double(stats::sd(delta_draws))
  if (!is.finite(delta_sd)) {
    delta_sd <- 0
  }
  if (isTRUE(use_scale)) {
    if (!"log_alpha" %in% colnames(draws_matrix)) {
      rlang::abort("Authoritative linking CmdStan output missing log_alpha draws.")
    }
    log_alpha_draws <- as.double(draws_matrix[, "log_alpha", drop = TRUE])
    log_alpha_mean <- as.double(mean(log_alpha_draws))
    log_alpha_sd <- as.double(stats::sd(log_alpha_draws))
    if (!is.finite(log_alpha_sd)) {
      log_alpha_sd <- 0
    }
  } else {
    log_alpha_draws <- rep(NA_real_, length(delta_draws))
    log_alpha_mean <- NA_real_
    log_alpha_sd <- NA_real_
  }

  if (isTRUE(estimate_hub) && length(fit_hub_idx) > 0L) {
    hub_cols <- paste0("theta_hub[", seq_along(fit_hub_idx), "]")
    if (!all(hub_cols %in% colnames(draws_matrix))) {
      rlang::abort("Authoritative linking CmdStan output missing theta_hub draws.")
    }
    theta_hub_post[fit_hub_idx] <- colMeans(draws_matrix[, hub_cols, drop = FALSE])
  }
  if (isTRUE(joint_used) && length(fit_spoke_idx) > 0L) {
    spoke_cols <- paste0("theta_spoke[", seq_along(fit_spoke_idx), "]")
    if (!all(spoke_cols %in% colnames(draws_matrix))) {
      rlang::abort("Authoritative linking CmdStan output missing theta_spoke draws.")
    }
    theta_spoke_post[fit_spoke_idx] <- colMeans(draws_matrix[, spoke_cols, drop = FALSE])
  }

  theta_hub_draws <- matrix(
    rep(theta_hub_post, each = nrow(draws_matrix)),
    nrow = nrow(draws_matrix),
    byrow = FALSE,
    dimnames = list(NULL, names(theta_hub_post))
  )
  theta_spoke_draws <- matrix(
    rep(theta_spoke_post, each = nrow(draws_matrix)),
    nrow = nrow(draws_matrix),
    byrow = FALSE,
    dimnames = list(NULL, names(theta_spoke_post))
  )
  if (isTRUE(estimate_hub) && length(fit_hub_idx) > 0L) {
    hub_cols <- paste0("theta_hub[", seq_along(fit_hub_idx), "]")
    theta_hub_draws[, fit_hub_idx] <- draws_matrix[, hub_cols, drop = FALSE]
  }
  if (isTRUE(joint_used) && length(fit_spoke_idx) > 0L) {
    spoke_cols <- paste0("theta_spoke[", seq_along(fit_spoke_idx), "]")
    theta_spoke_draws[, fit_spoke_idx] <- draws_matrix[, spoke_cols, drop = FALSE]
  }

  prop_var <- mean(hub_sd^2 + spoke_sd^2, na.rm = TRUE)
  if (is.finite(prop_var) && prop_var > 0) {
    delta_sd <- sqrt(delta_sd^2 + prop_var)
    if (isTRUE(use_scale)) {
      log_alpha_sd <- sqrt(log_alpha_sd^2 + 0.25 * prop_var)
    }
  }

  fit_contract <- list(
    contract_type = "link_refit",
    estimation_method = "cmdstan_hmc",
    uncertainty_approximation = "cmdstan_posterior_draws",
    link_refit_mode = as.character(link_refit_mode),
    link_transform_policy = as.character(
      .adaptive_normalize_link_transform_policy(refit_contract_ctx$link_transform_policy %||% "auto")
    ),
    link_transform_state = as.character(transform_mode),
    parameters = if (isTRUE(joint_used)) {
      if (isTRUE(use_scale)) {
        c("theta_hub", "theta_spoke", "delta_s", "log_alpha_s")
      } else {
        c("theta_hub", "theta_spoke", "delta_s")
      }
    } else {
      if (isTRUE(use_scale)) c("delta_s", "log_alpha_s") else c("delta_s")
    },
    priors = list(delta_sd = 1, log_alpha_sd = if (isTRUE(use_scale)) 0.2 else NA_real_),
    judge = list(
      mode = as.character(judge_params$mode %||% "global_shared"),
      scope = as.character(judge_params$scope %||% "link"),
      beta = as.double(beta),
      epsilon = as.double(epsilon),
      cold_start_fallback_used = as.logical(judge_params$cold_start_fallback_used %||% FALSE)
    ),
    lock = list(
      hub_lock_mode = as.character(lock_mode),
      hub_lock_kappa = as.double(lock_kappa)
    ),
    theta_treatment = as.character(refit_contract_ctx$shift_only_theta_treatment %||% NA_character_),
    joint_refit = list(
      used = as.logical(joint_used),
      n_hub_items_estimated = as.integer(n_hub_items_estimated),
      n_spoke_items_estimated = as.integer(n_spoke_items_estimated)
    ),
    diagnostics = list(
      max_rhat = as.double(diagnostics$max_rhat %||% NA_real_),
      min_ess_bulk = as.double(diagnostics$min_ess_bulk %||% NA_real_),
      divergences = as.integer(diagnostics$divergences %||% NA_integer_)
    ),
    mcmc = list(
      chains = as.integer(mcmc_config_used$chains %||% NA_integer_),
      parallel_chains = as.integer(mcmc_config_used$parallel_chains %||% NA_integer_),
      warmup = as.integer(cmdstan_schedule_used$iter_warmup %||% NA_integer_),
      samples = as.integer(cmdstan_schedule_used$iter_sampling %||% NA_integer_),
      threads_per_chain = as.integer(mcmc_config_used$threads_per_chain %||% NA_integer_),
      cmdstanr_version = as.character(mcmc_config_used$cmdstanr_version %||% NA_character_),
      repair_attempts = as.integer(repair_attempts)
    )
  )

  list(
    delta_mean = as.double(delta_mean),
    delta_sd = as.double(delta_sd),
    log_alpha_mean = if (isTRUE(use_scale)) as.double(log_alpha_mean) else NA_real_,
    log_alpha_sd = as.double(log_alpha_sd),
    fit_contract = fit_contract,
    theta_hub_post = theta_hub_post,
    theta_spoke_post = theta_spoke_post,
    posterior_draws = list(
      delta = as.double(delta_draws),
      log_alpha = as.double(log_alpha_draws),
      theta_hub = theta_hub_draws,
      theta_spoke = theta_spoke_draws
    ),
    diagnostics = diagnostics
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

.adaptive_link_fit_transform_alt_shift_scale <- function(cross_edges,
                                                         hub_theta,
                                                         spoke_theta,
                                                         delta_init = 0) {
  edges <- tibble::as_tibble(cross_edges)
  judge_params <- attr(edges, "judge_params", exact = TRUE) %||% list(beta = 0, epsilon = 0)
  if (nrow(edges) < 1L) {
    return(list(
      converged = FALSE,
      delta_mean = NA_real_,
      log_alpha_mean = NA_real_,
      log_alpha_sd = NA_real_
    ))
  }
  beta <- as.double(judge_params$beta %||% 0)
  epsilon <- max(0, min(1, as.double(judge_params$epsilon %||% 0)))
  h <- as.double(hub_theta[as.character(edges$hub_item)])
  s <- as.double(spoke_theta[as.character(edges$spoke_item)])
  y <- as.integer(edges$y_spoke)
  spoke_in_A <- as.logical(edges$spoke_in_A %||% TRUE)
  beta_signed <- ifelse(spoke_in_A, beta, -beta)
  keep <- is.finite(h) & is.finite(s) & y %in% c(0L, 1L)
  if (!any(keep)) {
    return(list(
      converged = FALSE,
      delta_mean = NA_real_,
      log_alpha_mean = NA_real_,
      log_alpha_sd = NA_real_
    ))
  }
  h <- h[keep]
  s <- s[keep]
  y <- y[keep]
  beta_signed <- beta_signed[keep]
  neg_log_post <- function(par) {
    delta <- par[[1L]]
    log_alpha <- par[[2L]]
    alpha <- exp(log_alpha)
    eta <- delta + alpha * s - h + beta_signed
    p <- (1 - epsilon) * stats::plogis(eta) + epsilon * 0.5
    p <- pmax(1e-10, pmin(1 - 1e-10, p))
    -sum(stats::dbinom(y, size = 1L, prob = p, log = TRUE)) +
      0.5 * (delta / 1)^2 +
      0.5 * (log_alpha / 0.2)^2
  }
  opt <- tryCatch(
    stats::optim(
      par = c(as.double(delta_init), 0),
      fn = neg_log_post,
      method = "BFGS",
      hessian = TRUE,
      control = list(maxit = 200, reltol = 1e-10)
    ),
    error = function(e) NULL
  )
  if (is.null(opt) || !is.list(opt) || opt$convergence != 0L || !all(is.finite(opt$par))) {
    return(list(
      converged = FALSE,
      delta_mean = NA_real_,
      log_alpha_mean = NA_real_,
      log_alpha_sd = NA_real_
    ))
  }
  hessian <- opt$hessian %||% matrix(NA_real_, nrow = 2L, ncol = 2L)
  vcov <- tryCatch(solve(hessian), error = function(e) matrix(NA_real_, nrow = 2L, ncol = 2L))
  log_alpha_sd <- if (all(is.finite(vcov)) && vcov[2L, 2L] >= 0) sqrt(vcov[2L, 2L]) else NA_real_
  list(
    converged = is.finite(log_alpha_sd),
    delta_mean = as.double(opt$par[[1L]]),
    log_alpha_mean = as.double(opt$par[[2L]]),
    log_alpha_sd = as.double(log_alpha_sd),
    fit_method = "map_laplace_hessian",
    uncertainty_approximation = "laplace_hessian"
  )
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

  if (total_pairs <= floor_pairs * length(active_keys)) {
    ord <- order(active_keys)
    cursor <- 1L
    while (sum(out) < total_pairs) {
      key <- active_keys[[ord[[cursor]]]]
      cap <- candidate_count[[key]]
      if (is.na(cap) || out[[key]] < cap) {
        out[[key]] <- out[[key]] + 1L
      }
      cursor <- if (cursor >= length(ord)) 1L else cursor + 1L
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
          for (idx in seq_len(unused)) {
            receiver <- receivers[[ord[[(idx - 1L) %% length(ord) + 1L]]]]
            cap <- candidate_count[[receiver]]
            if (is.na(cap) || out[[receiver]] < cap) {
              out[[receiver]] <- out[[receiver]] + 1L
            }
          }
          redistribute <- TRUE
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
    link_stop_reliability_min_used = rep(NA_real_, nrow(rows)),
    probe_brier_max_used = rep(NA_real_, nrow(rows)),
    probe_brier_pass = rep(NA, nrow(rows)),
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
  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  last_refit_step <- as.integer(state$refit_meta$last_refit_step %||% 0L)
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
        obs <- 0L
        if (nrow(step_log) > 0L &&
          all(c("pair_id", "is_cross_set", "link_spoke_id", "step_id") %in% names(step_log))) {
          obs <- as.integer(sum(
            !is.na(step_log$pair_id) &
              step_log$is_cross_set %in% TRUE &
              as.integer(step_log$link_spoke_id) == as.integer(key) &
              as.integer(step_log$step_id) > last_refit_step,
            na.rm = TRUE
          ))
        }
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
    obs <- 0L
    if (nrow(step_log) > 0L &&
      all(c("pair_id", "is_cross_set", "link_spoke_id", "step_id") %in% names(step_log))) {
      obs <- as.integer(sum(
        !is.na(step_log$pair_id) &
          step_log$is_cross_set %in% TRUE &
          as.integer(step_log$link_spoke_id) == as.integer(key) &
          as.integer(step_log$step_id) > last_refit_step,
        na.rm = TRUE
      ))
    }
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
  out <- state
  controller <- .adaptive_controller_resolve(out)
  run_mode <- as.character(controller$run_mode %||% "within_set")
  if (!run_mode %in% c("link_one_spoke", "link_multi_spoke")) {
    return(out)
  }
  phase_ctx <- .adaptive_link_phase_context(out, controller = controller)
  if (!identical(phase_ctx$phase, "phase_b")) {
    return(out)
  }
  if (length(phase_ctx$active_spokes %||% integer()) < 1L) {
    return(out)
  }
  hub_id <- as.integer(controller$hub_id %||% 1L)
  spoke_ids <- .adaptive_link_spoke_ids(out, hub_id)
  spoke_ids <- intersect(spoke_ids, as.integer(phase_ctx$active_spokes))
  if (length(spoke_ids) < 1L) {
    return(out)
  }
  link_stats <- controller$link_refit_stats_by_spoke %||% list()
  state_map <- controller$link_transform_state_by_spoke %||% list()
  last_delta <- controller$link_transform_last_delta_by_spoke %||% list()
  last_log_alpha <- controller$link_transform_last_log_alpha_by_spoke %||% list()
  frozen_map <- controller$link_transform_frozen_by_spoke %||% list()
  frozen_delta_map <- controller$link_transform_frozen_delta_by_spoke %||% list()
  frozen_log_alpha_map <- controller$link_transform_frozen_log_alpha_by_spoke %||% list()
  spoke_ids <- spoke_ids[!vapply(
    as.character(spoke_ids),
    function(key) isTRUE(frozen_map[[key]]) && !is.null(link_stats[[key]]),
    logical(1L)
  )]
  if (length(spoke_ids) < 1L) {
    return(out)
  }
  link_identified_map <- controller$linking_identified_by_spoke %||% list()
  stop_window_map <- controller$link_stop_recent_pass_window_by_spoke %||% list()
  escalation_window_map <- controller$link_escalation_recent_pass_window_by_spoke %||% list()
  epoch_id_map <- controller$link_epoch_id_by_spoke %||% list()
  epoch_signature_map <- controller$link_epoch_signature_by_spoke %||% list()
  epoch_start_step_map <- controller$link_epoch_start_step_by_spoke %||% list()
  coverage_bins_map <- controller$link_stage_coverage_bins_used %||% list()
  coverage_source_map <- controller$link_stage_coverage_source %||% list()
  lag_domain_key_map <- controller$link_lag_domain_key_by_spoke %||% list()
  lag_domain_reset_refit_map <- controller$link_lag_domain_reset_refit_id_by_spoke %||% list()
  last_step <- as.integer(refit_context$last_refit_step %||% 0L)
  current_refit_id <- as.integer(nrow(out$round_log) + 1L)
  link_stage_hist <- tibble::as_tibble(out$link_stage_log %||% new_link_stage_log())

  for (spoke_id in spoke_ids) {
    key <- as.character(spoke_id)
    epoch_start_step_map[[key]] <- as.integer(
      epoch_start_step_map[[key]] %||% .adaptive_link_epoch_start_step_default(out, spoke_id)
    )
    link_estimation_mode <- as.character(controller$link_estimation_mode %||% "transform")
    transform_frozen <- isTRUE(frozen_map[[key]])
    if (identical(link_estimation_mode, "anchored_joint")) {
      transform_policy <- NA_character_
      transform_state <- NA_character_
      refit_mode <- NA_character_
      lock_mode <- "hard_lock"
      kappa <- NA_real_
      theta_treatment <- NA_character_
      theta_treatment_resolved <- NA_character_
    } else {
      transform_policy <- .adaptive_normalize_link_transform_policy(
        controller$link_transform_policy %||% "auto"
      )
      transform_state <- .adaptive_link_transform_state_for_spoke(controller, spoke_id)
      refit_mode <- as.character(controller$link_refit_mode %||% "shift_only")
      lock_mode <- as.character(controller$hub_lock_mode %||% "soft_lock")
      kappa <- as.double(controller$hub_lock_kappa %||% 0.75)
      theta_treatment <- as.character(controller$shift_only_theta_treatment %||% "fixed_eap_plugin_var")
      theta_treatment_resolved <- theta_treatment
    }

    hub_phase <- .adaptive_link_phase_a_theta_map(out, hub_id, "theta_raw_mean")
    hub_phase_sd <- .adaptive_link_phase_a_theta_map(out, hub_id, "theta_raw_sd")
    spoke_phase <- .adaptive_link_phase_a_theta_map(out, spoke_id, "theta_raw_mean")
    spoke_phase_sd <- .adaptive_link_phase_a_theta_map(out, spoke_id, "theta_raw_sd")
    hub_current <- .adaptive_link_theta_mean_map(out, hub_id)
    hub_current_sd <- .adaptive_link_theta_sd_map(out, hub_id)
    spoke_current <- .adaptive_link_theta_mean_map(out, spoke_id)
    spoke_current_sd <- .adaptive_link_theta_sd_map(out, spoke_id)

    accepted_state_current <- NULL
    if (identical(link_estimation_mode, "anchored_joint")) {
      accepted_state_current <- .adaptive_link_anchored_joint_resolve_state(
        state = out,
        spoke_id = as.integer(spoke_id),
        controller = controller
      )
      hub_theta <- accepted_state_current$theta_hub_fixed
      hub_theta_sd <- .adaptive_phase_a_artifact_item_field_map(out, hub_id, "theta_raw_sd")
      hub_theta_sd[!is.finite(hub_theta_sd) | hub_theta_sd < 0] <- 0
      spoke_theta <- accepted_state_current$theta_spoke_global_mean
      spoke_theta_sd <- accepted_state_current$theta_spoke_global_sd
      spoke_theta_sd[!is.finite(spoke_theta_sd) | spoke_theta_sd < 0] <- 0
    } else if (identical(refit_mode, "joint_refit")) {
      if (identical(lock_mode, "hard_lock")) {
        hub_theta <- hub_phase
        hub_theta_sd <- stats::setNames(rep(0, length(hub_theta)), names(hub_theta))
      } else if (identical(lock_mode, "soft_lock")) {
        hub_theta <- hub_phase
        hub_theta_sd <- hub_phase_sd
      } else {
        hub_theta <- if (length(hub_current) > 0L) hub_current else hub_phase
        hub_theta_sd <- if (length(hub_current_sd) > 0L) hub_current_sd else hub_phase_sd
      }
      spoke_theta <- if (length(spoke_current) > 0L) spoke_current else spoke_phase
      spoke_theta_sd <- if (length(spoke_current_sd) > 0L) spoke_current_sd else spoke_phase_sd
    } else {
      hub_theta <- hub_phase
      hub_theta_sd <- hub_phase_sd
      spoke_theta <- spoke_phase
      spoke_theta_sd <- spoke_phase_sd
      if (identical(theta_treatment, "fixed_eap_plugin_var")) {
        hub_has_sd <- length(hub_theta_sd) > 0L && any(is.finite(hub_theta_sd))
        spoke_has_sd <- length(spoke_theta_sd) > 0L && any(is.finite(spoke_theta_sd))
        if (!isTRUE(hub_has_sd) || !isTRUE(spoke_has_sd)) {
          theta_treatment_resolved <- "fixed_eap"
          hub_theta_sd <- stats::setNames(rep(0, length(hub_theta)), names(hub_theta))
          spoke_theta_sd <- stats::setNames(rep(0, length(spoke_theta)), names(spoke_theta))
        }
      }
    }

    btl_config <- out$config$btl_config %||% list()
    cross_all <- .adaptive_link_cross_edges(out, spoke_id = spoke_id, last_refit_step = NULL)
    cross_since <- .adaptive_link_cross_edges(out, spoke_id = spoke_id, last_refit_step = last_step)
    startup_gap <- .adaptive_link_phase_b_startup_gap_for_spoke(out, spoke_id = spoke_id)
    judge_params <- .adaptive_link_judge_params(
      out,
      controller,
      scope = "link",
      allow_cold_start_fallback = isTRUE(startup_gap),
      expected_link_params = !isTRUE(startup_gap)
    )
    attr(cross_all, "judge_params") <- judge_params
    attr(cross_since, "judge_params") <- judge_params
    attr(cross_all, "within_hub_edges") <- .adaptive_link_within_edges(out, set_id = hub_id)
    attr(cross_all, "within_spoke_edges") <- .adaptive_link_within_edges(out, set_id = spoke_id)
    attr(cross_all, "refit_contract") <- list(
      link_refit_mode = refit_mode,
      link_transform_policy = transform_policy,
      hub_lock_mode = lock_mode,
      hub_lock_kappa = kappa,
      shift_only_theta_treatment = theta_treatment,
      cmdstan = btl_config[["cmdstan"]] %||% list(),
      cmdstan_fit_fn = btl_config[["cmdstan_fit_fn"]] %||% NULL,
      cmdstan_model_fn = btl_config[["cmdstan_model_fn"]] %||% NULL,
      link_diagnostics_thresholds = list(
        divergences_max = as.integer(btl_config$divergences_max %||% 0L),
        max_rhat = as.double(btl_config$max_rhat %||% 1.01),
        min_ess_bulk = as.double(btl_config$ess_bulk_min %||% 400)
      )
    )
    hub_theta_init <- if (identical(refit_mode, "joint_refit") && length(hub_current) > 0L) {
      hub_current
    } else {
      hub_theta
    }
    spoke_theta_init <- if (identical(refit_mode, "joint_refit") && length(spoke_current) > 0L) {
      spoke_current
    } else {
      spoke_theta
    }
    hub_theta_prior_center <- if (identical(refit_mode, "joint_refit") &&
      identical(lock_mode, "soft_lock")) {
      hub_phase
    } else {
      hub_theta
    }
    attr(hub_theta, "theta_sd") <- hub_theta_sd
    attr(hub_theta, "theta_init") <- hub_theta_init
    attr(hub_theta, "theta_prior_center") <- hub_theta_prior_center
    attr(spoke_theta, "theta_sd") <- spoke_theta_sd
    attr(spoke_theta, "theta_init") <- spoke_theta_init
    cross_active_all <- cross_all[!(cross_all$is_probe_step %in% TRUE), , drop = FALSE]
    fit <- if (identical(link_estimation_mode, "anchored_joint")) {
      if (isTRUE(transform_frozen)) {
        list(
          delta_mean = 0,
          delta_sd = NA_real_,
          log_alpha_mean = NA_real_,
          log_alpha_sd = NA_real_,
          theta_hub_post = accepted_state_current$theta_hub_fixed,
          theta_spoke_post = accepted_state_current$theta_spoke_global_mean,
          theta_spoke_sd_post = accepted_state_current$theta_spoke_global_sd,
          posterior_draws = list(),
          diagnostics = list(),
          fit_contract = list(
            contract_type = "link_refit_frozen_reuse",
            estimation_method = "accepted_state_reuse",
            uncertainty_approximation = "accepted_state"
          )
        )
      } else {
        .adaptive_link_fit_anchored_joint(
          state = out,
          spoke_id = as.integer(spoke_id),
          controller = controller,
          cross_edges = cross_active_all,
          judge_params = judge_params,
          accepted_state = accepted_state_current
        )
      }
    } else if (isTRUE(transform_frozen)) {
      list(
        delta_mean = as.double(frozen_delta_map[[key]] %||% last_delta[[key]] %||% 0),
        delta_sd = as.double((link_stats[[key]] %||% list())$delta_spoke_sd %||% 0),
        log_alpha_mean = as.double(frozen_log_alpha_map[[key]] %||% last_log_alpha[[key]] %||% NA_real_),
        log_alpha_sd = as.double((link_stats[[key]] %||% list())$log_alpha_spoke_sd %||% NA_real_),
        theta_hub_post = hub_theta,
        theta_spoke_post = spoke_theta,
        posterior_draws = list(),
        diagnostics = list(),
        fit_contract = list(contract_type = "link_refit_frozen_reuse")
      )
    } else {
      .adaptive_link_fit_transform(cross_active_all, hub_theta, spoke_theta, transform_mode = transform_state)
    }
    if (identical(link_estimation_mode, "anchored_joint")) {
      accepted_state_current <- .adaptive_anchored_joint_new_accepted_state(
        state = out,
        hub_id = hub_id,
        spoke_id = as.integer(spoke_id),
        theta_hub_fixed = fit$theta_hub_post %||% hub_theta,
        theta_spoke_global_mean = fit$theta_spoke_post %||% spoke_theta,
        theta_spoke_global_sd = fit$theta_spoke_sd_post %||% spoke_theta_sd,
        judge_params = judge_params,
        anchored_joint_init_state_method = if (nrow(cross_active_all) < 1L) {
          "phase_a_only_init_refit"
        } else {
          "phase_b_refit"
        },
        phase_a_evidence_hash_hub = .adaptive_phase_a_hash_object(
          .adaptive_phase_a_artifact_resolve_within_set_evidence(
            artifact = out$linking$phase_a$artifacts[[as.character(hub_id)]],
            state = out,
            set_id = hub_id,
            controller = controller
          )
        ),
        phase_a_evidence_hash_spoke = .adaptive_phase_a_hash_object(
          .adaptive_phase_a_artifact_resolve_within_set_evidence(
            artifact = out$linking$phase_a$artifacts[[as.character(spoke_id)]],
            state = out,
            set_id = as.integer(spoke_id),
            controller = controller
          )
        )
      )
      out$linking$anchored_joint$accepted_state_by_spoke[[key]] <- accepted_state_current
      out$linking$anchored_joint$fisher_t0_by_spoke[[key]] <- list(
        free_block_dim = as.integer(length(accepted_state_current$theta_spoke_global_mean)),
        I_s_t0_zero = TRUE,
        n_link_active_pairs = 0L,
        anchored_joint_init_state_method = as.character(
          accepted_state_current$anchored_joint_init_state_method
        )
      )
    }
    ppc_hub_theta <- fit$theta_hub_post %||% hub_theta
    ppc_spoke_theta <- if (identical(link_estimation_mode, "anchored_joint")) {
      fit$theta_spoke_post %||% accepted_state_current$theta_spoke_global_mean
    } else {
      fit$theta_spoke_post %||% spoke_theta
    }
    probe_holdout_flag <- .adaptive_link_is_holdout_probe_rows(cross_since)
    cross_since_probe <- cross_since[
      probe_holdout_flag,
      ,
      drop = FALSE
    ]
    cross_since_active <- cross_since[
      !probe_holdout_flag,
      ,
      drop = FALSE
    ]
    ppc_brier_cross_active <- .adaptive_link_ppc_brier_cross(
      cross_since_active,
      hub_theta = ppc_hub_theta,
      spoke_theta = ppc_spoke_theta,
      delta_mean = fit$delta_mean,
      log_alpha_mean = fit$log_alpha_mean,
      posterior_draws = fit$posterior_draws
    )
    ppc_brier_cross_probe <- .adaptive_link_ppc_brier_cross(
      cross_since_probe,
      hub_theta = ppc_hub_theta,
      spoke_theta = ppc_spoke_theta,
      delta_mean = fit$delta_mean,
      log_alpha_mean = fit$log_alpha_mean,
      posterior_draws = fit$posterior_draws
    )
    ppc_brier_cross <- .adaptive_link_ppc_brier_cross(
      cross_since,
      hub_theta = ppc_hub_theta,
      spoke_theta = ppc_spoke_theta,
      delta_mean = fit$delta_mean,
      log_alpha_mean = fit$log_alpha_mean,
      posterior_draws = fit$posterior_draws
    )

    hub_art <- out$linking$phase_a$artifacts[[as.character(hub_id)]] %||% list()
    spoke_art <- out$linking$phase_a$artifacts[[as.character(spoke_id)]] %||% list()
    epoch_signature_components <- .adaptive_link_epoch_signature_components(
      transform_state = transform_state,
      refit_mode = refit_mode,
      lock_mode = lock_mode,
      hub_art = hub_art,
      spoke_art = spoke_art,
      link_estimation_mode = link_estimation_mode
    )
    epoch_signature <- .adaptive_link_epoch_signature_string(epoch_signature_components)
    previous_signature <- as.character(epoch_signature_map[[key]] %||% NA_character_)
    link_epoch_id <- as.integer(epoch_id_map[[key]] %||% 1L)
    lag_domain_reset_reason <- .adaptive_link_epoch_reset_reason(
      previous_signature = previous_signature,
      current_components = epoch_signature_components
    )
    lag_domain_reset <- !is.na(lag_domain_reset_reason)
    if (isTRUE(lag_domain_reset)) {
      link_epoch_id <- as.integer(link_epoch_id + 1L)
      stop_window_map[[key]] <- logical()
      escalation_window_map[[key]] <- logical()
      lag_domain_reset_refit_map[[key]] <- as.integer(current_refit_id)
      epoch_start_step_map[[key]] <- as.integer(last_step + 1L)
    }
    epoch_id_map[[key]] <- as.integer(link_epoch_id)
    epoch_signature_map[[key]] <- as.character(epoch_signature)
    lag_domain_key <- as.character(epoch_signature)
    lag_domain_key_map[[key]] <- as.character(lag_domain_key)
    epoch_start_step <- as.integer(
      epoch_start_step_map[[key]] %||% .adaptive_link_epoch_start_step_default(out, spoke_id)
    )
    out$controller <- controller
    out$controller$link_epoch_id_by_spoke <- epoch_id_map
    panel_eval <- .adaptive_link_probe_panel_for_spoke(
      out,
      spoke_id = spoke_id,
      epoch_id = link_epoch_id
    )
    if (nrow(panel_eval) < 1L) {
      tmp_state <- out
      tmp_controller <- out$controller
      panel_eval <- .adaptive_link_probe_construct_panel(
        state = tmp_state,
        controller = tmp_controller,
        spoke_id = as.integer(spoke_id)
      )
      panel_eval <- tibble::as_tibble(panel_eval)
      if (nrow(panel_eval) < 1L) {
        rlang::abort(
          paste0(
            "Phase B probe-panel invariant failed: no held-out panel could be constructed for spoke_id=",
            as.integer(spoke_id),
            " in link_epoch_id=",
            as.integer(link_epoch_id),
            "."
          )
        )
      }
      out$linking <- out$linking %||% list()
      probe_state <- .adaptive_link_probe_state(out)
      probe_state$panels_by_spoke[[key]] <- panel_eval
      out$linking$probe <- probe_state
    }
    prior_panel_row <- link_stage_hist[
      as.integer(link_stage_hist$spoke_id) == as.integer(spoke_id),
      ,
      drop = FALSE
    ]
    if (nrow(prior_panel_row) > 0L) {
      prior_panel_row <- prior_panel_row[
        order(as.integer(prior_panel_row$refit_id), seq_len(nrow(prior_panel_row))),
        ,
        drop = FALSE
      ]
      prior_panel_row <- prior_panel_row[nrow(prior_panel_row), , drop = FALSE]
    }
    prior_panel_epoch <- if (nrow(prior_panel_row) > 0L) {
      as.integer(prior_panel_row$link_epoch_id[[1L]] %||% NA_integer_)
    } else {
      NA_integer_
    }
    prior_panel_id <- if (nrow(prior_panel_row) > 0L) {
      as.character(prior_panel_row$probe_panel_id[[1L]] %||% NA_character_)
    } else {
      NA_character_
    }
    current_panel_id <- as.character(panel_eval$probe_panel_id[[1L]] %||% NA_character_)
    same_epoch_panel_changed <- nrow(prior_panel_row) > 0L &&
      is.finite(prior_panel_epoch) &&
      identical(prior_panel_epoch, as.integer(link_epoch_id)) &&
      is.character(prior_panel_id) &&
      nzchar(prior_panel_id) &&
      is.character(current_panel_id) &&
      nzchar(current_panel_id) &&
      !identical(prior_panel_id, current_panel_id)
    if (isTRUE(same_epoch_panel_changed)) {
      if (.adaptive_is_resumed_session(out)) {
        .adaptive_link_probe_resume_abort(
          paste0(
            "persisted/current probe panel id ",
            current_panel_id,
            " disagrees with canonical `link_stage_log$probe_panel_id` ",
            prior_panel_id,
            " in the same link_epoch_id=",
            as.integer(link_epoch_id),
            "; refusing to rebuild the panel mid-epoch"
          ),
          spoke_id = spoke_id
        )
      }
      link_epoch_id <- as.integer(link_epoch_id + 1L)
      epoch_id_map[[key]] <- as.integer(link_epoch_id)
      stop_window_map[[key]] <- logical()
      escalation_window_map[[key]] <- logical()
      lag_domain_reset <- TRUE
      lag_domain_reset_reason <- "probe_panel_rebuild"
      lag_domain_reset_refit_map[[key]] <- as.integer(current_refit_id)
      epoch_start_step_map[[key]] <- as.integer(last_step + 1L)
      out$controller$link_epoch_id_by_spoke <- epoch_id_map
      out$linking <- out$linking %||% list()
      probe_state <- .adaptive_link_probe_state(out)
      rebuilt_panel <- .adaptive_link_probe_construct_panel(
        state = out,
        controller = out$controller,
        spoke_id = as.integer(spoke_id)
      )
      rebuilt_panel <- tibble::as_tibble(rebuilt_panel)
      if (nrow(rebuilt_panel) < 1L) {
        rlang::abort(
          paste0(
            "Phase B probe-panel invariant failed: no held-out panel could be constructed for spoke_id=",
            as.integer(spoke_id),
            " after probe-panel rebuild reset in link_epoch_id=",
            as.integer(link_epoch_id),
            "."
          )
        )
      }
      probe_state$panels_by_spoke[[key]] <- rebuilt_panel
      out$linking$probe <- probe_state
      panel_eval <- rebuilt_panel
      current_panel_id <- as.character(panel_eval$probe_panel_id[[1L]] %||% NA_character_)
    }
    probe_panel_id_eval <- as.character(panel_eval$probe_panel_id[[1L]] %||% NA_character_)
    probe_edges_planned_eval <- as.integer(nrow(panel_eval))
    probe_edges_realized_eval <- .adaptive_link_probe_realized_count(
      out,
      spoke_id = as.integer(spoke_id),
      epoch_id = as.integer(link_epoch_id)
    )
    probe_panel_shortfall_eval <- as.integer(
      max(0L, probe_edges_planned_eval - probe_edges_realized_eval)
    )
    eval_link_epoch_id <- as.integer(link_epoch_id)

    lag <- as.integer(out$config$btl_config$stability_lag %||% 2L)
    lag_eligible <- !isTRUE(lag_domain_reset) && !is.na(lag) && lag >= 1L && current_refit_id > lag
    lag_refit_id <- if (isTRUE(lag_eligible)) as.integer(current_refit_id - lag) else NA_integer_
    lag_row <- tibble::tibble()
    if (isTRUE(lag_eligible) && nrow(link_stage_hist) > 0L) {
      lag_row <- link_stage_hist[
        as.integer(link_stage_hist$refit_id) == lag_refit_id &
          as.integer(link_stage_hist$spoke_id) == as.integer(spoke_id) &
          as.integer(link_stage_hist$link_epoch_id %||% NA_integer_) == as.integer(link_epoch_id),
        ,
        drop = FALSE
      ]
    }
    lag_eligible <- isTRUE(lag_eligible) && nrow(lag_row) == 1L
    lag_delta <- if (nrow(lag_row) > 0L) as.double(lag_row$delta_spoke_mean[[1L]]) else NA_real_
    lag_log_alpha <- if (nrow(lag_row) > 0L) as.double(lag_row$log_alpha_spoke_mean[[1L]]) else NA_real_
    lag_global_theta <- if (identical(link_estimation_mode, "anchored_joint") && isTRUE(lag_eligible)) {
      .adaptive_phase_b_global_theta_history_at_refit(out, refit_id = lag_refit_id)
    } else {
      NULL
    }
    delta_change <- if (is.finite(lag_delta)) abs(fit$delta_mean - lag_delta) else NA_real_
    log_alpha_change <- if (is.finite(lag_log_alpha) && is.finite(fit$log_alpha_mean)) {
      abs(fit$log_alpha_mean - lag_log_alpha)
    } else {
      NA_real_
    }
    if (identical(transform_state, "shift_scale") && !is.finite(fit$log_alpha_mean)) {
      fit$log_alpha_mean <- 0
    }
    if (identical(transform_state, "shift_scale") && !is.finite(fit$log_alpha_sd)) {
      fit$log_alpha_sd <- 0.2
    }
    if (identical(transform_state, "shift_only")) {
      fit$log_alpha_mean <- NA_real_
      fit$log_alpha_sd <- NA_real_
      log_alpha_change <- NA_real_
    }

    active <- .adaptive_link_active_item_ids(out, spoke_id = spoke_id, hub_id = hub_id)
    reliability_stats <- .adaptive_link_global_score_stats_active(
      state = out,
      active_ids = active$active_all,
      spoke_id = spoke_id,
      hub_id = hub_id,
      transform_mode = transform_state,
      delta_mean = fit$delta_mean,
      log_alpha_mean = fit$log_alpha_mean,
      fit = fit,
      refit_mode = refit_mode,
      hub_lock_mode = lock_mode,
      shift_only_theta_treatment = theta_treatment_resolved,
      link_estimation_mode = link_estimation_mode,
      accepted_state = accepted_state_current,
      var_mu_epsilon = as.double(controller$reliability_var_mu_epsilon %||% 1e-6),
      total_var_epsilon = as.double(controller$reliability_total_var_epsilon %||% 1e-6)
    )
    reliability_active <- .adaptive_link_reliability_transformed_active(
      state = out,
      active_ids = active$active_all,
      spoke_id = spoke_id,
      hub_id = hub_id,
      transform_mode = transform_state,
      delta_mean = fit$delta_mean,
      log_alpha_mean = fit$log_alpha_mean,
      fit = fit,
      refit_mode = refit_mode,
      hub_lock_mode = lock_mode,
      shift_only_theta_treatment = theta_treatment_resolved,
      link_estimation_mode = link_estimation_mode,
      accepted_state = accepted_state_current,
      var_mu_epsilon = as.double(controller$reliability_var_mu_epsilon %||% 1e-6),
      total_var_epsilon = as.double(controller$reliability_total_var_epsilon %||% 1e-6)
    )
    if (!is.finite(reliability_active)) {
      reliability_active <- as.double(reliability_stats$reliability %||% NA_real_)
    }
    theta_mean_transformed <- if (identical(link_estimation_mode, "anchored_joint")) {
      .adaptive_link_anchored_joint_global_theta_map(
        state = out,
        spoke_id = as.integer(spoke_id),
        controller = controller,
        accepted_state = accepted_state_current
      )
    } else {
      .adaptive_link_transform_theta_mean_for_spoke(
        state = out,
        theta_mean = .adaptive_btl_fit_theta_mean(out$btl_fit %||% list()),
        spoke_id = spoke_id,
        hub_id = hub_id,
        transform_mode = transform_state,
        delta_mean = fit$delta_mean,
        log_alpha_mean = fit$log_alpha_mean
      )
    }
    ts_btl_rank_active <- .adaptive_link_ts_btl_rank_spearman_active(
      state = out,
      active_ids = active$active_all,
      theta_mean = theta_mean_transformed
    )
    rank_stability <- if (identical(link_estimation_mode, "anchored_joint")) {
      list(
        lag_eligible = FALSE,
        rho_rank_lagged = NA_real_,
        rho_rank_lagged_pass = FALSE
      )
    } else {
      .adaptive_link_rank_stability_lagged(
        state = out,
        active_ids = active$active_all,
        stability_lag = lag,
        spoke_id = spoke_id,
        hub_id = hub_id,
        transform_mode = transform_state,
        delta_mean = fit$delta_mean,
        log_alpha_mean = fit$log_alpha_mean,
        lag_row = lag_row
      )
    }
    fit_diag <- fit$diagnostics %||% list()
    fit_contract <- fit$fit_contract %||% list()
    hub_anchored <- if (identical(link_estimation_mode, "anchored_joint") ||
      identical(refit_mode, "shift_only") || identical(lock_mode, "hard_lock")) {
      TRUE
    } else if (isTRUE(lag_eligible) && nrow(lag_row) > 0L) {
      if (!identical(lock_mode, "soft_lock")) {
        rlang::abort(
          paste0(
            "Unsupported `hub_lock_mode` in linking stop-gate logic: ",
            lock_mode,
            ". Expected `hard_lock` or `soft_lock`."
          )
        )
      }
      history <- out$refit_meta$theta_mean_history %||% list()
      current_raw <- history[[length(history)]] %||% numeric()
      lag_raw <- history[[max(1L, length(history) - lag)]] %||% numeric()
      hub_items <- as.character(out$items$item_id[as.integer(out$items$set_id) == as.integer(hub_id)])
      if (!is.numeric(current_raw) || !is.numeric(lag_raw) || is.null(names(current_raw)) || is.null(names(lag_raw))) {
        FALSE
      } else {
        diff <- as.double(current_raw[hub_items] - lag_raw[hub_items])
        diff <- diff[is.finite(diff)]
        length(diff) > 0L &&
          sqrt(mean(diff^2)) <= as.double(controller$hub_theta_rmse_max %||% 0.02)
      }
    } else {
      FALSE
    }
    scope_ids <- .adaptive_link_theta_global_scope_ids(
      state = out,
      spoke_id = spoke_id,
      scope = controller$theta_global_rmse_scope %||% "direct_evidence_spoke"
    )
    theta_global_rmse_lagged <- if (identical(link_estimation_mode, "anchored_joint")) {
      .adaptive_link_theta_global_rmse_from_maps(
        current_theta = theta_mean_transformed,
        lag_theta = lag_global_theta,
        scope_ids = scope_ids
      )
    } else if (isTRUE(lag_eligible) && nrow(lag_row) > 0L) {
      .adaptive_link_theta_global_rmse_lagged(
        state = out,
        spoke_id = spoke_id,
        hub_id = hub_id,
        scope_ids = scope_ids,
        transform_mode = transform_state,
        delta_mean = fit$delta_mean,
        log_alpha_mean = fit$log_alpha_mean,
        lag_row = lag_row,
        lag = lag
      )
    } else {
      NA_real_
    }
    probe_edges_realized_tbl <- .adaptive_link_probe_edges_realized(
      state = out,
      spoke_id = spoke_id,
      epoch_id = eval_link_epoch_id
    )
    link_diagnostics_pass <- isTRUE(fit_diag$diagnostics_divergences_pass %||% NA) &&
      isTRUE(fit_diag$diagnostics_rhat_pass %||% NA) &&
      isTRUE(fit_diag$diagnostics_ess_pass %||% NA)
    probe_brier <- .adaptive_link_probe_brier_for_fit(
      edges = probe_edges_realized_tbl,
      hub_theta = ppc_hub_theta,
      spoke_theta = ppc_spoke_theta,
      delta_mean = fit$delta_mean,
      log_alpha_mean = fit$log_alpha_mean,
      judge_params = judge_params
    )
    probe_pred_rmse_lagged <- if (identical(link_estimation_mode, "anchored_joint")) {
      .adaptive_link_probe_pred_rmse_lagged_anchored_joint(
        edges = probe_edges_realized_tbl,
        current_theta = theta_mean_transformed,
        lag_theta = lag_global_theta,
        judge_params = judge_params
      )
    } else if (isTRUE(lag_eligible) && nrow(lag_row) > 0L) {
      .adaptive_link_probe_pred_rmse_lagged_for_fit(
        edges = probe_edges_realized_tbl,
        hub_theta = ppc_hub_theta,
        spoke_theta = ppc_spoke_theta,
        delta_mean = fit$delta_mean,
        log_alpha_mean = fit$log_alpha_mean,
        lag_delta_mean = lag_delta,
        lag_log_alpha_mean = lag_log_alpha,
        judge_params = judge_params
      )
    } else {
      NA_real_
    }
    link_lag_eligible <- isTRUE(lag_eligible)
    link_min_refit_eligible <- isTRUE(current_refit_id >= as.integer(controller$min_refits_in_phase_b %||% 3L))
    link_stop_gate_open <- isTRUE(link_diagnostics_pass) &&
      isTRUE(!is.na(reliability_active)) &&
      isTRUE(nrow(probe_edges_realized_tbl) >= as.integer(controller$probe_edges_min_for_stop %||% 30L))
    link_stop_eligible <- isTRUE(link_lag_eligible) &&
      isTRUE(link_min_refit_eligible) &&
      isTRUE(link_stop_gate_open)
    reliability_min_used <- as.double(controller$link_stop_reliability_min %||% 0.90)
    probe_brier_max_used <- as.double(controller$probe_brier_max %||% 0.19)
    probe_pred_rmse_max_used <- as.double(controller$probe_pred_rmse_max %||% 0.015)
    theta_global_rmse_max_used <- as.double(controller$theta_global_rmse_max %||% 0.05)
    reliability_stop_pass <- is.finite(reliability_active) &&
      reliability_active >= reliability_min_used
    probe_brier_pass <- is.finite(probe_brier) &&
      probe_brier <= probe_brier_max_used
    probe_pred_rmse_pass <- is.finite(probe_pred_rmse_lagged) &&
      probe_pred_rmse_lagged <= probe_pred_rmse_max_used
    theta_global_rmse_pass <- is.finite(theta_global_rmse_lagged) &&
      theta_global_rmse_lagged <= theta_global_rmse_max_used
    stop_window_refits_used <- as.integer(controller$stability_window_refits %||% 3L)
    stop_passes_required_used <- as.integer(controller$stability_passes_required %||% 2L)
    link_stop_pass_now <- isTRUE(link_stop_eligible) &&
      isTRUE(hub_anchored) &&
      isTRUE(reliability_stop_pass) &&
      isTRUE(probe_brier_pass) &&
      isTRUE(probe_pred_rmse_pass) &&
      isTRUE(theta_global_rmse_pass)
    stop_window <- .adaptive_link_result_window_normalize(
      stop_window_map[[key]] %||% logical(),
      max_size = stop_window_refits_used
    )
    if (isTRUE(link_stop_eligible)) {
      stop_window <- .adaptive_link_result_window_append(
        stop_window,
        result = link_stop_pass_now,
        max_size = stop_window_refits_used
      )
    }
    stop_window_map[[key]] <- stop_window
    stop_recent_pass_count <- .adaptive_link_result_window_pass_count(stop_window)
    stop_recent_window_size <- length(stop_window)
    link_stop_pass <- isTRUE(stop_recent_window_size >= stop_window_refits_used) &&
      isTRUE(stop_recent_pass_count >= stop_passes_required_used)

    cross_active_epoch <- cross_active_all[0, , drop = FALSE]
    scale_ready <- FALSE
    if (nrow(cross_all) > 0L) {
      spoke_item <- hub_item <- NULL
      cross_active_epoch <- cross_active_all[
        as.integer(cross_active_all$step_id) >= as.integer(epoch_start_step),
        ,
        drop = FALSE
      ]
      bins_used <- suppressWarnings(as.integer(
        coverage_bins_map[[key]] %||%
          controller$spoke_quantile_coverage_bins %||%
          3L
      ))
      if (length(bins_used) != 1L || is.na(bins_used) || !is.finite(bins_used) || bins_used < 1L) {
        bins_used <- max(1L, as.integer(controller$spoke_quantile_coverage_bins %||% 3L))
      }
      score_map <- .adaptive_link_phase_b_routing_scores(
        state = out,
        controller = controller,
        active_ids = active$active_spoke,
        hub_id = hub_id
      )
      spoke_bins <- .adaptive_link_probe_quantile_bins(active$active_spoke, score_map[active$active_spoke], bins_used)
      spoke_bin_tbl <- tibble::tibble(
        spoke_item = names(spoke_bins),
        spoke_bin = as.integer(spoke_bins)
      )
      realized_tbl <- tibble::as_tibble(cross_active_epoch) |>
        dplyr::distinct(spoke_item, hub_item)
      realized_bins <- dplyr::left_join(realized_tbl, spoke_bin_tbl, by = "spoke_item")
      per_bin_items <- table(realized_bins$spoke_bin)
      outer_ok <- all(vapply(c(1L, bins_used), function(bin_id) {
        rows <- realized_bins[realized_bins$spoke_bin == bin_id, , drop = FALSE]
        if (nrow(rows) < 1L) {
          return(FALSE)
        }
        length(unique(as.character(rows$hub_item))) >= 2L
      }, logical(1L)))
      scale_ready <- nrow(cross_active_epoch) >= as.integer(controller$shift_scale_min_cross_set_edges %||% 18L) &&
        all(vapply(seq_len(max(1L, bins_used)), function(bin_id) {
          count_bin <- if (as.character(bin_id) %in% names(per_bin_items)) {
            as.integer(per_bin_items[[as.character(bin_id)]])
          } else {
            0L
          }
          as.integer(count_bin) >=
            as.integer(controller$shift_scale_min_distinct_spoke_items_per_bin %||% 2L)
        }, logical(1L))) &&
        isTRUE(outer_ok)
    }
    alt_fit <- list(converged = FALSE, delta_mean = NA_real_, log_alpha_mean = NA_real_, log_alpha_sd = NA_real_)
    probe_brier_shift_only <- if (identical(transform_state, "shift_only")) probe_brier else NA_real_
    probe_brier_shift_scale <- NA_real_
    probe_brier_delta <- NA_real_
    escalated_this_refit <- FALSE
    escalation_window_refits_used <- as.integer(
      controller$link_transform_escalation_window_refits %||% 3L
    )
    escalation_passes_required_used <- as.integer(
      controller$link_transform_escalation_passes_required %||% 2L
    )
    escalation_window <- .adaptive_link_result_window_normalize(
      escalation_window_map[[key]] %||% logical(),
      max_size = escalation_window_refits_used
    )
    if (!isTRUE(transform_frozen) &&
      identical(transform_policy, "auto") &&
      identical(refit_mode, "shift_only") &&
      identical(transform_state, "shift_only") &&
      isTRUE(link_stop_eligible) &&
      isTRUE(scale_ready) &&
      nrow(probe_edges_realized_tbl) >= as.integer(controller$probe_edges_min_for_stop %||% 30L)) {
      alt_fit <- .adaptive_link_fit_transform_alt_shift_scale(
        cross_edges = cross_active_epoch,
        hub_theta = hub_theta,
        spoke_theta = spoke_theta,
        delta_init = fit$delta_mean
      )
      if (isTRUE(alt_fit$converged)) {
        probe_brier_shift_scale <- .adaptive_link_probe_brier_for_fit(
          edges = probe_edges_realized_tbl,
          hub_theta = hub_theta,
          spoke_theta = spoke_theta,
          delta_mean = alt_fit$delta_mean,
          log_alpha_mean = alt_fit$log_alpha_mean,
          judge_params = judge_params
        )
        probe_brier_delta <- as.double(probe_brier_shift_only - probe_brier_shift_scale)
      }
      escalation_pass_now <- isTRUE(alt_fit$converged) &&
        is.finite(probe_brier_delta) &&
        probe_brier_delta >= as.double(controller$probe_brier_delta_min %||% 0.005) &&
        is.finite(alt_fit$log_alpha_sd) &&
        alt_fit$log_alpha_sd <= as.double(controller$logalpha_sd_guardrail %||% 0.10)
      escalation_window <- .adaptive_link_result_window_append(
        escalation_window,
        result = escalation_pass_now,
        max_size = escalation_window_refits_used
      )
      if (length(escalation_window) >= escalation_window_refits_used &&
        .adaptive_link_result_window_pass_count(escalation_window) >=
          escalation_passes_required_used) {
        escalated_this_refit <- TRUE
        escalation_window <- logical()
        transform_state <- "shift_scale"
        state_map[[key]] <- "shift_scale"
        fit$delta_mean <- as.double(alt_fit$delta_mean %||% fit$delta_mean)
        fit$log_alpha_mean <- as.double(alt_fit$log_alpha_mean %||% 0)
        fit$log_alpha_sd <- as.double(alt_fit$log_alpha_sd %||% NA_real_)
        link_epoch_id <- as.integer(link_epoch_id + 1L)
        epoch_id_map[[key]] <- as.integer(link_epoch_id)
        epoch_start_step_map[[key]] <- as.integer(
          max(c(as.integer(tibble::as_tibble(out$step_log %||% tibble::tibble())$step_id), 0L), na.rm = TRUE) + 1L
        )
        epoch_signature <- .adaptive_link_epoch_signature_string(
          .adaptive_link_epoch_signature_components(
            transform_state = "shift_scale",
            refit_mode = refit_mode,
            lock_mode = lock_mode,
            hub_art = hub_art,
            spoke_art = spoke_art,
            link_estimation_mode = link_estimation_mode
          )
        )
        epoch_signature_map[[key]] <- as.character(epoch_signature)
        lag_domain_key <- as.character(epoch_signature)
        lag_domain_key_map[[key]] <- as.character(lag_domain_key)
        lag_domain_reset <- TRUE
        lag_domain_reset_reason <- "transform_state_change"
        lag_domain_reset_refit_map[[key]] <- as.integer(current_refit_id)
        scale_ready <- FALSE
        lag_eligible <- FALSE
        link_lag_eligible <- FALSE
        link_stop_eligible <- FALSE
        probe_pred_rmse_lagged <- NA_real_
        theta_global_rmse_lagged <- NA_real_
        rank_stability <- list(
          lag_eligible = FALSE,
          rho_rank_lagged = NA_real_,
          rho_rank_lagged_pass = FALSE
        )
        stop_window <- logical()
        stop_window_map[[key]] <- logical()
        stop_recent_pass_count <- 0L
        stop_recent_window_size <- 0L
        link_stop_pass <- FALSE
      }
    }
    escalation_window_map[[key]] <- escalation_window
    escalation_recent_pass_count <- .adaptive_link_result_window_pass_count(escalation_window)
    escalation_recent_window_size <- length(escalation_window)
    if (identical(link_estimation_mode, "anchored_joint")) {
      scale_ready <- FALSE
      escalation_recent_pass_count <- NA_integer_
      escalation_recent_window_size <- NA_integer_
      escalation_window_refits_used <- NA_integer_
      escalation_passes_required_used <- NA_integer_
    }
    stop_blockers <- .adaptive_link_stop_blockers(
      link_diagnostics_pass = link_diagnostics_pass,
      link_lag_eligible = link_lag_eligible,
      link_min_refit_eligible = link_min_refit_eligible,
      probe_edges_realized = probe_edges_realized_eval,
      probe_edges_min_for_stop = as.integer(controller$probe_edges_min_for_stop %||% 30L),
      link_stop_reliability_min = reliability_min_used,
      reliability_active = reliability_active,
      probe_brier = probe_brier,
      probe_brier_max = probe_brier_max_used,
      probe_pred_rmse_lagged = probe_pred_rmse_lagged,
      probe_pred_rmse_max = probe_pred_rmse_max_used,
      theta_global_rmse_lagged = theta_global_rmse_lagged,
      theta_global_rmse_max = theta_global_rmse_max_used,
      hub_anchored = hub_anchored
    )

    link_identified <- is.finite(reliability_active) &&
      reliability_active >= as.double(controller$link_identified_reliability_min %||% 0.80) &&
      is.finite(ts_btl_rank_active) &&
      ts_btl_rank_active >= as.double(controller$link_rank_corr_min %||% 0.90)
    link_identified_map[[key]] <- isTRUE(link_identified)
    state_map[[key]] <- as.character(transform_state)

    last_delta[[key]] <- as.double(fit$delta_mean)
    last_log_alpha[[key]] <- as.double(fit$log_alpha_mean %||% NA_real_)
    link_stats[[key]] <- list(
      link_transform_policy = as.character(transform_policy),
      link_transform_state = as.character(transform_state),
      shift_only_theta_treatment = as.character(theta_treatment),
      shift_only_theta_treatment_resolved = as.character(theta_treatment_resolved),
      link_state_frozen = as.logical(transform_frozen),
      transform_frozen = as.logical(transform_frozen),
      delta_spoke_mean = as.double(fit$delta_mean),
      delta_spoke_sd = as.double(fit$delta_sd),
      log_alpha_spoke_mean = as.double(fit$log_alpha_mean),
      log_alpha_spoke_sd = as.double(fit$log_alpha_sd),
      delta_change_lagged = as.double(delta_change),
      log_alpha_change_lagged = as.double(log_alpha_change),
      reliability_link_global = as.double(reliability_active),
      link_stop_reliability_min_used = as.double(reliability_min_used),
      link_reliability_identified_pass = as.logical(
        is.finite(reliability_active) &&
          reliability_active >= as.double(controller$link_identified_reliability_min %||% 0.80)
      ),
      link_reliability_stop_pass = as.logical(reliability_stop_pass),
      ts_btl_rank_spearman_active = as.double(ts_btl_rank_active),
      link_rank_corr_pass = as.logical(
        is.finite(ts_btl_rank_active) &&
          ts_btl_rank_active >= as.double(controller$link_rank_corr_min %||% 0.90)
      ),
      lag_domain_key = as.character(lag_domain_key),
      lag_domain_reset = as.logical(lag_domain_reset),
      lag_domain_reset_reason = as.character(lag_domain_reset_reason %||% NA_character_),
      link_epoch_id = as.integer(eval_link_epoch_id),
      lag_eligible = as.logical(lag_eligible),
      link_lag_eligible = as.logical(link_lag_eligible),
      link_min_refit_eligible = as.logical(link_min_refit_eligible),
      link_stop_gate_open = as.logical(link_stop_gate_open),
      rank_stability_lagged = as.double(rank_stability$rho_rank_lagged %||% NA_real_),
      link_identified = as.logical(link_identified),
      link_stop_eligible = as.logical(link_stop_eligible),
      stop_recent_pass_count = as.integer(stop_recent_pass_count),
      stop_recent_window_size = as.integer(stop_recent_window_size),
      link_stop_pass = as.logical(link_stop_pass),
      stability_window_refits_used = as.integer(stop_window_refits_used),
      stability_passes_required_used = as.integer(stop_passes_required_used),
      ppc_brier_cross_active = as.double(ppc_brier_cross_active),
      ppc_brier_cross_probe = as.double(ppc_brier_cross_probe),
      ppc_brier_cross = as.double(ppc_brier_cross),
      fit_contract = fit_contract,
      link_fit_method = as.character(fit_contract$estimation_method %||% NA_character_),
      link_uncertainty_approximation = as.character(
        fit_contract$uncertainty_approximation %||% NA_character_
      ),
      link_diagnostics_divergences = as.integer(fit_diag$divergences %||% NA_integer_),
      link_diagnostics_max_rhat = as.double(fit_diag$max_rhat %||% NA_real_),
      link_diagnostics_min_ess_bulk = as.double(fit_diag$min_ess_bulk %||% NA_real_),
      link_diagnostics_divergences_pass = as.logical(fit_diag$diagnostics_divergences_pass %||% NA),
      link_diagnostics_rhat_pass = as.logical(fit_diag$diagnostics_rhat_pass %||% NA),
      link_diagnostics_ess_pass = as.logical(fit_diag$diagnostics_ess_pass %||% NA),
      hub_anchored = as.logical(hub_anchored),
      scale_ready = as.logical(scale_ready),
      stop_blocker_codes = as.character(stop_blockers$codes),
      probe_brier = as.double(probe_brier),
      probe_brier_max_used = as.double(probe_brier_max_used),
      probe_brier_pass = as.logical(probe_brier_pass),
      probe_pred_rmse_lagged = as.double(probe_pred_rmse_lagged),
      probe_pred_rmse_max_used = as.double(probe_pred_rmse_max_used),
      probe_pred_rmse_pass = as.logical(probe_pred_rmse_pass),
      theta_global_rmse_scope = as.character(controller$theta_global_rmse_scope %||% "direct_evidence_spoke"),
      theta_global_rmse_lagged = as.double(theta_global_rmse_lagged),
      theta_global_rmse_max_used = as.double(theta_global_rmse_max_used),
      theta_global_rmse_pass = as.logical(theta_global_rmse_pass),
      escalated_this_refit = as.logical(escalated_this_refit),
      escalation_recent_pass_count = as.integer(escalation_recent_pass_count),
      escalation_recent_window_size = as.integer(escalation_recent_window_size),
      probe_brier_shift_only = as.double(probe_brier_shift_only),
      probe_brier_shift_scale = as.double(probe_brier_shift_scale),
      probe_brier_delta = as.double(probe_brier_delta),
      log_alpha_spoke_sd_alt = as.double(alt_fit$log_alpha_sd %||% NA_real_),
      alt_eval_active_edges = if (identical(link_estimation_mode, "anchored_joint")) {
        NA_integer_
      } else {
        as.integer(nrow(cross_active_epoch))
      },
      alt_eval_converged = if (identical(link_estimation_mode, "anchored_joint")) {
        FALSE
      } else {
        as.logical(alt_fit$converged %||% FALSE)
      },
      alternative_fit_method = if (identical(link_estimation_mode, "anchored_joint")) {
        NA_character_
      } else {
        as.character(alt_fit$fit_method %||% "map_laplace_hessian")
      },
      alternative_uncertainty_approximation = if (identical(link_estimation_mode, "anchored_joint")) {
        NA_character_
      } else {
        as.character(alt_fit$uncertainty_approximation %||% "laplace_hessian")
      },
      probe_brier_delta_min_used = if (identical(link_estimation_mode, "anchored_joint")) {
        NA_real_
      } else {
        as.double(controller$probe_brier_delta_min %||% 0.005)
      },
      logalpha_sd_guardrail_used = if (identical(link_estimation_mode, "anchored_joint")) {
        NA_real_
      } else {
        as.double(controller$logalpha_sd_guardrail %||% 0.10)
      },
      probe_edges_min_for_stop_used = as.integer(controller$probe_edges_min_for_stop %||% 30L),
      link_transform_escalation_window_refits_used = if (identical(link_estimation_mode, "anchored_joint")) {
        NA_integer_
      } else {
        as.integer(escalation_window_refits_used)
      },
      link_transform_escalation_passes_required_used = if (identical(link_estimation_mode, "anchored_joint")) {
        NA_integer_
      } else {
        as.integer(escalation_passes_required_used)
      },
      n_probe_pairs_since_last_refit = as.integer(nrow(cross_since_probe)),
      n_cross_edges_active_since_last_refit = as.integer(nrow(cross_since_active)),
      n_cross_edges_probe_since_last_refit = as.integer(nrow(cross_since_probe)),
      n_cross_edges_total_since_last_refit = as.integer(nrow(cross_since)),
      coverage_bins_used = as.integer(coverage_bins_map[[key]] %||% NA_integer_),
      coverage_source = as.character(coverage_source_map[[key]] %||% NA_character_),
      probe_panel_id = as.character(probe_panel_id_eval),
      probe_edges_planned = as.integer(probe_edges_planned_eval),
      probe_edges_realized = as.integer(probe_edges_realized_eval),
      probe_panel_shortfall = as.integer(probe_panel_shortfall_eval),
      active_item_count_hub = as.integer(length(active$active_hub)),
      active_item_count_spoke = as.integer(length(scope_ids)),
      active_item_count_total = as.integer(length(active$active_all)),
      var_mean_theta_global_active = as.double(reliability_stats$V_mu %||% NA_real_),
      mean_var_theta_global_active = as.double(reliability_stats$V_post %||% NA_real_),
      reliability_var_mu_epsilon_used = as.double(controller$reliability_var_mu_epsilon %||% 1e-6),
      reliability_total_var_epsilon_used = as.double(controller$reliability_total_var_epsilon %||% 1e-6),
      uncertainty = if (identical(link_estimation_mode, "anchored_joint")) {
        theta_sd_vals <- as.double(fit$theta_spoke_sd_post %||% numeric())
        theta_sd_vals <- theta_sd_vals[is.finite(theta_sd_vals)]
        if (length(theta_sd_vals) < 1L) {
          NA_real_
        } else {
          as.double(mean(theta_sd_vals))
        }
      } else {
        as.double(fit$delta_sd + if (is.finite(fit$log_alpha_sd)) fit$log_alpha_sd else 0)
      }
    )
  }

  budget_map <- .adaptive_link_budget_map_for_refit(
    state = out,
    controller = controller,
    eligible_spoke_ids = spoke_ids
  )
  if (length(budget_map) > 0L) {
    concurrent_mode <- identical(as.character(controller$multi_spoke_mode %||% "independent"), "concurrent")
    for (key in names(budget_map)) {
      stats_row <- link_stats[[key]] %||% list()
      budget_row <- budget_map[[key]] %||% list()
      budget_fields <- if (isTRUE(concurrent_mode)) {
        .adaptive_link_budget_fields()
      } else {
        c("B_spoke_refit_budget", "B_spoke_refit_budget_source")
      }
      for (field in budget_fields) {
        stats_row[[field]] <- budget_row[[field]] %||% stats_row[[field]] %||% NULL
      }
      link_stats[[key]] <- stats_row
    }
  }

  controller$link_refit_stats_by_spoke <- link_stats
  controller$link_transform_state_by_spoke <- state_map
  controller$link_transform_last_delta_by_spoke <- last_delta
  controller$link_transform_last_log_alpha_by_spoke <- last_log_alpha
  controller$link_state_frozen_by_spoke <- frozen_map
  controller$link_transform_frozen_by_spoke <- frozen_map
  controller$link_transform_frozen_delta_by_spoke <- frozen_delta_map
  controller$link_transform_frozen_log_alpha_by_spoke <- frozen_log_alpha_map
  controller$linking_identified_by_spoke <- link_identified_map
  controller$link_stop_recent_pass_window_by_spoke <- stop_window_map
  controller$link_escalation_recent_pass_window_by_spoke <- escalation_window_map
  controller$link_epoch_id_by_spoke <- epoch_id_map
  controller$link_epoch_signature_by_spoke <- epoch_signature_map
  controller$link_epoch_start_step_by_spoke <- epoch_start_step_map
  controller$link_lag_domain_key_by_spoke <- lag_domain_key_map
  controller$link_lag_domain_reset_refit_id_by_spoke <- lag_domain_reset_refit_map
  controller$linking_identified <- any(unlist(link_identified_map), na.rm = TRUE)
  out$controller <- controller
  out
}

.adaptive_link_stage_refit_rows <- function(state, refit_id, refit_context) {
  controller <- .adaptive_controller_resolve(state)
  run_mode <- as.character(controller$run_mode %||% "within_set")
  if (!run_mode %in% c("link_one_spoke", "link_multi_spoke")) {
    return(tibble::as_tibble(new_link_stage_log()))
  }
  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  if (!identical(phase_ctx$phase, "phase_b")) {
    return(tibble::as_tibble(new_link_stage_log()))
  }
  if (length(phase_ctx$active_spokes %||% integer()) < 1L) {
    return(tibble::as_tibble(new_link_stage_log()))
  }

  hub_id <- as.integer(controller$hub_id %||% 1L)
  spoke_ids <- .adaptive_link_spoke_ids(state, hub_id = hub_id)
  spoke_ids <- intersect(spoke_ids, as.integer(phase_ctx$active_spokes))
  if (length(spoke_ids) < 1L) {
    return(tibble::as_tibble(new_link_stage_log()))
  }

  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  round_log <- tibble::as_tibble(state$round_log %||% tibble::tibble())
  diagnostics_pass <- if (nrow(round_log) > 0L && "diagnostics_pass" %in% names(round_log)) {
    as.logical(round_log$diagnostics_pass[[nrow(round_log)]])
  } else {
    NA
  }
  rows <- vector("list", length(spoke_ids))
  link_identified_map <- controller$linking_identified_by_spoke %||% list()
  link_stats <- controller$link_refit_stats_by_spoke %||% list()
  d_opt_map <- controller$link_d_opt_it_by_spoke %||% list()
  stopped_map <- controller$link_stopped_by_spoke %||% list()
  frozen_map <- controller$link_transform_frozen_by_spoke %||% list()
  cached_budget_refit_id <- as.integer(controller$link_budget_refit_id %||% NA_integer_)
  cached_budget_map <- controller$link_budget_map %||% list()
  if (!is.na(cached_budget_refit_id) &&
    identical(cached_budget_refit_id, as.integer(refit_id)) &&
    length(cached_budget_map) > 0L) {
    budget_map <- cached_budget_map[as.character(spoke_ids)]
    budget_map <- budget_map[!vapply(budget_map, is.null, logical(1L))]
  } else {
    budget_map <- .adaptive_link_budget_map_for_refit(
      state = state,
      controller = controller,
      eligible_spoke_ids = spoke_ids
    )
  }

  for (idx in seq_along(spoke_ids)) {
    spoke_id <- as.integer(spoke_ids[[idx]])
    key <- as.character(spoke_id)
    stats_row <- link_stats[[key]] %||% list()
    linking_identified <- if (!is.null(stats_row$link_identified)) {
      isTRUE(stats_row$link_identified)
    } else if (!is.null(link_identified_map[[key]])) {
      isTRUE(link_identified_map[[key]])
    } else {
      FALSE
    }

    is_cross <- rep(FALSE, nrow(step_log))
    if (nrow(step_log) > 0L && all(c("pair_id", "is_cross_set", "link_spoke_id") %in% names(step_log))) {
      link_spoke <- as.integer(step_log$link_spoke_id)
      is_cross <- !is.na(step_log$pair_id) &
        step_log$is_cross_set %in% TRUE &
        !is.na(link_spoke) &
        link_spoke == spoke_id
    }
    cumulative <- step_log[is_cross, , drop = FALSE]
    since_last <- cumulative
    if (nrow(cumulative) > 0L && "step_id" %in% names(cumulative)) {
      since_last <- cumulative[cumulative$step_id > as.integer(refit_context$last_refit_step %||% 0L), , drop = FALSE]
    }
    if (!"run_mode" %in% names(since_last)) {
      since_last$run_mode <- NA_character_
    }
    if (!"is_probe_step" %in% names(since_last)) {
      since_last$is_probe_step <- NA
    }

    n_pairs_done <- as.integer(nrow(cumulative))
    since_last_probe_flag <- .adaptive_link_is_holdout_probe_rows(since_last)
    since_last_probe <- since_last[
      since_last_probe_flag,
      ,
      drop = FALSE
    ]
    since_last_active <- since_last[
      !since_last_probe_flag,
      ,
      drop = FALSE
    ]
    n_pairs_since_probe <- as.integer(nrow(since_last_probe))
    n_pairs_since_active <- as.integer(nrow(since_last_active))
    n_pairs_since_total <- as.integer(nrow(since_last))
    retired_spoke <- isTRUE(stopped_map[[key]]) || isTRUE(frozen_map[[key]])
    budget_info <- budget_map[[key]] %||% if (isTRUE(retired_spoke)) {
      list(
        B_spoke_refit_budget = 0L,
        B_spoke_refit_budget_source = "frozen_spoke_retired"
      )
    } else {
      list(
        B_spoke_refit_budget = .adaptive_link_refit_budget_default(as.integer(state$n_items), controller),
        B_spoke_refit_budget_source = "single_spoke_default"
      )
    }
    quota_controller <- controller
    quota_controller$current_link_spoke_id <- as.integer(spoke_id)
    quota_controller$B_spoke_refit_budget <- as.integer(budget_info$B_spoke_refit_budget %||% NA_integer_)
    quota_controller$B_spoke_refit_budget_source <- as.character(
      budget_info$B_spoke_refit_budget_source %||% "single_spoke_default"
    )
    stage_quotas <- .adaptive_round_compute_quotas(
      round_id = as.integer(state$round$round_id %||% 1L),
      n_items = as.integer(state$n_items),
      controller = quota_controller
    )
    stage_quotas <- .adaptive_link_adjust_stage_quotas_for_feasibility(
      state = state,
      controller = controller,
      spoke_id = as.integer(spoke_id),
      stage_quotas = stage_quotas,
      stage_order = .adaptive_stage_order(),
      refit_id = as.integer(refit_id)
    )
    quota_meta <- attr(stage_quotas, "quota_meta") %||% list()
    quota_long_link_raw <- as.integer(quota_meta$long_quota_raw %||% NA_integer_)
    quota_long_link_effective <- as.integer(quota_meta$long_quota_effective %||%
      stage_quotas[["long_link"]] %||% NA_integer_)
    quota_long_link_removed <- as.integer(quota_meta$long_quota_removed %||% NA_integer_)
    quota_taper_applied <- if (!is.na(quota_long_link_raw) && !is.na(quota_long_link_effective)) {
      as.logical(quota_long_link_effective < quota_long_link_raw)
    } else {
      as.logical(quota_meta$taper_applied %||% FALSE)
    }
    quota_taper_spoke_id <- as.integer(quota_meta$link_spoke_id %||% spoke_id)
    stage_order <- .adaptive_stage_order()
    committed_stage <- stats::setNames(rep.int(0L, length(stage_order)), stage_order)
    refit_spoke_key <- .adaptive_link_refit_spoke_key(
      refit_id = as.integer(refit_id),
      spoke_id = as.integer(spoke_id)
    )
    tracked_shortfalls <- .adaptive_link_refit_shortfalls_map(state)[[refit_spoke_key]] %||% list()
    refit_step_end <- if ("step_id" %in% names(step_log) && nrow(step_log) > 0L) {
      as.integer(max(as.integer(step_log$step_id), na.rm = TRUE))
    } else {
      0L
    }
    refit_step_start <- as.integer(refit_context$last_refit_step %||% 0L)
    if (nrow(step_log) > 0L && all(c("pair_id", "step_id", "link_spoke_id", "is_cross_set") %in%
      names(step_log))) {
      stage_col <- if ("link_stage" %in% names(step_log)) "link_stage" else "round_stage"
      stage_rows <- step_log[
        !is.na(step_log$pair_id) &
          step_log$is_cross_set %in% TRUE &
          as.integer(step_log$step_id) > refit_step_start &
          as.integer(step_log$step_id) <= refit_step_end &
          as.integer(step_log$link_spoke_id) == as.integer(spoke_id) &
          as.character(step_log[[stage_col]]) %in% stage_order,
        ,
        drop = FALSE
      ]
      if (nrow(stage_rows) > 0L) {
        tab_stage <- table(factor(as.character(stage_rows[[stage_col]]), levels = stage_order))
        committed_stage[names(tab_stage)] <- as.integer(tab_stage)
      }
    }
    stage_quotas <- stats::setNames(
      vapply(
        stage_order,
        function(stage_name) {
          as.integer(stage_quotas[[stage_name]] %||% 0L)
        },
        integer(1L)
      ),
      stage_order
    )
    stage_quotas[!is.finite(stage_quotas)] <- 0L
    committed_stage[!is.finite(committed_stage)] <- 0L
    authoritative_budget_total <- as.integer(
      budget_info$B_spoke_refit_budget %||% sum(stage_quotas, na.rm = TRUE)
    )
    realized_active_budget_floor <- as.integer(sum(committed_stage, na.rm = TRUE))
    if (is.finite(authoritative_budget_total) &&
      authoritative_budget_total < realized_active_budget_floor) {
      rlang::abort(
        paste0(
          "link_stage_log budget invariant failure: realized active counts exceed emitted budget ",
          "for spoke_id=", as.integer(spoke_id),
          " at refit_id=", as.integer(refit_id),
          ". budget=", as.integer(authoritative_budget_total),
          ", realized=", as.integer(realized_active_budget_floor),
          "."
        )
      )
    }
    tracked_targets <- stats::setNames(rep.int(NA_integer_, length(stage_order)), stage_order)
    for (stage_name in stage_order) {
      shortfall_val <- as.integer(tracked_shortfalls[[stage_name]] %||% NA_integer_)
      if (is.finite(shortfall_val)) {
        tracked_targets[[stage_name]] <- as.integer((committed_stage[[stage_name]] %||% 0L) + shortfall_val)
      }
    }
    has_tracked_targets <- any(is.finite(tracked_targets))
    stage_total <- as.integer(sum(stage_quotas, na.rm = TRUE))
    needs_completed_window_reconcile <- isTRUE(has_tracked_targets) ||
      stage_total < realized_active_budget_floor
    if (isTRUE(needs_completed_window_reconcile) &&
      is.finite(authoritative_budget_total) &&
      authoritative_budget_total >= 0L) {
      if (isTRUE(has_tracked_targets)) {
        for (stage_name in stage_order) {
          if (is.finite(tracked_targets[[stage_name]])) {
            stage_quotas[[stage_name]] <- as.integer(tracked_targets[[stage_name]])
          }
        }
      }
      stage_quotas <- stats::setNames(
        pmax(as.integer(stage_quotas), as.integer(committed_stage)),
        stage_order
      )
      stage_total <- as.integer(sum(stage_quotas, na.rm = TRUE))
      if (stage_total != authoritative_budget_total) {
        slack <- pmax(as.integer(stage_quotas) - as.integer(committed_stage), 0L)
        names(slack) <- stage_order
        if (stage_total > authoritative_budget_total) {
          excess <- as.integer(stage_total - authoritative_budget_total)
          reduce_order <- names(sort(slack, decreasing = TRUE))
          for (stage_name in reduce_order) {
            if (excess <= 0L) {
              break
            }
            reducible <- min(excess, slack[[stage_name]])
            if (reducible > 0L) {
              stage_quotas[[stage_name]] <- as.integer(stage_quotas[[stage_name]] - reducible)
              excess <- as.integer(excess - reducible)
            }
          }
        } else {
          remainder <- as.integer(authoritative_budget_total - stage_total)
          add_order <- names(sort(as.integer(stage_quotas), decreasing = TRUE))
          if (length(add_order) < 1L) {
            add_order <- stage_order
          }
          if (all(replace(as.integer(stage_quotas[add_order]), is.na(as.integer(stage_quotas[add_order])), 0L) == 0L)) {
            add_order <- stage_order
          }
          pos <- 1L
          while (remainder > 0L && length(add_order) > 0L) {
            stage_name <- add_order[[pos]]
            stage_quotas[[stage_name]] <- as.integer(stage_quotas[[stage_name]] + 1L)
            remainder <- as.integer(remainder - 1L)
            pos <- if (pos >= length(add_order)) 1L else pos + 1L
          }
        }
      }
    }
    stage_budget_total <- as.integer(sum(stage_quotas, na.rm = TRUE))
    if (stage_budget_total < realized_active_budget_floor) {
      rlang::abort(
        paste0(
          "link_stage_log budget invariant failure: realized active counts exceed emitted budget ",
          "for spoke_id=", as.integer(spoke_id),
          " at refit_id=", as.integer(refit_id),
          ". budget=", as.integer(stage_budget_total),
          ", realized=", as.integer(realized_active_budget_floor),
          "."
        )
      )
    }
    n_unique <- 0L
    if (nrow(cumulative) > 0L && all(c("A", "B") %in% names(cumulative))) {
      ids <- as.character(state$item_ids)
      a_id <- ids[as.integer(cumulative$A)]
      b_id <- ids[as.integer(cumulative$B)]
      n_unique <- as.integer(length(unique(make_unordered_key(a_id, b_id))))
    }

    spoke_items <- as.character(state$items$item_id[as.integer(state$items$set_id) == spoke_id])

    hub_items <- as.character(state$items$item_id[as.integer(state$items$set_id) == hub_id])
    coverage_ids <- unique(c(hub_items, spoke_items))
    coverage_scores <- .adaptive_link_phase_b_routing_scores(
      state = state,
      controller = controller,
      active_ids = coverage_ids,
      hub_id = hub_id
    )
    coverage <- .adaptive_link_spoke_coverage(
      state = state,
      controller = controller,
      spoke_id = spoke_id,
      spoke_ids = spoke_items,
      routing_scores = coverage_scores,
      score_source = "linking_global_score"
    )

    link_estimation_mode <- as.character(controller$link_estimation_mode %||% "transform")
    transform_policy <- if (identical(link_estimation_mode, "anchored_joint")) {
      NA_character_
    } else {
      as.character(stats_row$link_transform_policy %||%
        controller$link_transform_policy %||% "auto")
    }
    transform_state <- if (identical(link_estimation_mode, "anchored_joint")) {
      NA_character_
    } else {
      as.character(stats_row$link_transform_state %||%
        .adaptive_link_transform_state_for_spoke(controller, spoke_id))
    }
    d_opt_key <- .adaptive_link_d_opt_state_key(refit_id = as.integer(refit_id), spoke_id = as.integer(spoke_id))
    d_opt_entry <- d_opt_map[[d_opt_key]] %||%
      .adaptive_link_d_opt_state_get(
        controller = controller,
        refit_id = as.integer(refit_id),
        spoke_id = as.integer(spoke_id),
        transform_mode = transform_state,
        link_estimation_mode = link_estimation_mode,
        free_block_dim = if (identical(link_estimation_mode, "anchored_joint")) {
          .adaptive_link_anchored_joint_free_block_dim(
            state = state,
            spoke_id = as.integer(spoke_id),
            controller = controller
          )
        } else {
          NULL
        }
      )
    d_opt_dim <- .adaptive_link_d_opt_matrix_dim(
      transform_mode = transform_state,
      link_estimation_mode = link_estimation_mode,
      free_block_dim = if (identical(link_estimation_mode, "anchored_joint")) {
        .adaptive_link_anchored_joint_free_block_dim(
          state = state,
          spoke_id = as.integer(spoke_id),
          controller = controller
        )
      } else {
        NULL
      }
    )
    d_opt_it <- as.matrix(d_opt_entry$it %||% matrix(0, nrow = d_opt_dim, ncol = d_opt_dim))
    d_opt_logdet_start <- as.double(d_opt_entry$it_logdet_start %||% NA_real_)
    d_opt_logdet_end <- .adaptive_link_logdet_spd(d_opt_it, ridge = 1e-6)
    d_opt_trace_end <- if (is.matrix(d_opt_it) && nrow(d_opt_it) == ncol(d_opt_it)) {
      as.double(sum(diag(d_opt_it)))
    } else {
      NA_real_
    }
    d_opt_n_pairs <- as.integer(d_opt_entry$it_n_pairs_accumulated %||% 0L)
    reliability_stop_pass <- as.logical(stats_row$link_reliability_stop_pass %||% NA)
    lag_eligible <- as.logical(stats_row$link_lag_eligible %||% stats_row$lag_eligible %||% FALSE)
    link_min_refit_eligible <- as.logical(stats_row$link_min_refit_eligible %||% FALSE)
    link_stop_gate_open <- as.logical(stats_row$link_stop_gate_open %||% FALSE)
    link_stop_eligible <- as.logical(stats_row$link_stop_eligible %||%
      (isTRUE(lag_eligible) && isTRUE(link_min_refit_eligible) && isTRUE(link_stop_gate_open)))
    link_stop_pass <- as.logical(stats_row$link_stop_pass %||% FALSE)
    transform_frozen <- isTRUE(
      stats_row$link_state_frozen %||% stats_row$transform_frozen %||% FALSE
    ) || isTRUE(link_stop_pass)
    stats_epoch_id <- as.integer(stats_row$link_epoch_id %||% .adaptive_link_probe_epoch_for_spoke(state, spoke_id))
    probe_panel <- .adaptive_link_probe_panel_for_spoke(
      state,
      spoke_id = as.integer(spoke_id),
      epoch_id = stats_epoch_id
    )
    probe_panel_id <- as.character(
      stats_row$probe_panel_id %||%
        if (nrow(probe_panel) > 0L) probe_panel$probe_panel_id[[1L]] else NA_character_
    )
    probe_edges_planned <- as.integer(stats_row$probe_edges_planned %||% nrow(probe_panel))
    realized_probe_log <- .adaptive_link_probe_realized_log_for_panel(
      state = state,
      spoke_id = as.integer(spoke_id),
      epoch_id = as.integer(stats_epoch_id),
      panel = probe_panel
    )
    realized_probe_log_current_window <- realized_probe_log[
      as.integer(realized_probe_log$step_id) > refit_step_start &
        as.integer(realized_probe_log$step_id) <= refit_step_end,
      ,
      drop = FALSE
    ]
    canonical_probe_edges_realized <- as.integer(nrow(realized_probe_log))
    probe_edges_realized_before_refit <- as.integer(
      max(0L, canonical_probe_edges_realized - nrow(realized_probe_log_current_window))
    )
    prior_probe_edges_realized_max <- .adaptive_link_probe_prior_realized_max(
      link_stage_log = state$link_stage_log,
      spoke_id = as.integer(spoke_id),
      epoch_id = as.integer(stats_epoch_id),
      refit_id = as.integer(refit_id)
    )
    if (is.finite(prior_probe_edges_realized_max) &&
      canonical_probe_edges_realized < prior_probe_edges_realized_max) {
      rlang::abort(
        paste0(
          "Phase B probe monotonicity invariant failed: canonical `probe_edges_realized` decreased ",
          "within link_epoch_id=", as.integer(stats_epoch_id),
          " for spoke_id=", as.integer(spoke_id),
          ". prior_max=", as.integer(prior_probe_edges_realized_max),
          ", current=", as.integer(canonical_probe_edges_realized),
          "."
        )
      )
    }
    if (nrow(since_last_probe) > 0L &&
      !identical(as.integer(nrow(since_last_probe)), as.integer(nrow(realized_probe_log_current_window)))) {
      rlang::abort(
        paste0(
          "Phase B probe accounting invariant failed: `n_probe_pairs_since_last_refit` from committed ",
          "probe steps does not match canonical realized probe rows for spoke_id=",
          as.integer(spoke_id),
          " at refit_id=", as.integer(refit_id),
          ". steps=", as.integer(nrow(since_last_probe)),
          ", canonical=", as.integer(nrow(realized_probe_log_current_window)),
          "."
        )
      )
    }
    n_pairs_since_probe <- as.integer(nrow(realized_probe_log_current_window))
    probe_edges_realized <- canonical_probe_edges_realized
    probe_edges_realized_delta_since_last_refit <- as.integer(n_pairs_since_probe)
    probe_panel_shortfall <- as.integer(
      max(0L, probe_edges_planned - probe_edges_realized)
    )
    probe_shortfall_reason <- if (probe_panel_shortfall < 1L) {
      "none"
    } else if (identical(as.character(stats_row$lag_domain_reset_reason %||% NA_character_), "probe_panel_rebuild")) {
      "probe_panel_rebuild"
    } else if (!is.na(as.character(stats_row$lag_domain_reset_reason %||% NA_character_))) {
      "epoch_reset"
    } else {
      "insufficient_realization"
    }
    probe_effort_plan <- .adaptive_link_probe_effort_plan(
      state = state,
      controller = controller,
      spoke_id = as.integer(spoke_id)
    )
    probe_effort_base_cap <- max(0L, as.integer(controller$probe_pairs_per_refit_per_spoke %||% 2L))
    probe_panel_reallocation_used <- as.logical(n_pairs_since_probe > probe_effort_base_cap)
    probe_cache <- tibble::as_tibble(.adaptive_link_probe_state(state)$prediction_cache)
    probe_pred_cache_used <- nrow(probe_cache[
      as.integer(probe_cache$refit_id) == as.integer(refit_id) &
        as.integer(probe_cache$spoke_id) == as.integer(spoke_id),
      ,
      drop = FALSE
    ]) > 0L

    reallocation_used <- isTRUE(quota_meta$feasibility_reallocation_used %||% FALSE) ||
      any(committed_stage > stage_quotas, na.rm = TRUE)
    reallocation_rule <- if (isTRUE(reallocation_used)) "pooled_utility_backfill" else "none"
    rows[[idx]] <- list(
      refit_id = as.integer(refit_id),
      spoke_id = as.integer(spoke_id),
      hub_id = as.integer(hub_id),
      link_epoch_id = as.integer(stats_epoch_id),
      link_estimation_mode = as.character(controller$link_estimation_mode %||% "transform"),
      link_transform_policy = as.character(transform_policy),
      link_transform_state = as.character(transform_state),
      link_refit_mode = as.character(controller$link_refit_mode %||% NA_character_),
      hub_lock_mode = as.character(controller$hub_lock_mode %||% NA_character_),
      hub_lock_kappa = if (identical(as.character(controller$hub_lock_mode %||% NA_character_), "soft_lock")) {
        as.double(controller$hub_lock_kappa %||% NA_real_)
      } else {
        NA_real_
      },
      shift_only_theta_treatment = as.character(
        stats_row$shift_only_theta_treatment %||% controller$shift_only_theta_treatment %||% NA_character_
      ),
      shift_only_theta_treatment_resolved = as.character(
        stats_row$shift_only_theta_treatment_resolved %||%
          stats_row$shift_only_theta_treatment %||%
          controller$shift_only_theta_treatment %||%
          NA_character_
      ),
      delta_spoke_mean = as.double(stats_row$delta_spoke_mean %||% NA_real_),
      delta_spoke_sd = as.double(stats_row$delta_spoke_sd %||% NA_real_),
      log_alpha_spoke_mean = as.double(stats_row$log_alpha_spoke_mean %||% NA_real_),
      log_alpha_spoke_sd = as.double(stats_row$log_alpha_spoke_sd %||% NA_real_),
      delta_change_lagged = as.double(stats_row$delta_change_lagged %||% NA_real_),
      log_alpha_change_lagged = as.double(stats_row$log_alpha_change_lagged %||% NA_real_),
      reliability_link_global = as.double(
        stats_row$reliability_link_global %||% stats_row$link_reliability %||% NA_real_
      ),
      link_stop_reliability_min_used = as.double(
        stats_row$link_stop_reliability_min_used %||%
          controller$link_stop_reliability_min %||%
          0.90
      ),
      reliability_stop_pass = as.logical(stats_row$link_reliability_stop_pass %||% NA),
      linking_identified = as.logical(linking_identified),
      lag_eligible = as.logical(stats_row$lag_eligible %||% FALSE),
      link_lag_eligible = as.logical(stats_row$link_lag_eligible %||% stats_row$lag_eligible %||% FALSE),
      link_min_refit_eligible = as.logical(stats_row$link_min_refit_eligible %||% FALSE),
      link_stop_gate_open = as.logical(stats_row$link_stop_gate_open %||% FALSE),
      rank_stability_lagged = as.double(stats_row$rank_stability_lagged %||% NA_real_),
      link_stop_eligible = as.logical(link_stop_eligible),
      stop_recent_pass_count = as.integer(
        stats_row$stop_recent_pass_count %||% stats_row$stop_consecutive_pass_count %||% 0L
      ),
      stop_recent_window_size = as.integer(
        stats_row$stop_recent_window_size %||% stats_row$stop_consecutive_pass_count %||% 0L
      ),
      link_stop_pass = as.logical(link_stop_pass),
      link_state_frozen = as.logical(transform_frozen),
      link_state_frozen_refit_id = as.integer(
        controller$link_state_frozen_refit_id_by_spoke[[key]] %||%
          controller$link_transform_frozen_refit_id_by_spoke[[key]] %||%
          if (isTRUE(transform_frozen)) refit_id else NA_integer_
      ),
      stability_window_refits_used = as.integer(
        stats_row$stability_window_refits_used %||%
          controller$stability_window_refits %||%
          3L
      ),
      stability_passes_required_used = as.integer(
        stats_row$stability_passes_required_used %||%
          stats_row$stability_consecutive_k %||%
          controller$stability_passes_required %||%
          controller$stability_consecutive_k %||%
          2L
      ),
      ts_btl_rank_spearman = as.double(stats_row$ts_btl_rank_spearman_active %||% NA_real_),
      ppc_brier_cross_active = as.double(stats_row$ppc_brier_cross_active %||% NA_real_),
      ppc_brier_cross_probe = as.double(stats_row$ppc_brier_cross_probe %||% NA_real_),
      ppc_brier_cross = as.double(stats_row$ppc_brier_cross %||% NA_real_),
      hub_anchored = as.logical(stats_row$hub_anchored %||% NA),
      scale_ready = as.logical(stats_row$scale_ready %||% NA),
      stop_blocker_codes = as.character(stats_row$stop_blocker_codes %||% NA_character_),
      link_fit_method = as.character(stats_row$link_fit_method %||% NA_character_),
      link_uncertainty_approximation = as.character(
        stats_row$link_uncertainty_approximation %||% NA_character_
      ),
      link_diagnostics_divergences = as.integer(stats_row$link_diagnostics_divergences %||% NA_integer_),
      link_diagnostics_max_rhat = as.double(stats_row$link_diagnostics_max_rhat %||% NA_real_),
      link_diagnostics_min_ess_bulk = as.double(stats_row$link_diagnostics_min_ess_bulk %||% NA_real_),
      link_diagnostics_divergences_pass = as.logical(stats_row$link_diagnostics_divergences_pass %||% NA),
      link_diagnostics_rhat_pass = as.logical(stats_row$link_diagnostics_rhat_pass %||% NA),
      link_diagnostics_ess_pass = as.logical(stats_row$link_diagnostics_ess_pass %||% NA),
      escalation_recent_pass_count = as.integer(
        stats_row$escalation_recent_pass_count %||%
          stats_row$escalation_consecutive_pass_count %||%
          0L
      ),
      escalation_recent_window_size = as.integer(
        stats_row$escalation_recent_window_size %||%
          stats_row$escalation_consecutive_pass_count %||%
          0L
      ),
      escalated_this_refit = as.logical(stats_row$escalated_this_refit %||% FALSE),
      probe_brier_shift_only = as.double(stats_row$probe_brier_shift_only %||% NA_real_),
      probe_brier_shift_scale = as.double(stats_row$probe_brier_shift_scale %||% NA_real_),
      probe_brier_delta = as.double(stats_row$probe_brier_delta %||% NA_real_),
      log_alpha_spoke_sd_alt = as.double(stats_row$log_alpha_spoke_sd_alt %||% NA_real_),
      n_pairs_cross_set_done = as.integer(n_pairs_done),
      n_unique_cross_pairs_seen = as.integer(n_unique),
      n_probe_pairs_since_last_refit = as.integer(n_pairs_since_probe),
      n_cross_edges_active_since_last_refit = as.integer(n_pairs_since_active),
      n_cross_edges_probe_since_last_refit = as.integer(n_pairs_since_probe),
      n_cross_edges_total_since_last_refit = as.integer(n_pairs_since_total),
      B_spoke_refit_budget = as.integer(sum(stage_quotas)),
      B_spoke_refit_budget_source = as.character(
        budget_info$B_spoke_refit_budget_source %||% "single_spoke_default"
      ),
      stage_target_anchor_link = as.integer(stage_quotas[["anchor_link"]] %||% NA_integer_),
      stage_target_long_link = as.integer(stage_quotas[["long_link"]] %||% NA_integer_),
      stage_target_mid_link = as.integer(stage_quotas[["mid_link"]] %||% NA_integer_),
      stage_target_local_link = as.integer(stage_quotas[["local_link"]] %||% NA_integer_),
      feasible_stage_capacity_anchor_link = as.integer(
        quota_meta$feasible_stage_capacity_anchor_link %||% stage_quotas[["anchor_link"]] %||% NA_integer_
      ),
      feasible_stage_capacity_long_link = as.integer(
        quota_meta$feasible_stage_capacity_long_link %||% stage_quotas[["long_link"]] %||% NA_integer_
      ),
      feasible_stage_capacity_mid_link = as.integer(
        quota_meta$feasible_stage_capacity_mid_link %||% stage_quotas[["mid_link"]] %||% NA_integer_
      ),
      feasible_stage_capacity_local_link = as.integer(
        quota_meta$feasible_stage_capacity_local_link %||% stage_quotas[["local_link"]] %||% NA_integer_
      ),
      feasibility_budget_released = as.integer(quota_meta$feasibility_budget_released %||% 0L),
      feasibility_reallocation_used = as.logical(quota_meta$feasibility_reallocation_used %||% FALSE),
      feasibility_reallocation_rule = as.character(
        quota_meta$feasibility_reallocation_rule %||% "none"
      ),
      stage_realized_anchor_link = as.integer(committed_stage[["anchor_link"]] %||% 0L),
      stage_realized_long_link = as.integer(committed_stage[["long_link"]] %||% 0L),
      stage_realized_mid_link = as.integer(committed_stage[["mid_link"]] %||% 0L),
      stage_realized_local_link = as.integer(committed_stage[["local_link"]] %||% 0L),
      stage_shortfall_anchor_link = as.integer(
        max(0L, (stage_quotas[["anchor_link"]] %||% 0L) - (committed_stage[["anchor_link"]] %||% 0L))
      ),
      stage_shortfall_long_link = as.integer(
        max(0L, (stage_quotas[["long_link"]] %||% 0L) - (committed_stage[["long_link"]] %||% 0L))
      ),
      stage_shortfall_mid_link = as.integer(
        max(0L, (stage_quotas[["mid_link"]] %||% 0L) - (committed_stage[["mid_link"]] %||% 0L))
      ),
      stage_shortfall_local_link = as.integer(
        max(0L, (stage_quotas[["local_link"]] %||% 0L) - (committed_stage[["local_link"]] %||% 0L))
      ),
      stage_reallocation_used = as.logical(reallocation_used),
      stage_reallocation_rule_used = as.character(reallocation_rule),
      stage_budget_unfilled = as.integer(max(0L, sum(stage_quotas) - sum(committed_stage))),
      quota_anchor_link = as.integer(stage_quotas[["anchor_link"]] %||% NA_integer_),
      quota_long_link = as.integer(stage_quotas[["long_link"]] %||% NA_integer_),
      quota_mid_link = as.integer(stage_quotas[["mid_link"]] %||% NA_integer_),
      quota_local_link = as.integer(stage_quotas[["local_link"]] %||% NA_integer_),
      quota_long_link_raw = as.integer(quota_long_link_raw),
      quota_long_link_effective = as.integer(quota_long_link_effective),
      quota_long_link_removed = as.integer(quota_long_link_removed),
      quota_taper_applied = as.logical(quota_taper_applied),
      quota_taper_spoke_id = as.integer(quota_taper_spoke_id),
      long_link_taper_applied = as.logical(quota_meta$long_link_taper_applied %||% quota_taper_applied),
      stage_target_long_link_pre_taper = as.integer(
        quota_meta$stage_target_long_link_pre_taper %||% quota_long_link_raw
      ),
      stage_target_long_link_post_taper = as.integer(
        quota_meta$stage_target_long_link_post_taper %||% quota_long_link_effective
      ),
      committed_anchor_link = as.integer(committed_stage[["anchor_link"]] %||% 0L),
      committed_long_link = as.integer(committed_stage[["long_link"]] %||% 0L),
      committed_mid_link = as.integer(committed_stage[["mid_link"]] %||% 0L),
      committed_local_link = as.integer(committed_stage[["local_link"]] %||% 0L),
      concurrent_target_pairs = as.integer(
        budget_info$concurrent_target_pairs %||% stats_row$concurrent_target_pairs %||% NA_integer_
      ),
      concurrent_floor_pairs = as.integer(
        budget_info$concurrent_floor_pairs %||% stats_row$concurrent_floor_pairs %||% NA_integer_
      ),
      concurrent_floor_met = as.logical(
        budget_info$concurrent_floor_met %||% stats_row$concurrent_floor_met %||% NA
      ),
      concurrent_target_met = as.logical(
        budget_info$concurrent_target_met %||% stats_row$concurrent_target_met %||% NA
      ),
      concurrent_utility_mass = as.double(
        budget_info$concurrent_utility_mass %||% stats_row$concurrent_utility_mass %||% NA_real_
      ),
      concurrent_top_k_used = as.integer(
        budget_info$concurrent_top_k_used %||% stats_row$concurrent_top_k_used %||% NA_integer_
      ),
      concurrent_candidate_count = as.integer(
        budget_info$concurrent_candidate_count %||% stats_row$concurrent_candidate_count %||% NA_integer_
      ),
      active_item_count_hub = as.integer(stats_row$active_item_count_hub %||% NA_integer_),
      active_item_count_spoke = as.integer(stats_row$active_item_count_spoke %||% NA_integer_),
      active_item_count_total = as.integer(
        stats_row$active_item_count_total %||%
          ((stats_row$active_item_count_hub %||% 0L) + (stats_row$active_item_count_spoke %||% 0L))
      ),
      var_mean_theta_global_active = as.double(stats_row$var_mean_theta_global_active %||% NA_real_),
      mean_var_theta_global_active = as.double(stats_row$mean_var_theta_global_active %||% NA_real_),
      reliability_var_mu_epsilon_used = as.double(stats_row$reliability_var_mu_epsilon_used %||% NA_real_),
      reliability_total_var_epsilon_used = as.double(stats_row$reliability_total_var_epsilon_used %||% NA_real_),
      it_logdet_start = as.double(d_opt_logdet_start),
      it_logdet_end = as.double(d_opt_logdet_end),
      it_trace_end = as.double(d_opt_trace_end),
      it_n_pairs_accumulated = as.integer(d_opt_n_pairs),
      coverage_bins_used = as.integer(stats_row$coverage_bins_used %||% coverage$bins_used %||% NA_integer_),
      coverage_source = as.character(stats_row$coverage_source %||% coverage$source %||% NA_character_),
      probe_panel_id = as.character(probe_panel_id),
      N_spoke_phase_b_start = as.integer(sum(as.integer(state$items$set_id) == as.integer(spoke_id), na.rm = TRUE)),
      probe_edges_planned = as.integer(probe_edges_planned),
      probe_edges_realized_before_refit = as.integer(probe_edges_realized_before_refit),
      probe_edges_realized = as.integer(probe_edges_realized),
      probe_edges_realized_delta_since_last_refit = as.integer(probe_edges_realized_delta_since_last_refit),
      probe_panel_shortfall = as.integer(probe_panel_shortfall),
      probe_shortfall_reason = as.character(probe_shortfall_reason),
      probe_acceleration_used = as.logical(probe_effort_plan$acceleration_used %||% FALSE),
      probe_effort_base_cap = as.integer(probe_effort_plan$base_cap %||% probe_effort_base_cap),
      probe_effort_effective_cap = as.integer(
        probe_effort_plan$effective_cap %||% probe_effort_base_cap
      ),
      probe_remaining_to_min_start = as.integer(
        probe_effort_plan$remaining_to_min_start %||% NA_integer_
      ),
      probe_panel_reallocation_used = as.logical(probe_panel_reallocation_used),
      probe_pred_cache_used = as.logical(probe_pred_cache_used),
      probe_brier = as.double(stats_row$probe_brier %||% NA_real_),
      probe_brier_max_used = as.double(
        stats_row$probe_brier_max_used %||% controller$probe_brier_max %||% 0.19
      ),
      probe_brier_pass = as.logical(stats_row$probe_brier_pass %||% NA),
      probe_pred_rmse_lagged = as.double(stats_row$probe_pred_rmse_lagged %||% NA_real_),
      probe_pred_rmse_max_used = as.double(
        stats_row$probe_pred_rmse_max_used %||% controller$probe_pred_rmse_max %||% 0.015
      ),
      probe_pred_rmse_pass = as.logical(stats_row$probe_pred_rmse_pass %||% NA),
      theta_global_rmse_scope = as.character(
        stats_row$theta_global_rmse_scope %||% controller$theta_global_rmse_scope %||% "direct_evidence_spoke"
      ),
      theta_global_rmse_lagged = as.double(stats_row$theta_global_rmse_lagged %||% NA_real_),
      theta_global_rmse_max_used = as.double(
        stats_row$theta_global_rmse_max_used %||% controller$theta_global_rmse_max %||% 0.05
      ),
      theta_global_rmse_pass = as.logical(stats_row$theta_global_rmse_pass %||% NA),
      probe_edges_min_for_stop_used = as.integer(
        stats_row$probe_edges_min_for_stop_used %||% controller$probe_edges_min_for_stop %||% 30L
      ),
      alternative_fit_method = as.character(stats_row$alternative_fit_method %||% NA_character_),
      alternative_uncertainty_approximation = as.character(
        stats_row$alternative_uncertainty_approximation %||% NA_character_
      ),
      alt_eval_active_edges = as.integer(stats_row$alt_eval_active_edges %||% NA_integer_),
      alt_eval_converged = as.logical(stats_row$alt_eval_converged %||% NA),
      probe_brier_delta_min_used = as.double(
        stats_row$probe_brier_delta_min_used %||% controller$probe_brier_delta_min %||% 0.005
      ),
      logalpha_sd_guardrail_used = as.double(
        stats_row$logalpha_sd_guardrail_used %||% controller$logalpha_sd_guardrail %||% 0.10
      ),
      link_transform_escalation_window_refits_used = as.integer(
        stats_row$link_transform_escalation_window_refits_used %||%
          stats_row$link_transform_escalation_refits_required_used %||%
          controller$link_transform_escalation_window_refits %||%
          controller$link_transform_escalation_refits_required %||%
          3L
      ),
      link_transform_escalation_passes_required_used = as.integer(
        stats_row$link_transform_escalation_passes_required_used %||%
          stats_row$link_transform_escalation_refits_required_used %||%
          controller$link_transform_escalation_passes_required %||%
          controller$link_transform_escalation_refits_required %||%
          2L
      ),
      probe_edges_count_toward_active_constraints_used = as.logical(
        controller$probe_edges_count_toward_active_constraints %||% FALSE
      ),
      lag_domain_key = as.character(stats_row$lag_domain_key %||% NA_character_),
      lag_domain_reset = as.logical(stats_row$lag_domain_reset %||% NA),
      lag_domain_reset_reason = as.character(stats_row$lag_domain_reset_reason %||% NA_character_),
      resumed_from_session = as.logical(.adaptive_is_resumed_session(state))
    )
  }

  rows_tbl <- dplyr::bind_rows(rows)
  .adaptive_assert_link_stage_budget_invariants(rows_tbl)
  append_link_stage_log(new_link_stage_log(), rows_tbl)
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
    "probe_pred_rmse_lagged", "probe_pred_rmse_max_used", "probe_pred_rmse_pass",
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
  history <- .adaptive_history_tbl(state)
  counts <- .adaptive_pair_counts(history, ids)

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
  new_pairs_since_last_refit <- nrow(committed_subset)
  cross_subset <- committed_subset[committed_subset$is_cross_set %in% TRUE, , drop = FALSE]
  probe_subset <- cross_subset[
    as.character(cross_subset$run_mode) %in% c("link_probe_holdout", "link_probe") |
      cross_subset$is_probe_step %in% TRUE,
    ,
    drop = FALSE
  ]
  active_subset <- cross_subset[
    !(as.character(cross_subset$run_mode) %in% c("link_probe_holdout", "link_probe") |
      cross_subset$is_probe_step %in% TRUE),
    ,
    drop = FALSE
  ]
  new_active_pairs_since_last_refit <- if (isTRUE(phase_b_linking)) {
    as.integer(nrow(active_subset))
  } else {
    NA_integer_
  }
  new_probe_pairs_since_last_refit <- if (isTRUE(phase_b_linking)) {
    as.integer(nrow(probe_subset))
  } else {
    NA_integer_
  }
  new_total_cross_pairs_since_last_refit <- if (isTRUE(phase_b_linking)) {
    as.integer(nrow(cross_subset))
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
  ci95_theta_width_mean <- NA_real_
  ci95_theta_width_median <- NA_real_
  ci95_theta_width_p90 <- NA_real_
  ci95_theta_width_max <- NA_real_
  near_tie_adj_frac <- NA_real_
  near_tie_adj_count <- NA_integer_
  p_adj_median <- NA_real_
  cov_trace_theta <- NA_real_
  cov_logdet_diag_theta <- NA_real_
  post_sd_theta_p10 <- NA_real_
  post_sd_theta_p50 <- NA_real_
  post_sd_theta_p90 <- NA_real_
  top20_boundary_entropy_mean <- NA_real_
  top20_boundary_entropy_p90 <- NA_real_
  nn_diff_sd_mean <- NA_real_
  nn_diff_sd_p90 <- NA_real_

  trueskill_state <- state$trueskill_state %||% NULL
  defaults <- adaptive_defaults(length(ids))
  recent_deg_summary <- .adaptive_recent_deg(history, ids, defaults$W_cap)
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
        ts_btl_theta_corr <- stats::cor(ts_mu, theta_vals, use = "pairwise.complete.obs")
        rank_theta <- rank(theta_vals, ties.method = "average")
        rank_mu <- rank(ts_mu, ties.method = "average")
        ts_btl_rank_spearman <- stats::cor(rank_mu, rank_theta,
          method = "spearman",
          use = "pairwise.complete.obs"
        )
      }
    }
  }

  draws <- fit$btl_posterior_draws %||% NULL
  draw_ids <- character()
  if (is.matrix(draws) && is.numeric(draws)) {
    if (is.null(colnames(draws)) && ncol(draws) == length(ids)) {
      colnames(draws) <- ids
    }
    draw_ids <- intersect(ids, as.character(colnames(draws)))
    if (length(draw_ids) > 0L) {
      draws <- draws[, draw_ids, drop = FALSE]
      draws <- .pairwiseLLM_sanitize_draws_matrix(draws, name = "btl_posterior_draws")
      draw_metric_ids <- intersect(as.character(colnames(draws)), metric_ids)
      if (length(draw_metric_ids) > 0L) {
        draws <- draws[, draw_metric_ids, drop = FALSE]
        draw_ids <- as.character(colnames(draws))
      } else {
        draws <- NULL
      }
    } else {
      draws <- NULL
    }
  }

  if (is.matrix(draws) && is.numeric(draws) && ncol(draws) > 0L) {
    ci_bounds <- apply(
      draws,
      2,
      stats::quantile,
      probs = c(0.025, 0.975),
      names = FALSE
    )
    ci_widths <- ci_bounds[2L, ] - ci_bounds[1L, ]
    ci95_theta_width_mean <- mean(ci_widths)
    ci95_theta_width_median <- stats::median(ci_widths)
    ci95_theta_width_p90 <- stats::quantile(ci_widths, probs = 0.90, names = FALSE)
    ci95_theta_width_max <- max(ci_widths)

    cov_theta <- stats::cov(draws)
    cov_diag <- diag(cov_theta)
    cov_trace_theta <- sum(cov_diag)
    cov_logdet_diag_theta <- sum(log(pmax(cov_diag, .Machine$double.eps)))
    post_sd <- sqrt(pmax(cov_diag, 0))
    post_sd_theta_p10 <- stats::quantile(post_sd, probs = 0.10, names = FALSE)
    post_sd_theta_p50 <- stats::quantile(post_sd, probs = 0.50, names = FALSE)
    post_sd_theta_p90 <- stats::quantile(post_sd, probs = 0.90, names = FALSE)

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
      top20_boundary_entropy_mean <- mean(entropy[boundary_idx])
      top20_boundary_entropy_p90 <- stats::quantile(entropy[boundary_idx], probs = 0.90, names = FALSE)
    }

    theta_for_draws <- NULL
    if (!is.null(theta_map) && length(draw_ids) == ncol(draws) && all(draw_ids %in% names(theta_map))) {
      theta_for_draws <- as.double(theta_map[draw_ids])
    } else if (ncol(draws) >= 1L) {
      theta_for_draws <- as.double(colMeans(draws))
    }
    if (!is.null(theta_for_draws) && length(theta_for_draws) >= 2L) {
      rank_order <- order(-theta_for_draws, draw_ids)
      p_adj <- vapply(seq_len(length(rank_order) - 1L), function(k) {
        lhs <- rank_order[[k]]
        rhs <- rank_order[[k + 1L]]
        mean(draws[, lhs] > draws[, rhs])
      }, numeric(1L))
      near_low <- as.double(config$near_tie_p_low)
      near_high <- as.double(config$near_tie_p_high)
      near_tie <- p_adj >= near_low & p_adj <= near_high
      near_tie_adj_frac <- mean(near_tie)
      near_tie_adj_count <- sum(near_tie)
      p_adj_median <- stats::median(p_adj)

      nn_diff_draws <- draws[, rank_order[-length(rank_order)], drop = FALSE] -
        draws[, rank_order[-1L], drop = FALSE]
      nn_diff_sd <- apply(nn_diff_draws, 2, stats::sd)
      nn_diff_sd_mean <- mean(nn_diff_sd)
      nn_diff_sd_p90 <- stats::quantile(nn_diff_sd, probs = 0.90, names = FALSE)
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
    ci95_theta_width_mean = as.double(ci95_theta_width_mean),
    ci95_theta_width_median = as.double(ci95_theta_width_median),
    ci95_theta_width_p90 = as.double(ci95_theta_width_p90),
    ci95_theta_width_max = as.double(ci95_theta_width_max),
    near_tie_adj_frac = as.double(near_tie_adj_frac),
    near_tie_adj_count = as.integer(near_tie_adj_count),
    p_adj_median = as.double(p_adj_median),
    cov_trace_theta = as.double(cov_trace_theta),
    cov_logdet_diag_theta = as.double(cov_logdet_diag_theta),
    post_sd_theta_p10 = as.double(post_sd_theta_p10),
    post_sd_theta_p50 = as.double(post_sd_theta_p50),
    post_sd_theta_p90 = as.double(post_sd_theta_p90),
    top20_boundary_entropy_mean = as.double(top20_boundary_entropy_mean),
    top20_boundary_entropy_p90 = as.double(top20_boundary_entropy_p90),
    nn_diff_sd_mean = as.double(nn_diff_sd_mean),
    nn_diff_sd_p90 = as.double(nn_diff_sd_p90),
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
    mcmc_cmdstanr_version = as.character(mcmc_config_used$cmdstanr_version %||% NA_character_),
    stop_decision = as.logical(stop_decision),
    stop_reason = if (isTRUE(stop_decision)) as.character(stop_reason) else NA_character_,
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
    cmdstan = config[["cmdstan"]] %||% list()
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
compute_stop_metrics <- function(state, config) {
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
    combined_draws <- .adaptive_phase_b_global_metric_draws(state, controller = controller)
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
