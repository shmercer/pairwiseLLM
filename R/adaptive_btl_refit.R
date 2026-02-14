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
  refit_pairs_target <- config$refit_pairs_target %||% .btl_mcmc_clamp(
    100L,
    5000L,
    as.integer(ceiling(state$n_items / 2))
  )
  as.integer(refit_pairs_target)
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

.adaptive_results_from_step_log <- function(state) {
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
  winner_pos <- ifelse(step_log$Y == 1L, 1L, 2L)
  better_id <- ifelse(step_log$Y == 1L, A_id, B_id)
  controller <- .adaptive_controller_resolve(state)
  run_mode <- as.character(controller$run_mode %||% "within_set")
  is_link_mode <- run_mode %in% c("link_one_spoke", "link_multi_spoke")
  phase_a <- state$linking$phase_a %||% list()
  phase_b_ready <- isTRUE(phase_a$ready_for_phase_b %||% FALSE)
  has_cross <- "is_cross_set" %in% names(step_log)
  is_cross <- if (isTRUE(has_cross)) step_log$is_cross_set %in% TRUE else rep(FALSE, nrow(step_log))
  phase_is_b <- rep(FALSE, nrow(step_log))
  if (isTRUE(is_link_mode) && isTRUE(has_cross)) {
    # Use the first committed cross-set row as the observable phase boundary.
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

.adaptive_link_transform_mode_for_spoke <- function(controller, spoke_id) {
  global_mode <- as.character(controller$link_transform_mode %||% "auto")
  if (!identical(global_mode, "auto")) {
    return(global_mode)
  }
  mode_map <- controller$link_transform_mode_by_spoke %||% list()
  mode <- as.character(mode_map[[as.character(spoke_id)]] %||% "shift_only")
  if (!mode %in% c("shift_only", "shift_scale")) {
    return("shift_only")
  }
  mode
}

.adaptive_link_active_item_ids <- function(state, spoke_id, hub_id) {
  spoke_items <- as.character(state$items$item_id[as.integer(state$items$set_id) == as.integer(spoke_id)])
  hub_items <- as.character(state$items$item_id[as.integer(state$items$set_id) == as.integer(hub_id)])

  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  hub_active_cross <- character()
  if (nrow(step_log) > 0L &&
    all(c("pair_id", "is_cross_set", "link_spoke_id", "set_i", "set_j", "i", "j") %in% names(step_log))) {
    link_spoke <- as.integer(step_log$link_spoke_id)
    cumulative <- step_log[
      !is.na(step_log$pair_id) &
        step_log$is_cross_set %in% TRUE &
        !is.na(link_spoke) &
        link_spoke == as.integer(spoke_id),
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

  hub_anchor <- as.character(state$round$anchor_ids %||% character())
  hub_anchor <- hub_anchor[hub_anchor %in% hub_items]
  active_hub <- unique(c(hub_active_cross, hub_anchor))
  active_all <- unique(c(spoke_items, active_hub))

  list(
    active_all = as.character(active_all),
    active_hub = as.character(active_hub),
    active_spoke = as.character(spoke_items)
  )
}

.adaptive_link_reliability_active <- function(state, active_ids) {
  fit <- state$btl_fit %||% list()
  draws <- fit$btl_posterior_draws %||% NULL
  if (!is.matrix(draws) || !is.numeric(draws) || is.null(colnames(draws))) {
    return(NA_real_)
  }
  ids <- as.character(active_ids)
  ids <- ids[ids %in% colnames(draws)]
  if (length(ids) < 2L) {
    return(NA_real_)
  }
  active_draws <- draws[, ids, drop = FALSE]
  if (ncol(active_draws) < 2L) {
    return(NA_real_)
  }
  as.double(compute_reliability_EAP(active_draws))
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

.adaptive_link_reliability_transformed_active <- function(state,
                                                          active_ids,
                                                          spoke_id,
                                                          hub_id,
                                                          transform_mode,
                                                          delta_mean,
                                                          log_alpha_mean = NA_real_) {
  fit <- state$btl_fit %||% list()
  draws <- fit$btl_posterior_draws %||% NULL
  if (!is.matrix(draws) || !is.numeric(draws) || is.null(colnames(draws))) {
    return(NA_real_)
  }
  ids <- as.character(active_ids)
  ids <- ids[ids %in% colnames(draws)]
  if (length(ids) < 2L) {
    return(NA_real_)
  }
  active_draws <- draws[, ids, drop = FALSE]
  if (ncol(active_draws) < 2L) {
    return(NA_real_)
  }

  mode <- as.character(transform_mode %||% "shift_only")
  if (!mode %in% c("shift_only", "shift_scale")) {
    mode <- "shift_only"
  }
  delta <- as.double(delta_mean %||% NA_real_)
  if (!is.finite(delta)) {
    return(NA_real_)
  }
  alpha <- 1
  if (identical(mode, "shift_scale")) {
    log_alpha <- as.double(log_alpha_mean %||% NA_real_)
    if (!is.finite(log_alpha)) {
      return(NA_real_)
    }
    alpha <- exp(log_alpha)
  }

  item_ids <- as.character(state$items$item_id)
  set_ids <- as.integer(state$items$set_id)
  set_by_item <- stats::setNames(set_ids, item_ids)
  spoke_items <- names(set_by_item)[set_by_item == as.integer(spoke_id)]
  spoke_cols <- colnames(active_draws) %in% spoke_items
  if (any(spoke_cols)) {
    active_draws[, spoke_cols] <- delta + alpha * active_draws[, spoke_cols, drop = FALSE]
  }
  as.double(compute_reliability_EAP(active_draws))
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
    as.character(lag_row$link_transform_mode[[1L]] %||% "shift_only")
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
  # Prefer log-level pass flags when available so stop can be reconstructed from
  # canonical logs without external controller parameters.
  if ("reliability_stop_pass" %in% names(row) &&
    "delta_sd_pass" %in% names(row) &&
    "log_alpha_sd_pass" %in% names(row) &&
    "rank_stability_pass" %in% names(row)) {
    rel_gate <- isTRUE(row$reliability_stop_pass[[1L]])
    delta_sd_gate <- isTRUE(row$delta_sd_pass[[1L]])
    log_alpha_sd_gate <- is.na(row$log_alpha_sd_pass[[1L]]) || isTRUE(row$log_alpha_sd_pass[[1L]])
    delta_change_gate <- isTRUE(row$delta_change_pass[[1L]])
    log_alpha_change_gate <- is.na(row$log_alpha_change_pass[[1L]]) || isTRUE(row$log_alpha_change_pass[[1L]])
    rank_gate <- isTRUE(row$rank_stability_pass[[1L]])
    return(isTRUE(rel_gate) &&
      isTRUE(delta_sd_gate) &&
      isTRUE(log_alpha_sd_gate) &&
      isTRUE(delta_change_gate) &&
      isTRUE(log_alpha_change_gate) &&
      isTRUE(rank_gate))
  }
  rel_gate <- is.finite(row$reliability_EAP_link[[1L]]) &&
    row$reliability_EAP_link[[1L]] >= as.double(controller$link_stop_reliability_min %||% 0.90)
  delta_max <- as.double(controller$delta_sd_max %||% 0.10) * as.double(hub_theta_sd)
  delta_sd_gate <- is.finite(row$delta_spoke_sd[[1L]]) &&
    is.finite(delta_max) &&
    row$delta_spoke_sd[[1L]] <= delta_max
  mode <- as.character(row$link_transform_mode[[1L]] %||% "shift_only")
  alpha_sd_gate <- if (identical(mode, "shift_scale")) {
    is.finite(row$log_alpha_spoke_sd[[1L]]) &&
      row$log_alpha_spoke_sd[[1L]] <= as.double(controller$log_alpha_sd_max %||% 0.10)
  } else {
    TRUE
  }
  delta_change_gate <- isTRUE(row$delta_change_pass[[1L]])
  alpha_change_gate <- if (identical(mode, "shift_scale")) {
    isTRUE(row$log_alpha_change_pass[[1L]])
  } else {
    TRUE
  }
  rank_metric <- NA_real_
  if ("rank_stability_lagged" %in% names(row)) {
    rank_metric <- as.double(row$rank_stability_lagged[[1L]] %||% NA_real_)
  }
  rank_gate <- is.finite(rank_metric) && rank_metric >= 0.98
  isTRUE(rel_gate) &&
    isTRUE(delta_sd_gate) &&
    isTRUE(alpha_sd_gate) &&
    isTRUE(delta_change_gate) &&
    isTRUE(alpha_change_gate) &&
    isTRUE(rank_gate)
}

.adaptive_link_reconstruct_identified_from_logs <- function(link_row, controller) {
  row <- tibble::as_tibble(link_row)
  if (nrow(row) != 1L) {
    rlang::abort("`link_row` must have exactly one row.")
  }
  controller <- utils::modifyList(.adaptive_controller_defaults(2L), controller %||% list())
  rel_gate <- is.finite(row$reliability_EAP_link[[1L]]) &&
    row$reliability_EAP_link[[1L]] >= as.double(controller$link_identified_reliability_min %||% 0.80)
  rank_gate <- isTRUE(row$lag_eligible[[1L]]) && isTRUE(row$rank_stability_pass[[1L]])
  delta_sd_gate <- if ("delta_sd_pass" %in% names(row)) {
    isTRUE(row$delta_sd_pass[[1L]])
  } else {
    FALSE
  }
  mode <- as.character(row$link_transform_mode[[1L]] %||% "shift_only")
  alpha_sd_gate <- if (identical(mode, "shift_scale")) {
    if ("log_alpha_sd_pass" %in% names(row)) {
      isTRUE(row$log_alpha_sd_pass[[1L]])
    } else {
      FALSE
    }
  } else {
    TRUE
  }
  isTRUE(rel_gate) && isTRUE(rank_gate) && isTRUE(delta_sd_gate) && isTRUE(alpha_sd_gate)
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

.adaptive_link_judge_params <- function(state, controller, scope = c("link", "within")) {
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
      if (!is.finite(beta) || !is.finite(epsilon)) {
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
    epsilon = as.double(epsilon)
  )
}

.adaptive_link_cross_edges <- function(state, spoke_id, last_refit_step = NULL) {
  step_log <- tibble::as_tibble(state$step_log %||% tibble::tibble())
  if (nrow(step_log) < 1L) {
    return(tibble::tibble(
      spoke_item = character(),
      hub_item = character(),
      y_spoke = integer(),
      step_id = integer(),
      spoke_in_A = logical()
    ))
  }
  required <- c("pair_id", "step_id", "is_cross_set", "link_spoke_id", "A", "B", "Y")
  if (!all(required %in% names(step_log))) {
    return(tibble::tibble(
      spoke_item = character(),
      hub_item = character(),
      y_spoke = integer(),
      step_id = integer(),
      spoke_in_A = logical()
    ))
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
    return(tibble::tibble(
      spoke_item = character(),
      hub_item = character(),
      y_spoke = integer(),
      step_id = integer(),
      spoke_in_A = logical()
    ))
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
    return(tibble::tibble(
      spoke_item = character(),
      hub_item = character(),
      y_spoke = integer(),
      step_id = integer(),
      spoke_in_A = logical()
    ))
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
    spoke_in_A = as.logical(spoke_is_A)
  )
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

  hub_sd_map <- attr(hub_theta, "theta_sd", exact = TRUE) %||% stats::setNames(numeric(), character())
  spoke_sd_map <- attr(spoke_theta, "theta_sd", exact = TRUE) %||% stats::setNames(numeric(), character())
  edges <- tibble::as_tibble(cross_edges)
  if (nrow(edges) < 1L) {
    empty <- list(
      delta_mean = 0,
      delta_sd = 1,
      log_alpha_mean = if (isTRUE(use_scale)) 0 else NA_real_,
      log_alpha_sd = if (isTRUE(use_scale)) 0.2 else NA_real_
    )
    empty$fit_contract <- list(
      contract_type = "link_refit",
      link_refit_mode = as.character(refit_contract_ctx$link_refit_mode %||% "shift_only"),
      link_transform_mode = as.character(transform_mode),
      parameters = if (isTRUE(use_scale)) c("delta_s", "log_alpha_s") else c("delta_s"),
      priors = list(delta_sd = 1, log_alpha_sd = if (isTRUE(use_scale)) 0.2 else NA_real_),
      judge = list(mode = as.character(judge_params$mode), scope = as.character(judge_params$scope))
    )
    return(empty)
  }
  h <- as.double(hub_theta[as.character(edges$hub_item)])
  s <- as.double(spoke_theta[as.character(edges$spoke_item)])
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
    empty <- list(
      delta_mean = 0,
      delta_sd = 1,
      log_alpha_mean = if (isTRUE(use_scale)) 0 else NA_real_,
      log_alpha_sd = if (isTRUE(use_scale)) 0.2 else NA_real_
    )
    empty$fit_contract <- list(
      contract_type = "link_refit",
      link_refit_mode = as.character(refit_contract_ctx$link_refit_mode %||% "shift_only"),
      link_transform_mode = as.character(transform_mode),
      parameters = if (isTRUE(use_scale)) c("delta_s", "log_alpha_s") else c("delta_s"),
      priors = list(delta_sd = 1, log_alpha_sd = if (isTRUE(use_scale)) 0.2 else NA_real_),
      judge = list(mode = as.character(judge_params$mode), scope = as.character(judge_params$scope))
    )
    return(empty)
  }
  h <- h[keep]
  s <- s[keep]
  hub_sd <- hub_sd[keep]
  spoke_sd <- spoke_sd[keep]
  beta_signed <- beta_signed[keep]
  y <- y[keep]
  nlp <- function(par) {
    delta <- par[[1L]]
    log_alpha <- if (isTRUE(use_scale)) par[[2L]] else 0
    alpha <- exp(log_alpha)
    eta <- delta + alpha * s - h + beta_signed
    p_base <- stats::plogis(eta)
    p <- (1 - epsilon) * p_base + epsilon * 0.5
    p <- pmax(1e-10, pmin(1 - 1e-10, p))
    ll <- sum(stats::dbinom(y, size = 1L, prob = p, log = TRUE))
    prior <- 0.5 * (delta / 1)^2
    if (isTRUE(use_scale)) {
      prior <- prior + 0.5 * (log_alpha / 0.2)^2
    }
    -ll + prior
  }
  start <- if (isTRUE(use_scale)) c(0, 0) else c(0)
  opt <- stats::optim(start, nlp, method = "BFGS", hessian = TRUE)
  par <- opt$par
  vcov <- suppressWarnings(tryCatch(solve(opt$hessian), error = function(e) NULL))
  delta_sd <- 1
  log_alpha_sd <- if (isTRUE(use_scale)) 0.2 else NA_real_
  if (is.matrix(vcov) && nrow(vcov) >= 1L) {
    if (is.finite(vcov[1, 1]) && vcov[1, 1] >= 0) {
      delta_sd <- sqrt(vcov[1, 1])
    }
    if (isTRUE(use_scale) && nrow(vcov) >= 2L && is.finite(vcov[2, 2]) && vcov[2, 2] >= 0) {
      log_alpha_sd <- sqrt(vcov[2, 2])
    }
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
    link_refit_mode = as.character(refit_contract_ctx$link_refit_mode %||% "shift_only"),
    link_transform_mode = as.character(transform_mode),
    parameters = if (isTRUE(use_scale)) c("delta_s", "log_alpha_s") else c("delta_s"),
    priors = list(delta_sd = 1, log_alpha_sd = if (isTRUE(use_scale)) 0.2 else NA_real_),
    judge = list(
      mode = as.character(judge_params$mode %||% "global_shared"),
      scope = as.character(judge_params$scope %||% "link"),
      beta = as.double(beta),
      epsilon = as.double(epsilon)
    ),
    lock = list(
      hub_lock_mode = as.character(refit_contract_ctx$hub_lock_mode %||% NA_character_),
      hub_lock_kappa = as.double(refit_contract_ctx$hub_lock_kappa %||% NA_real_)
    ),
    theta_treatment = as.character(refit_contract_ctx$shift_only_theta_treatment %||% NA_character_)
  )

  list(
    delta_mean = as.double(par[[1L]]),
    delta_sd = as.double(delta_sd),
    log_alpha_mean = if (isTRUE(use_scale)) as.double(par[[2L]]) else NA_real_,
    log_alpha_sd = as.double(log_alpha_sd),
    fit_contract = fit_contract
  )
}

.adaptive_link_ppc_mae_cross <- function(cross_edges,
                                         hub_theta,
                                         spoke_theta,
                                         delta_mean,
                                         log_alpha_mean = NA_real_) {
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
  if (nrow(edges) < 1L) {
    return(NA_real_)
  }
  h <- as.double(hub_theta[as.character(edges$hub_item)])
  s <- as.double(spoke_theta[as.character(edges$spoke_item)])
  spoke_in_A <- as.logical(edges$spoke_in_A %||% rep(TRUE, nrow(edges)))
  beta_sign <- ifelse(spoke_in_A, 1, -1)
  beta_signed <- beta * as.double(beta_sign)
  y <- as.integer(edges$y_spoke)
  keep <- is.finite(h) & is.finite(s) & y %in% c(0L, 1L) & is.finite(beta_signed)
  if (!any(keep)) {
    return(NA_real_)
  }
  alpha <- if (is.finite(log_alpha_mean)) exp(log_alpha_mean) else 1
  eta <- as.double(delta_mean) + alpha * s[keep] - h[keep] + beta_signed[keep]
  p_base <- stats::plogis(eta)
  p <- (1 - epsilon) * p_base + epsilon * 0.5
  as.double(mean(abs(as.double(y[keep]) - p)))
}

.adaptive_link_concurrent_targets <- function(spoke_stats, total_pairs, floor_pairs) {
  if (length(spoke_stats) < 1L) {
    return(integer())
  }
  keys <- names(spoke_stats)
  total_pairs <- as.integer(max(0L, total_pairs))
  floor_pairs <- as.integer(max(0L, floor_pairs))
  n_spokes <- length(spoke_stats)
  base <- rep.int(floor_pairs, n_spokes)
  names(base) <- keys
  rem <- max(0L, total_pairs - sum(base))
  weights <- vapply(spoke_stats, function(x) as.double(x$uncertainty %||% 0), numeric(1L))
  weights[!is.finite(weights) | weights < 0] <- 0
  if (sum(weights) <= 0) {
    weights[] <- 1
  }
  add <- floor(rem * weights / sum(weights))
  out <- base + as.integer(add)
  left <- rem - sum(add)
  if (left > 0L) {
    ord <- order(-weights, keys)
    out[ord[seq_len(left)]] <- out[ord[seq_len(left)]] + 1L
  }
  out <- as.integer(out)
  names(out) <- keys
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
  if (length(phase_ctx$ready_spokes) < 1L) {
    rlang::abort(
      "Phase metadata and routing mode disagree: phase marked phase_b but no ready spokes are available."
    )
  }
  hub_id <- as.integer(controller$hub_id %||% 1L)
  spoke_ids <- .adaptive_link_spoke_ids(out, hub_id)
  spoke_ids <- intersect(spoke_ids, as.integer(phase_ctx$ready_spokes))
  if (length(spoke_ids) < 1L) {
    rlang::abort(
      "Phase B linking cannot continue: no ready spoke has valid Phase A artifact eligibility."
    )
  }
  link_stats <- controller$link_refit_stats_by_spoke %||% list()
  bad_refits <- controller$link_transform_bad_refits_by_spoke %||% list()
  mode_map <- controller$link_transform_mode_by_spoke %||% list()
  last_delta <- controller$link_transform_last_delta_by_spoke %||% list()
  last_log_alpha <- controller$link_transform_last_log_alpha_by_spoke %||% list()
  link_identified_map <- controller$linking_identified_by_spoke %||% list()
  coverage_bins_map <- controller$link_stage_coverage_bins_used %||% list()
  coverage_source_map <- controller$link_stage_coverage_source %||% list()
  last_step <- as.integer(refit_context$last_refit_step %||% 0L)
  current_refit_id <- as.integer(nrow(out$round_log) + 1L)
  link_stage_hist <- tibble::as_tibble(out$link_stage_log %||% new_link_stage_log())

  for (spoke_id in spoke_ids) {
    key <- as.character(spoke_id)
    transform_mode <- .adaptive_link_transform_mode_for_spoke(controller, spoke_id)
    refit_mode <- as.character(controller$link_refit_mode %||% "shift_only")
    lock_mode <- as.character(controller$hub_lock_mode %||% "soft_lock")
    kappa <- as.double(controller$hub_lock_kappa %||% 0.75)
    theta_treatment <- as.character(controller$shift_only_theta_treatment %||% "fixed_eap")

    hub_phase <- .adaptive_link_phase_a_theta_map(out, hub_id, "theta_raw_mean")
    hub_phase_sd <- .adaptive_link_phase_a_theta_map(out, hub_id, "theta_raw_sd")
    spoke_phase <- .adaptive_link_phase_a_theta_map(out, spoke_id, "theta_raw_mean")
    spoke_phase_sd <- .adaptive_link_phase_a_theta_map(out, spoke_id, "theta_raw_sd")
    hub_current <- .adaptive_link_theta_mean_map(out, hub_id)
    hub_current_sd <- .adaptive_link_theta_sd_map(out, hub_id)
    spoke_current <- .adaptive_link_theta_mean_map(out, spoke_id)
    spoke_current_sd <- .adaptive_link_theta_sd_map(out, spoke_id)

    if (identical(refit_mode, "joint_refit")) {
      if (identical(lock_mode, "hard_lock") || isTRUE(kappa == 0)) {
        hub_theta <- hub_phase
        hub_theta_sd <- stats::setNames(rep(0, length(hub_theta)), names(hub_theta))
      } else if (identical(lock_mode, "soft_lock")) {
        ids <- intersect(names(hub_phase), names(hub_current))
        hub_theta <- hub_phase
        hub_theta_sd <- hub_phase_sd
        if (length(ids) > 0L) {
          prior_sd <- as.double(hub_phase_sd[ids]) / max(kappa, 1e-8)
          prior_var <- prior_sd^2
          current_sd <- as.double(hub_current_sd[ids])
          current_sd[!is.finite(current_sd) | current_sd <= 0] <- 1
          current_var <- current_sd^2
          w_prior <- 1 / pmax(prior_var, 1e-8)
          w_current <- 1 / pmax(current_var, 1e-8)
          hub_theta[ids] <- (w_prior * hub_phase[ids] + w_current * hub_current[ids]) / (w_prior + w_current)
          hub_theta_sd[ids] <- sqrt(1 / (w_prior + w_current))
        }
      } else {
        hub_theta <- if (length(hub_current) > 0L) hub_current else hub_phase
        hub_theta_sd <- if (length(hub_current_sd) > 0L) hub_current_sd else hub_phase_sd
      }
      spoke_theta <- if (length(spoke_current) > 0L) spoke_current else spoke_phase
      spoke_theta_sd <- if (length(spoke_current_sd) > 0L) spoke_current_sd else spoke_phase_sd
    } else {
      hub_theta <- hub_phase
      hub_theta_sd <- hub_phase_sd
      if (identical(theta_treatment, "normal_prior")) {
        ids <- intersect(names(spoke_phase), names(spoke_current))
        spoke_theta <- spoke_phase
        spoke_theta_sd <- spoke_phase_sd
        if (length(ids) > 0L) {
          prior_sd <- as.double(spoke_phase_sd[ids])
          prior_sd[!is.finite(prior_sd) | prior_sd <= 0] <- 1
          prior_var <- prior_sd^2
          current_sd <- as.double(spoke_current_sd[ids])
          current_sd[!is.finite(current_sd) | current_sd <= 0] <- 1
          current_var <- current_sd^2
          w_prior <- 1 / pmax(prior_var, 1e-8)
          w_current <- 1 / pmax(current_var, 1e-8)
          spoke_theta[ids] <- (w_prior * spoke_phase[ids] + w_current * spoke_current[ids]) / (w_prior + w_current)
          spoke_theta_sd[ids] <- sqrt(1 / (w_prior + w_current))
        }
      } else {
        spoke_theta <- spoke_phase
        spoke_theta_sd <- spoke_phase_sd
      }
    }

    cross_all <- .adaptive_link_cross_edges(out, spoke_id = spoke_id, last_refit_step = NULL)
    cross_since <- .adaptive_link_cross_edges(out, spoke_id = spoke_id, last_refit_step = last_step)
    judge_params <- .adaptive_link_judge_params(out, controller, scope = "link")
    attr(cross_all, "judge_params") <- judge_params
    attr(cross_since, "judge_params") <- judge_params
    attr(cross_all, "refit_contract") <- list(
      link_refit_mode = refit_mode,
      hub_lock_mode = lock_mode,
      hub_lock_kappa = kappa,
      shift_only_theta_treatment = theta_treatment
    )
    attr(hub_theta, "theta_sd") <- hub_theta_sd
    attr(spoke_theta, "theta_sd") <- spoke_theta_sd
    fit <- .adaptive_link_fit_transform(cross_all, hub_theta, spoke_theta, transform_mode = transform_mode)
    ppc_mae <- .adaptive_link_ppc_mae_cross(
      cross_since,
      hub_theta = hub_theta,
      spoke_theta = spoke_theta,
      delta_mean = fit$delta_mean,
      log_alpha_mean = fit$log_alpha_mean
    )

    escalated_this_refit <- FALSE
    if (identical(as.character(controller$link_transform_mode %||% "auto"), "auto")) {
      if (identical(transform_mode, "shift_only")) {
        bad <- as.integer(bad_refits[[key]] %||% 0L)
        if (is.finite(ppc_mae) && ppc_mae > as.double(controller$cross_set_ppc_mae_max %||% 0.07)) {
          bad <- bad + 1L
        } else {
          bad <- 0L
        }
        need <- as.integer(controller$link_transform_escalation_refits_required %||% 2L)
        if (bad >= need) {
          mode_map[[key]] <- "shift_scale"
          transform_mode <- "shift_scale"
          escalated_this_refit <- TRUE
          # Escalation takes effect in the current refit: recompute transform
          # fit and PPC using shift+scale parameters before gating/logging.
          fit <- .adaptive_link_fit_transform(cross_all, hub_theta, spoke_theta, transform_mode = transform_mode)
          ppc_mae <- .adaptive_link_ppc_mae_cross(
            cross_since,
            hub_theta = hub_theta,
            spoke_theta = spoke_theta,
            delta_mean = fit$delta_mean,
            log_alpha_mean = fit$log_alpha_mean
          )
        }
        bad_refits[[key]] <- bad
      }
      if (identical(transform_mode, "shift_scale") &&
        isTRUE(controller$link_transform_escalation_is_one_way %||% TRUE)) {
        mode_map[[key]] <- "shift_scale"
      }
    }

    lag <- as.integer(out$config$btl_config$stability_lag %||% 2L)
    lag_eligible <- !is.na(lag) && lag >= 1L && current_refit_id > lag
    lag_refit_id <- if (isTRUE(lag_eligible)) as.integer(current_refit_id - lag) else NA_integer_
    lag_row <- tibble::tibble()
    if (isTRUE(lag_eligible) && nrow(link_stage_hist) > 0L) {
      lag_row <- link_stage_hist[
        as.integer(link_stage_hist$refit_id) == lag_refit_id &
          as.integer(link_stage_hist$spoke_id) == as.integer(spoke_id),
        ,
        drop = FALSE
      ]
    }
    lag_delta <- if (nrow(lag_row) > 0L) as.double(lag_row$delta_spoke_mean[[1L]]) else NA_real_
    lag_log_alpha <- if (nrow(lag_row) > 0L) as.double(lag_row$log_alpha_spoke_mean[[1L]]) else NA_real_
    delta_change <- if (is.finite(lag_delta)) abs(fit$delta_mean - lag_delta) else NA_real_
    log_alpha_change <- if (is.finite(lag_log_alpha) && is.finite(fit$log_alpha_mean)) {
      abs(fit$log_alpha_mean - lag_log_alpha)
    } else {
      NA_real_
    }
    delta_change_pass <- if (isTRUE(lag_eligible) && is.finite(delta_change)) {
      delta_change <= as.double(controller$delta_change_max %||% 0.05)
    } else {
      NA
    }
    log_alpha_change_pass <- if (isTRUE(lag_eligible) &&
      identical(transform_mode, "shift_scale") &&
      is.finite(log_alpha_change)) {
      log_alpha_change <= as.double(controller$log_alpha_change_max %||% 0.05)
    } else if (identical(transform_mode, "shift_only")) {
      NA
    } else {
      NA
    }

    delta_sd_max_used <- .adaptive_link_delta_sd_max_derived(
      state = out,
      hub_id = hub_id,
      delta_sd_mult = as.double(controller$delta_sd_max %||% 0.10)
    )
    delta_sd_pass <- is.finite(fit$delta_sd) &&
      is.finite(delta_sd_max_used) &&
      fit$delta_sd <= delta_sd_max_used
    log_alpha_sd_pass <- if (identical(transform_mode, "shift_scale")) {
      is.finite(fit$log_alpha_sd) && fit$log_alpha_sd <= as.double(controller$log_alpha_sd_max %||% 0.10)
    } else {
      TRUE
    }

    if (identical(transform_mode, "shift_scale") && !is.finite(fit$log_alpha_mean)) {
      fit$log_alpha_mean <- 0
    }
    if (identical(transform_mode, "shift_scale") && !is.finite(fit$log_alpha_sd)) {
      fit$log_alpha_sd <- 0.2
    }
    if (identical(transform_mode, "shift_only")) {
      fit$log_alpha_mean <- NA_real_
      fit$log_alpha_sd <- NA_real_
      log_alpha_change <- NA_real_
      log_alpha_change_pass <- NA
    }

    active <- .adaptive_link_active_item_ids(out, spoke_id = spoke_id, hub_id = hub_id)
    reliability_active <- .adaptive_link_reliability_transformed_active(
      state = out,
      active_ids = active$active_all,
      spoke_id = spoke_id,
      hub_id = hub_id,
      transform_mode = transform_mode,
      delta_mean = fit$delta_mean,
      log_alpha_mean = fit$log_alpha_mean
    )
    theta_mean_transformed <- .adaptive_link_transform_theta_mean_for_spoke(
      state = out,
      theta_mean = .adaptive_btl_fit_theta_mean(out$btl_fit %||% list()),
      spoke_id = spoke_id,
      hub_id = hub_id,
      transform_mode = transform_mode,
      delta_mean = fit$delta_mean,
      log_alpha_mean = fit$log_alpha_mean
    )
    ts_btl_rank_active <- .adaptive_link_ts_btl_rank_spearman_active(
      state = out,
      active_ids = active$active_all,
      theta_mean = theta_mean_transformed
    )
    rank_stability <- .adaptive_link_rank_stability_lagged(
      state = out,
      active_ids = active$active_all,
      stability_lag = lag,
      spoke_id = spoke_id,
      hub_id = hub_id,
      transform_mode = transform_mode,
      delta_mean = fit$delta_mean,
      log_alpha_mean = fit$log_alpha_mean,
      lag_row = lag_row
    )

    link_identified <- is.finite(reliability_active) &&
      reliability_active >= as.double(controller$link_identified_reliability_min %||% 0.80) &&
      isTRUE(rank_stability$lag_eligible %||% FALSE) &&
      isTRUE(rank_stability$rho_rank_lagged_pass %||% FALSE) &&
      isTRUE(delta_sd_pass) &&
      (identical(transform_mode, "shift_only") || isTRUE(log_alpha_sd_pass))
    link_identified_map[[key]] <- isTRUE(link_identified)

    last_delta[[key]] <- as.double(fit$delta_mean)
    last_log_alpha[[key]] <- as.double(fit$log_alpha_mean %||% NA_real_)
    link_stats[[key]] <- list(
      link_transform_mode = as.character(transform_mode),
      delta_spoke_mean = as.double(fit$delta_mean),
      delta_spoke_sd = as.double(fit$delta_sd),
      log_alpha_spoke_mean = as.double(fit$log_alpha_mean),
      log_alpha_spoke_sd = as.double(fit$log_alpha_sd),
      delta_change_lagged = as.double(delta_change),
      log_alpha_change_lagged = as.double(log_alpha_change),
      delta_change_pass = as.logical(delta_change_pass),
      log_alpha_change_pass = as.logical(log_alpha_change_pass),
      delta_sd_pass = as.logical(delta_sd_pass),
      log_alpha_sd_pass = as.logical(log_alpha_sd_pass),
      delta_sd_max_used = as.double(delta_sd_max_used),
      link_reliability = as.double(reliability_active),
      link_reliability_identified_pass = as.logical(
        is.finite(reliability_active) &&
          reliability_active >= as.double(controller$link_identified_reliability_min %||% 0.80)
      ),
      link_reliability_stop_pass = as.logical(
        is.finite(reliability_active) &&
          reliability_active >= as.double(controller$link_stop_reliability_min %||% 0.90)
      ),
      ts_btl_rank_spearman_active = as.double(ts_btl_rank_active),
      link_rank_corr_pass = as.logical(
        isTRUE(rank_stability$lag_eligible %||% FALSE) &&
          isTRUE(rank_stability$rho_rank_lagged_pass %||% FALSE)
      ),
      lag_eligible = as.logical(rank_stability$lag_eligible %||% FALSE),
      rank_stability_lagged = as.double(rank_stability$rho_rank_lagged %||% NA_real_),
      rank_stability_pass = as.logical(rank_stability$rho_rank_lagged_pass %||% FALSE),
      link_identified = as.logical(link_identified),
      ppc_mae_cross = as.double(ppc_mae),
      fit_contract = fit$fit_contract %||% list(),
      escalated_this_refit = as.logical(escalated_this_refit),
      n_cross_edges_since_last_refit = as.integer(nrow(cross_since)),
      coverage_bins_used = as.integer(coverage_bins_map[[key]] %||% NA_integer_),
      coverage_source = as.character(coverage_source_map[[key]] %||% NA_character_),
      active_item_count_hub = as.integer(length(active$active_hub)),
      active_item_count_spoke = as.integer(length(active$active_spoke)),
      uncertainty = as.double(fit$delta_sd + if (is.finite(fit$log_alpha_sd)) fit$log_alpha_sd else 0)
    )
  }

  if (identical(as.character(controller$multi_spoke_mode %||% "independent"), "concurrent")) {
    total_since <- sum(vapply(
      link_stats,
      function(x) as.integer(x$n_cross_edges_since_last_refit %||% 0L),
      integer(1L)
    ))
    floor_pairs <- as.integer(controller$min_cross_set_pairs_per_spoke_per_refit %||% 5L)
    targets <- .adaptive_link_concurrent_targets(link_stats, total_pairs = total_since, floor_pairs = floor_pairs)
    keys <- names(targets)
    for (key in keys) {
      stats_row <- link_stats[[key]] %||% list()
      obs <- as.integer(stats_row$n_cross_edges_since_last_refit %||% 0L)
      tgt <- as.integer(targets[[key]])
      stats_row$concurrent_target_pairs <- tgt
      stats_row$concurrent_floor_pairs <- floor_pairs
      stats_row$concurrent_floor_met <- obs >= floor_pairs
      stats_row$concurrent_target_met <- obs >= tgt
      link_stats[[key]] <- stats_row
    }
  }

  controller$link_refit_stats_by_spoke <- link_stats
  controller$link_transform_bad_refits_by_spoke <- bad_refits
  controller$link_transform_mode_by_spoke <- mode_map
  controller$link_transform_last_delta_by_spoke <- last_delta
  controller$link_transform_last_log_alpha_by_spoke <- last_log_alpha
  controller$linking_identified_by_spoke <- link_identified_map
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
  if (length(phase_ctx$ready_spokes) < 1L) {
    rlang::abort(
      "Phase metadata and routing mode disagree: phase marked phase_b but no ready spokes are available."
    )
  }

  hub_id <- as.integer(controller$hub_id %||% 1L)
  spoke_ids <- .adaptive_link_spoke_ids(state, hub_id = hub_id)
  spoke_ids <- intersect(spoke_ids, as.integer(phase_ctx$ready_spokes))
  if (length(spoke_ids) < 1L) {
    rlang::abort(
      "Phase B link-stage logging cannot proceed: no ready spoke has valid Phase A artifact eligibility."
    )
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
  proxy <- .adaptive_rank_proxy(state)

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

    n_pairs_done <- as.integer(nrow(cumulative))
    n_pairs_since <- as.integer(nrow(since_last))
    quota_controller <- controller
    quota_controller$current_link_spoke_id <- as.integer(spoke_id)
    stage_quotas <- .adaptive_round_compute_quotas(
      round_id = as.integer(state$round$round_id %||% 1L),
      n_items = as.integer(state$n_items),
      controller = quota_controller
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
    refit_step_end <- if ("step_id" %in% names(step_log) && nrow(step_log) > 0L) {
      as.integer(max(as.integer(step_log$step_id), na.rm = TRUE))
    } else {
      0L
    }
    refit_step_start <- as.integer(refit_context$last_refit_step %||% 0L)
    if (nrow(step_log) > 0L && all(c("pair_id", "round_stage", "step_id", "link_spoke_id", "is_cross_set") %in%
      names(step_log))) {
      stage_rows <- step_log[
        !is.na(step_log$pair_id) &
          step_log$is_cross_set %in% TRUE &
          as.integer(step_log$step_id) > refit_step_start &
          as.integer(step_log$step_id) <= refit_step_end &
          as.integer(step_log$link_spoke_id) == as.integer(spoke_id) &
          as.character(step_log$round_stage) %in% stage_order,
        ,
        drop = FALSE
      ]
      if (nrow(stage_rows) > 0L) {
        tab_stage <- table(factor(as.character(stage_rows$round_stage), levels = stage_order))
        committed_stage[names(tab_stage)] <- as.integer(tab_stage)
      }
    }
    n_unique <- 0L
    if (nrow(cumulative) > 0L && all(c("A", "B") %in% names(cumulative))) {
      ids <- as.character(state$item_ids)
      a_id <- ids[as.integer(cumulative$A)]
      b_id <- ids[as.integer(cumulative$B)]
      n_unique <- as.integer(length(unique(make_unordered_key(a_id, b_id))))
    }

    spoke_items <- as.character(state$items$item_id[as.integer(state$items$set_id) == spoke_id])

    spoke_scores <- proxy$scores[spoke_items]
    coverage <- .adaptive_link_spoke_coverage(
      state = state,
      controller = controller,
      spoke_id = spoke_id,
      spoke_ids = spoke_items,
      proxy_scores = spoke_scores
    )

    reliability_stop_pass <- isTRUE(stats_row$link_reliability_stop_pass %||% FALSE)
    delta_sd_pass <- isTRUE(stats_row$delta_sd_pass %||% FALSE)
    log_alpha_sd_pass <- is.na(stats_row$log_alpha_sd_pass %||% NA) || isTRUE(stats_row$log_alpha_sd_pass)
    lag_eligible <- isTRUE(stats_row$lag_eligible %||% FALSE)
    delta_change_pass <- isTRUE(stats_row$delta_change_pass %||% FALSE)
    log_alpha_change_pass <- is.na(stats_row$log_alpha_change_pass %||% NA) || isTRUE(stats_row$log_alpha_change_pass)
    rank_stability_pass <- isTRUE(stats_row$rank_stability_pass %||% FALSE)
    link_stop_eligible <- isTRUE(lag_eligible) && !is.na(diagnostics_pass)
    link_stop_pass <- isTRUE(link_stop_eligible) &&
      isTRUE(diagnostics_pass) &&
      isTRUE(reliability_stop_pass) &&
      isTRUE(delta_sd_pass) &&
      isTRUE(log_alpha_sd_pass) &&
      isTRUE(delta_change_pass) &&
      isTRUE(log_alpha_change_pass) &&
      isTRUE(rank_stability_pass)

    rows[[idx]] <- list(
      refit_id = as.integer(refit_id),
      spoke_id = as.integer(spoke_id),
      hub_id = as.integer(hub_id),
      link_transform_mode = as.character(stats_row$link_transform_mode %||%
        .adaptive_link_transform_mode_for_spoke(controller, spoke_id)),
      link_refit_mode = as.character(controller$link_refit_mode %||% NA_character_),
      hub_lock_mode = as.character(controller$hub_lock_mode %||% NA_character_),
      hub_lock_kappa = as.double(controller$hub_lock_kappa %||% NA_real_),
      delta_spoke_mean = as.double(stats_row$delta_spoke_mean %||% NA_real_),
      delta_spoke_sd = as.double(stats_row$delta_spoke_sd %||% NA_real_),
      log_alpha_spoke_mean = as.double(stats_row$log_alpha_spoke_mean %||% NA_real_),
      log_alpha_spoke_sd = as.double(stats_row$log_alpha_spoke_sd %||% NA_real_),
      delta_change_lagged = as.double(stats_row$delta_change_lagged %||% NA_real_),
      log_alpha_change_lagged = as.double(stats_row$log_alpha_change_lagged %||% NA_real_),
      delta_change_pass = as.logical(stats_row$delta_change_pass %||% NA),
      log_alpha_change_pass = as.logical(stats_row$log_alpha_change_pass %||% NA),
      delta_sd_max_used = as.double(stats_row$delta_sd_max_used %||% NA_real_),
      delta_sd_pass = as.logical(stats_row$delta_sd_pass %||% NA),
      log_alpha_sd_pass = as.logical(stats_row$log_alpha_sd_pass %||% NA),
      reliability_EAP_link = as.double(stats_row$link_reliability %||% NA_real_),
      reliability_stop_pass = as.logical(stats_row$link_reliability_stop_pass %||% NA),
      linking_identified = as.logical(linking_identified),
      lag_eligible = as.logical(stats_row$lag_eligible %||% FALSE),
      rank_stability_lagged = as.double(stats_row$rank_stability_lagged %||% NA_real_),
      rank_stability_pass = as.logical(stats_row$rank_stability_pass %||% FALSE),
      link_stop_eligible = as.logical(link_stop_eligible),
      link_stop_pass = as.logical(link_stop_pass),
      ts_btl_rank_spearman = as.double(stats_row$ts_btl_rank_spearman_active %||% NA_real_),
      ppc_mae_cross = as.double(stats_row$ppc_mae_cross %||% NA_real_),
      escalated_this_refit = as.logical(stats_row$escalated_this_refit %||% FALSE),
      n_pairs_cross_set_done = as.integer(n_pairs_done),
      n_unique_cross_pairs_seen = as.integer(n_unique),
      n_cross_edges_since_last_refit = as.integer(n_pairs_since),
      quota_anchor_link = as.integer(stage_quotas[["anchor_link"]] %||% NA_integer_),
      quota_long_link = as.integer(stage_quotas[["long_link"]] %||% NA_integer_),
      quota_mid_link = as.integer(stage_quotas[["mid_link"]] %||% NA_integer_),
      quota_local_link = as.integer(stage_quotas[["local_link"]] %||% NA_integer_),
      quota_long_link_raw = as.integer(quota_long_link_raw),
      quota_long_link_effective = as.integer(quota_long_link_effective),
      quota_long_link_removed = as.integer(quota_long_link_removed),
      quota_taper_applied = as.logical(quota_taper_applied),
      quota_taper_spoke_id = as.integer(quota_taper_spoke_id),
      committed_anchor_link = as.integer(committed_stage[["anchor_link"]] %||% 0L),
      committed_long_link = as.integer(committed_stage[["long_link"]] %||% 0L),
      committed_mid_link = as.integer(committed_stage[["mid_link"]] %||% 0L),
      committed_local_link = as.integer(committed_stage[["local_link"]] %||% 0L),
      concurrent_target_pairs = as.integer(stats_row$concurrent_target_pairs %||% NA_integer_),
      concurrent_floor_pairs = as.integer(stats_row$concurrent_floor_pairs %||% NA_integer_),
      concurrent_floor_met = as.logical(stats_row$concurrent_floor_met %||% NA),
      concurrent_target_met = as.logical(stats_row$concurrent_target_met %||% NA),
      active_item_count_hub = as.integer(stats_row$active_item_count_hub %||% NA_integer_),
      active_item_count_spoke = as.integer(stats_row$active_item_count_spoke %||% NA_integer_),
      coverage_bins_used = as.integer(stats_row$coverage_bins_used %||% coverage$bins_used %||% NA_integer_),
      coverage_source = as.character(stats_row$coverage_source %||% coverage$source %||% NA_character_)
    )
  }

  rows_tbl <- dplyr::bind_rows(rows)
  append_link_stage_log(new_link_stage_log(), rows_tbl)
}

#' @keywords internal
#' @noRd
.adaptive_assert_link_stage_rows_completeness <- function(link_rows) {
  rows <- tibble::as_tibble(link_rows)
  if (nrow(rows) < 1L) {
    return(invisible(TRUE))
  }
  required <- c(
    "refit_id", "spoke_id", "hub_id", "link_transform_mode", "link_refit_mode",
    "hub_lock_mode", "reliability_EAP_link", "linking_identified", "link_stop_eligible", "link_stop_pass",
    "n_pairs_cross_set_done", "n_unique_cross_pairs_seen", "n_cross_edges_since_last_refit", "coverage_bins_used"
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
  mode_na <- rows[
    is.na(rows$link_transform_mode) | is.na(rows$link_refit_mode) | is.na(rows$hub_lock_mode),
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
  history <- .adaptive_history_tbl(state)
  counts <- .adaptive_pair_counts(history, ids)

  deg_vals <- as.double(counts$deg[ids])
  mean_degree <- if (length(deg_vals) > 0L) mean(deg_vals) else NA_real_
  min_degree <- if (length(deg_vals) > 0L) min(deg_vals) else NA_integer_
  pos_balance <- as.double(counts$posA[ids] - counts$posB[ids])
  pos_balance_sd <- if (length(pos_balance) > 1L) stats::sd(pos_balance) else 0

  n_unique_pairs_seen <- sum(counts$pair_count >= 1L)
  total_pairs_done <- nrow(history)
  new_pairs_since_last_refit <- total_pairs_done - refit_context$last_refit_M_done

  last_step <- refit_context$last_refit_step
  step_id_at_refit <- refit_context$step_id_at_refit
  step_log <- tibble::as_tibble(state$step_log)
  step_subset <- step_log[step_log$step_id > last_step &
    step_log$step_id <= step_id_at_refit, , drop = FALSE]

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
  if (length(theta_mean) == length(ids)) {
    theta_mean <- as.double(theta_mean[ids])
  } else {
    theta_mean <- NULL
  }

  if (!is.null(theta_mean) && !is.null(trueskill_state)) {
    ts_mu <- trueskill_state$items$mu[match(ids, trueskill_state$items$item_id)]
    if (length(ts_mu) == length(theta_mean)) {
      ts_btl_theta_corr <- stats::cor(ts_mu, theta_mean, use = "pairwise.complete.obs")
      rank_theta <- rank(theta_mean, ties.method = "average")
      rank_mu <- rank(ts_mu, ties.method = "average")
      ts_btl_rank_spearman <- stats::cor(rank_mu, rank_theta,
        method = "spearman",
        use = "pairwise.complete.obs"
      )
    }
  }

  draws <- fit$btl_posterior_draws %||% NULL
  if (is.matrix(draws) && is.numeric(draws)) {
    if (is.null(colnames(draws))) {
      colnames(draws) <- ids
    }
    draws <- draws[, ids, drop = FALSE]
    draws <- .pairwiseLLM_sanitize_draws_matrix(draws, name = "btl_posterior_draws")

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

    if (!is.null(theta_mean) && length(theta_mean) == length(ids) && length(ids) >= 2L) {
      rank_order <- order(-theta_mean, ids)
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
  controller <- .adaptive_controller_resolve(state)
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
    mean_degree = as.double(mean_degree),
    min_degree = as.integer(min_degree),
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
    eap_reliability_min = as.double(metrics$eap_reliability_min %||% NA_real_),
    eap_pass = as.logical(metrics$eap_pass %||% NA),
    theta_sd_eap = as.double(metrics$theta_sd_eap %||% NA_real_),
    rho_theta = as.double(metrics$rho_theta %||% NA_real_),
    lag_eligible = as.logical(metrics$lag_eligible %||% NA),
    theta_corr_min = as.double(metrics$theta_corr_min %||% NA_real_),
    theta_corr_pass = as.logical(metrics$theta_corr_pass %||% NA),
    delta_sd_theta = as.double(metrics$delta_sd_theta %||% NA_real_),
    theta_sd_rel_change_max = as.double(metrics$theta_sd_rel_change_max %||% NA_real_),
    delta_sd_theta_pass = as.logical(metrics$delta_sd_theta_pass %||% NA),
    rho_rank = as.double(metrics$rho_rank %||% NA_real_),
    rank_spearman_min = as.double(metrics$rank_spearman_min %||% NA_real_),
    rho_rank_pass = as.logical(metrics$rho_rank_pass %||% NA),
    mcmc_chains = as.integer(mcmc_config_used$chains %||% NA_integer_),
    mcmc_parallel_chains = as.integer(mcmc_config_used$parallel_chains %||% NA_integer_),
    mcmc_core_fraction = as.double(mcmc_config_used$core_fraction %||% NA_real_),
    mcmc_cores_detected_physical = as.integer(mcmc_config_used$cores_detected_physical %||% NA_integer_),
    mcmc_cores_detected_logical = as.integer(mcmc_config_used$cores_detected_logical %||% NA_integer_),
    mcmc_threads_per_chain = as.integer(mcmc_config_used$threads_per_chain %||% NA_integer_),
    mcmc_cmdstanr_version = as.character(mcmc_config_used$cmdstanr_version %||% NA_character_),
    stop_decision = as.logical(stop_decision),
    stop_reason = if (isTRUE(stop_decision)) as.character(stop_reason) else NA_character_
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
  results <- .adaptive_results_from_step_log(state)
  if (nrow(results) < 1L) {
    rlang::abort("BTL refit requires at least one committed comparison.")
  }

  fit_out <- fit_bayes_btl_mcmc(
    results = results,
    ids = as.character(state$item_ids),
    model_variant = config$model_variant %||% "btl_e_b",
    cmdstan = config$cmdstan %||% list()
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

  M_done <- as.integer(nrow(state$history_pairs))
  last_refit_M_done <- state$refit_meta$last_refit_M_done %||% 0L
  last_refit_step <- state$refit_meta$last_refit_step %||% 0L

  refit_pairs_target <- .adaptive_refit_pairs_target(state, config)
  config$refit_pairs_target <- refit_pairs_target
  eligibility <- .adaptive_refit_eligibility(
    total_committed = M_done,
    last_refit_committed = last_refit_M_done,
    refit_pairs_target = refit_pairs_target
  )
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

  refit_context <- .adaptive_btl_refit_context(state, last_refit_M_done, last_refit_step)

  state$btl_fit <- fit
  state$refit_meta$last_refit_M_done <- M_done
  state$refit_meta$last_refit_step <- refit_context$step_id_at_refit
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

  theta_mean <- .adaptive_btl_fit_theta_mean(fit)
  theta_mean <- as.double(theta_mean)
  theta_sd_eap <- stats::sd(theta_mean)
  reliability_EAP <- compute_reliability_EAP(draws)

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

  history <- state$refit_meta$theta_mean_history %||% list()
  current_refit <- length(history)
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

  if (isTRUE(lag_eligible)) {
    lag_idx <- current_refit - stability_lag
    lag_theta <- history[[lag_idx]]
    if (length(lag_theta) == length(theta_mean)) {
      lag_theta <- as.double(lag_theta)
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
  }

  list(
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
    eap_reliability_min = eap_min,
    eap_pass = eap_pass,
    theta_sd_eap = theta_sd_eap,
    rho_theta = rho_theta,
    theta_corr_min = as.double(config$theta_corr_min),
    theta_corr_pass = theta_corr_pass,
    delta_sd_theta = delta_sd_theta,
    theta_sd_rel_change_max = as.double(config$theta_sd_rel_change_max),
    delta_sd_theta_pass = delta_sd_theta_pass,
    rho_rank = rho_rank,
    rank_spearman_min = as.double(config$rank_spearman_min),
    rho_rank_pass = rho_rank_pass,
    lag_eligible = lag_eligible
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
  threshold <- eap_min - 0.05
  if (is.finite(metrics$reliability_EAP) && metrics$reliability_EAP >= threshold) {
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
  eap_min <- as.double(config$eap_reliability_min)
  if (!is.finite(metrics$reliability_EAP) || metrics$reliability_EAP < eap_min) {
    return(FALSE)
  }
  if (!isTRUE(metrics$lag_eligible)) {
    return(FALSE)
  }

  theta_corr_min <- as.double(config$theta_corr_min)
  theta_sd_rel_change_max <- as.double(config$theta_sd_rel_change_max)
  rank_spearman_min <- as.double(config$rank_spearman_min)

  if (!is.finite(metrics$rho_theta) || metrics$rho_theta < theta_corr_min) {
    return(FALSE)
  }
  if (!is.finite(metrics$delta_sd_theta) || metrics$delta_sd_theta > theta_sd_rel_change_max) {
    return(FALSE)
  }
  if (!is.finite(metrics$rho_rank) || metrics$rho_rank < rank_spearman_min) {
    return(FALSE)
  }

  TRUE
}
