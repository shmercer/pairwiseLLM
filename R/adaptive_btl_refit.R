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
                                                    lag_row) {
  ids <- as.character(scope_ids)
  if (length(ids) < 2L) {
    return(NA_real_)
  }
  history <- state$refit_meta$theta_mean_history %||% list()
  current_refit <- length(history)
  if (current_refit < 1L) {
    return(NA_real_)
  }
  current_raw <- history[[current_refit]]
  lag_raw <- history[[max(1L, current_refit - 1L)]]
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
  rel_gate <- if ("reliability_stop_pass" %in% names(row)) {
    isTRUE(row$reliability_stop_pass[[1L]] %||% FALSE)
  } else if ("reliability_EAP_link" %in% names(row)) {
    is.finite(row$reliability_EAP_link[[1L]]) &&
      row$reliability_EAP_link[[1L]] >= as.double(controller$link_stop_reliability_min %||% 0.90)
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
      as.double(row$probe_brier[[1L]]) <= as.double(controller$probe_brier_max %||% 0.19)
  } else {
    FALSE
  }
  probe_rmse_gate <- if ("probe_pred_rmse_lagged" %in% names(row)) {
    is.finite(as.double(row$probe_pred_rmse_lagged[[1L]] %||% NA_real_)) &&
      as.double(row$probe_pred_rmse_lagged[[1L]]) <= as.double(controller$probe_pred_rmse_max %||% 0.015)
    } else {
      FALSE
    }
  theta_rmse_gate <- if ("theta_global_rmse_lagged" %in% names(row)) {
    is.finite(as.double(row$theta_global_rmse_lagged[[1L]] %||% NA_real_)) &&
      as.double(row$theta_global_rmse_lagged[[1L]]) <= as.double(controller$theta_global_rmse_max %||% 0.04)
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
  rel_gate <- is.finite(row$reliability_EAP_link[[1L]]) &&
    row$reliability_EAP_link[[1L]] >= as.double(controller$link_identified_reliability_min %||% 0.80)
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

.adaptive_mcmc_rhat <- function(chains) {
  mat <- as.matrix(chains)
  n <- nrow(mat)
  m <- ncol(mat)
  if (n < 2L || m < 2L) {
    return(NA_real_)
  }
  chain_means <- colMeans(mat)
  w <- mean(apply(mat, 2L, stats::var))
  if (!is.finite(w) || w <= 0) {
    return(NA_real_)
  }
  b <- n * stats::var(chain_means)
  var_hat <- ((n - 1) / n) * w + (b / n)
  as.double(sqrt(var_hat / w))
}

.adaptive_mcmc_ess_bulk <- function(chains) {
  mat <- as.matrix(chains)
  n <- nrow(mat)
  m <- ncol(mat)
  if (n < 3L || m < 1L) {
    return(NA_real_)
  }
  ess_sum <- 0
  for (j in seq_len(m)) {
    x <- as.double(mat[, j])
    if (!all(is.finite(x)) || stats::sd(x) == 0) {
      next
    }
    max_lag <- min(100L, n - 1L)
    acf_vals <- stats::acf(x, lag.max = max_lag, plot = FALSE)$acf[-1L]
    acf_vals[!is.finite(acf_vals)] <- 0
    if (length(acf_vals) >= 2L) {
      odd <- acf_vals[seq.int(1L, length(acf_vals), by = 2L)]
      even <- acf_vals[seq.int(2L, length(acf_vals), by = 2L)]
      n_pair <- min(length(odd), length(even))
      pair_sums <- if (n_pair > 0L) {
        odd[seq_len(n_pair)] + even[seq_len(n_pair)]
      } else {
        numeric()
      }
      if (length(pair_sums) > 0L && any(is.finite(pair_sums))) {
        first_bad <- which(pair_sums < 0)[1L]
        if (!is.na(first_bad)) {
          pair_sums <- pair_sums[seq_len(max(0L, first_bad - 1L))]
        }
      }
      tau <- 1 + 2 * sum(pair_sums[pair_sums > 0], na.rm = TRUE)
    } else {
      tau <- 1
    }
    tau <- max(1, tau)
    ess_sum <- ess_sum + (n / tau)
  }
  as.double(ess_sum)
}

.adaptive_link_mcmc_diagnostics <- function(chain_draws, param_names) {
  arr <- array(chain_draws, dim = dim(chain_draws), dimnames = dimnames(chain_draws))
  if (length(dim(arr)) != 3L) {
    return(list(
      divergences = NA_integer_,
      max_rhat = NA_real_,
      min_ess_bulk = NA_real_,
      diagnostics_divergences_pass = NA,
      diagnostics_rhat_pass = NA,
      diagnostics_ess_pass = NA
    ))
  }
  n_par <- dim(arr)[3L]
  rhat <- rep(NA_real_, n_par)
  ess <- rep(NA_real_, n_par)
  for (k in seq_len(n_par)) {
    mat <- arr[, , k, drop = TRUE]
    rhat[[k]] <- .adaptive_mcmc_rhat(mat)
    ess[[k]] <- .adaptive_mcmc_ess_bulk(mat)
  }
  max_rhat <- max(rhat, na.rm = TRUE)
  min_ess <- min(ess, na.rm = TRUE)
  if (!is.finite(max_rhat)) {
    max_rhat <- NA_real_
  }
  if (!is.finite(min_ess)) {
    min_ess <- NA_real_
  }
  list(
    param_names = as.character(param_names),
    rhat = as.double(rhat),
    ess_bulk = as.double(ess),
    divergences = NA_integer_,
    max_rhat = as.double(max_rhat),
    min_ess_bulk = as.double(min_ess),
    diagnostics_divergences_pass = NA,
    diagnostics_rhat_pass = if (is.finite(max_rhat)) max_rhat <= 1.05 else NA,
    diagnostics_ess_pass = if (is.finite(min_ess)) min_ess >= 100 else NA
  )
}

.adaptive_link_mcmc_sample <- function(log_post_fn,
                                       init,
                                       seed,
                                       n_chains = 4L,
                                       n_warmup = 120L,
                                       n_samples = 180L) {
  init <- as.double(init)
  n_param <- length(init)
  if (n_param < 1L) {
    rlang::abort("Linking Bayesian refit requires at least one parameter.")
  }
  n_chains <- max(2L, as.integer(n_chains))
  n_warmup <- max(0L, as.integer(n_warmup))
  n_samples <- max(20L, as.integer(n_samples))
  total_iter <- as.integer(n_warmup + n_samples)
  chain_draws <- array(
    NA_real_,
    dim = c(n_samples, n_chains, n_param),
    dimnames = list(NULL, paste0("chain_", seq_len(n_chains)), NULL)
  )
  accept_rates <- rep(NA_real_, n_chains)

  for (chain in seq_len(n_chains)) {
    chain_seed <- as.integer((seed + chain * 101L) %% .Machine$integer.max)
    res <- withr::with_seed(chain_seed, {
      cur <- init + stats::rnorm(n_param, sd = 0.05)
      cur_lp <- as.double(log_post_fn(cur))
      tries <- 0L
      while ((!is.finite(cur_lp)) && tries < 200L) {
        tries <- tries + 1L
        cur <- init + stats::rnorm(n_param, sd = 0.25)
        cur_lp <- as.double(log_post_fn(cur))
      }
      if (!is.finite(cur_lp)) {
        rlang::abort("Linking Bayesian refit failed to initialize a finite posterior state.")
      }

      step_sd <- rep(0.08, n_param)
      accepted <- 0L
      accept_window <- 0L
      draws <- matrix(NA_real_, nrow = n_samples, ncol = n_param)
      keep_idx <- 0L
      for (iter in seq_len(total_iter)) {
        prop <- cur + stats::rnorm(n_param, sd = step_sd)
        prop_lp <- as.double(log_post_fn(prop))
        accept <- is.finite(prop_lp) &&
          log(stats::runif(1L)) < (prop_lp - cur_lp)
        if (isTRUE(accept)) {
          cur <- prop
          cur_lp <- prop_lp
          accepted <- accepted + 1L
          accept_window <- accept_window + 1L
        }
        if (iter <= n_warmup && (iter %% 20L) == 0L) {
          rate <- accept_window / 20
          if (rate > 0.35) {
            step_sd <- step_sd * 1.15
          } else if (rate < 0.20) {
            step_sd <- step_sd * 0.85
          }
          accept_window <- 0L
        }
        if (iter > n_warmup) {
          keep_idx <- keep_idx + 1L
          draws[keep_idx, ] <- cur
        }
      }
      list(draws = draws, accept_rate = accepted / total_iter)
    })
    chain_draws[, chain, ] <- res$draws
    accept_rates[[chain]] <- res$accept_rate
  }

  list(
    chain_draws = chain_draws,
    draws = matrix(chain_draws, ncol = n_param),
    accept_rate = as.double(mean(accept_rates))
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
      estimation_method = "bayesian_mcmc",
      uncertainty_approximation = "posterior_draws",
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
  n_hub_items_estimated <- 0L
  n_spoke_items_estimated <- 0L

  param_names <- character()
  start <- numeric()
  log_post <- NULL
  idx_delta <- NA_integer_
  idx_log_alpha <- NA_integer_
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
    n_h <- length(fit_hub_idx)
    n_s <- length(fit_spoke_idx)
    idx_delta <- n_h + n_s + 1L
    idx_log_alpha <- if (isTRUE(use_scale)) idx_delta + 1L else NA_integer_
    n_par <- n_h + n_s + 1L + ifelse(isTRUE(use_scale), 1L, 0L)
    start <- rep(0, n_par)
    if (n_h > 0L) {
      start[seq_len(n_h)] <- hub_init[fit_hub_idx]
    }
    if (n_s > 0L) {
      start[n_h + seq_len(n_s)] <- spoke_init[fit_spoke_idx]
    }
    param_names <- c(
      if (n_h > 0L) paste0("theta_hub_", seq_len(n_h)) else character(),
      if (n_s > 0L) paste0("theta_spoke_", seq_len(n_s)) else character(),
      "delta",
      if (isTRUE(use_scale)) "log_alpha" else character()
    )
    hub_lut <- stats::setNames(seq_along(hub_ref), names(hub_ref))
    spoke_lut <- stats::setNames(seq_along(spoke_ref), names(spoke_ref))
    log_post <- function(par) {
      hub_val <- hub_ref
      if (n_h > 0L) {
        hub_val[fit_hub_idx] <- par[seq_len(n_h)]
      }
      spoke_val <- spoke_ref
      if (n_s > 0L) {
        spoke_val[fit_spoke_idx] <- par[n_h + seq_len(n_s)]
      }
      delta <- par[[idx_delta]]
      log_alpha <- if (isTRUE(use_scale)) par[[idx_log_alpha]] else 0
      alpha <- exp(log_alpha)
      eta_cross <- delta +
        alpha * spoke_val[as.character(edges_obs$spoke_item)] -
        hub_val[as.character(edges_obs$hub_item)] +
        beta_signed
      p_cross <- (1 - epsilon) * stats::plogis(eta_cross) + epsilon * 0.5
      p_cross <- pmax(1e-10, pmin(1 - 1e-10, p_cross))
      ll <- sum(stats::dbinom(y, size = 1L, prob = p_cross, log = TRUE))

      if (nrow(within_hub) > 0L) {
        idx_a <- hub_lut[as.character(within_hub$A_item)]
        idx_b <- hub_lut[as.character(within_hub$B_item)]
        y_a <- as.integer(within_hub$y_A)
        keep_h <- !is.na(idx_a) & !is.na(idx_b) & y_a %in% c(0L, 1L)
        if (any(keep_h)) {
          eta_h <- hub_val[idx_a[keep_h]] - hub_val[idx_b[keep_h]] + beta
          p_h <- (1 - epsilon) * stats::plogis(eta_h) + epsilon * 0.5
          p_h <- pmax(1e-10, pmin(1 - 1e-10, p_h))
          ll <- ll + sum(stats::dbinom(y_a[keep_h], size = 1L, prob = p_h, log = TRUE))
        }
      }
      if (nrow(within_spoke) > 0L) {
        idx_a <- spoke_lut[as.character(within_spoke$A_item)]
        idx_b <- spoke_lut[as.character(within_spoke$B_item)]
        y_a <- as.integer(within_spoke$y_A)
        keep_s <- !is.na(idx_a) & !is.na(idx_b) & y_a %in% c(0L, 1L)
        if (any(keep_s)) {
          eta_s <- alpha * (spoke_val[idx_a[keep_s]] - spoke_val[idx_b[keep_s]]) + beta
          p_s <- (1 - epsilon) * stats::plogis(eta_s) + epsilon * 0.5
          p_s <- pmax(1e-10, pmin(1 - 1e-10, p_s))
          ll <- ll + sum(stats::dbinom(y_a[keep_s], size = 1L, prob = p_s, log = TRUE))
        }
      }

      prior_pen <- 0.5 * (delta / 1)^2
      if (isTRUE(use_scale)) {
        prior_pen <- prior_pen + 0.5 * (log_alpha / 0.2)^2
      }
      if (n_h > 0L) {
        if (identical(lock_mode, "soft_lock")) {
          sd_soft <- hub_ref_sd[fit_hub_idx] / max(lock_kappa, 1e-8)
          prior_pen <- prior_pen + sum(
            0.5 * ((hub_val[fit_hub_idx] - hub_prior_center[fit_hub_idx]) / pmax(sd_soft, 1e-8))^2
          )
        } else {
          if (!identical(lock_mode, "hard_lock")) {
            rlang::abort(
              paste0(
                "Unsupported `hub_lock_mode` in linking joint refit: ",
                lock_mode,
                ". Expected `hard_lock` or `soft_lock`."
              )
            )
          }
          prior_pen <- prior_pen + sum(
            0.5 * ((hub_val[fit_hub_idx] - hub_ref[fit_hub_idx]) / pmax(hub_ref_sd[fit_hub_idx], 1e-8))^2
          )
        }
      }
      if (n_s > 0L) {
        prior_pen <- prior_pen + sum(
          0.5 * ((spoke_val[fit_spoke_idx] - spoke_ref[fit_spoke_idx]) / pmax(spoke_ref_sd[fit_spoke_idx], 1e-8))^2
        )
      }
      as.double(ll - prior_pen)
    }
  } else {
    idx_delta <- 1L
    idx_log_alpha <- if (isTRUE(use_scale)) 2L else NA_integer_
    start <- if (isTRUE(use_scale)) c(0, 0) else c(0)
    param_names <- c("delta", if (isTRUE(use_scale)) "log_alpha" else character())
    log_post <- function(par) {
      delta <- par[[idx_delta]]
      log_alpha <- if (isTRUE(use_scale)) par[[idx_log_alpha]] else 0
      alpha <- exp(log_alpha)
      eta <- delta + alpha * s - h + beta_signed
      p <- (1 - epsilon) * stats::plogis(eta) + epsilon * 0.5
      p <- pmax(1e-10, pmin(1 - 1e-10, p))
      ll <- sum(stats::dbinom(y, size = 1L, prob = p, log = TRUE))
      prior_pen <- 0.5 * (delta / 1)^2
      if (isTRUE(use_scale)) {
        prior_pen <- prior_pen + 0.5 * (log_alpha / 0.2)^2
      }
      as.double(ll - prior_pen)
    }
  }

  seed <- .adaptive_link_refit_seed(
    cross_edges = edges_obs,
    transform_mode = transform_mode,
    link_refit_mode = link_refit_mode
  )
  mcmc <- .adaptive_link_mcmc_sample(
    log_post_fn = log_post,
    init = start,
    seed = seed
  )
  draws <- as.matrix(mcmc$draws)
  colnames(draws) <- param_names
  diagnostics <- .adaptive_link_mcmc_diagnostics(mcmc$chain_draws, param_names = param_names)

  delta_draws <- as.double(draws[, idx_delta])
  delta_mean <- as.double(mean(delta_draws))
  delta_sd <- as.double(stats::sd(delta_draws))
  if (!is.finite(delta_sd)) {
    delta_sd <- 0
  }
  if (isTRUE(use_scale)) {
    log_alpha_draws <- as.double(draws[, idx_log_alpha])
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

  if (isTRUE(joint_used) && length(fit_hub_idx) > 0L) {
    hub_cols <- seq_along(fit_hub_idx)
    theta_hub_post[fit_hub_idx] <- colMeans(draws[, hub_cols, drop = FALSE])
  }
  if (isTRUE(joint_used) && length(fit_spoke_idx) > 0L) {
    spoke_cols <- length(fit_hub_idx) + seq_along(fit_spoke_idx)
    theta_spoke_post[fit_spoke_idx] <- colMeans(draws[, spoke_cols, drop = FALSE])
  }

  theta_hub_draws <- matrix(
    rep(theta_hub_post, each = nrow(draws)),
    nrow = nrow(draws),
    byrow = FALSE,
    dimnames = list(NULL, names(theta_hub_post))
  )
  theta_spoke_draws <- matrix(
    rep(theta_spoke_post, each = nrow(draws)),
    nrow = nrow(draws),
    byrow = FALSE,
    dimnames = list(NULL, names(theta_spoke_post))
  )
  if (isTRUE(joint_used) && length(fit_hub_idx) > 0L) {
    hub_cols <- seq_along(fit_hub_idx)
    theta_hub_draws[, fit_hub_idx] <- draws[, hub_cols, drop = FALSE]
  }
  if (isTRUE(joint_used) && length(fit_spoke_idx) > 0L) {
    spoke_cols <- length(fit_hub_idx) + seq_along(fit_spoke_idx)
    theta_spoke_draws[, fit_spoke_idx] <- draws[, spoke_cols, drop = FALSE]
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
    estimation_method = "bayesian_mcmc",
    uncertainty_approximation = "posterior_draws",
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
      chains = as.integer(dim(mcmc$chain_draws)[2L] %||% NA_integer_),
      warmup = 120L,
      samples = as.integer(dim(mcmc$chain_draws)[1L] %||% NA_integer_),
      mean_accept_rate = as.double(mcmc$accept_rate %||% NA_real_)
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
  panel_keys <- as.character(panel$pair_key[panel$realized %in% TRUE])
  if (length(panel_keys) < 1L) {
    return(tibble::tibble())
  }
  cross <- .adaptive_link_cross_edges(state, spoke_id = spoke_id, last_refit_step = NULL)
  if (nrow(cross) < 1L) {
    return(tibble::tibble())
  }
  cross <- tibble::as_tibble(cross)
  cross$pair_key <- make_unordered_key(cross$hub_item, cross$spoke_item)
  cross[cross$pair_key %in% panel_keys & cross$is_probe_step %in% TRUE, , drop = FALSE]
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
.adaptive_assert_link_stage_budget_invariants <- function(link_rows) {
  rows <- tibble::as_tibble(link_rows)
  if (nrow(rows) < 1L) {
    return(invisible(TRUE))
  }
  required <- c(
    "B_spoke_refit_budget",
    "stage_target_anchor_link",
    "stage_target_long_link",
    "stage_target_mid_link",
    "stage_target_local_link",
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
                                                seed = 1L) {
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
  concurrent_mode <- identical(as.character(controller$multi_spoke_mode %||% "independent"), "concurrent")
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
      list(
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
    list(
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
  link_identified_map <- controller$linking_identified_by_spoke %||% list()
  stop_counter_map <- controller$link_stop_consecutive_pass_count_by_spoke %||% list()
  escalation_counter_map <- controller$link_escalation_consecutive_pass_count_by_spoke %||% list()
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
    transform_frozen <- isTRUE(frozen_map[[key]])
    transform_policy <- .adaptive_normalize_link_transform_policy(
      controller$link_transform_policy %||% "auto"
    )
    transform_state <- .adaptive_link_transform_state_for_spoke(controller, spoke_id)
    refit_mode <- as.character(controller$link_refit_mode %||% "shift_only")
    lock_mode <- as.character(controller$hub_lock_mode %||% "soft_lock")
    kappa <- as.double(controller$hub_lock_kappa %||% 0.75)
    theta_treatment <- as.character(controller$shift_only_theta_treatment %||% "fixed_eap_plugin_var")
    theta_treatment_resolved <- theta_treatment

    hub_phase <- .adaptive_link_phase_a_theta_map(out, hub_id, "theta_raw_mean")
    hub_phase_sd <- .adaptive_link_phase_a_theta_map(out, hub_id, "theta_raw_sd")
    spoke_phase <- .adaptive_link_phase_a_theta_map(out, spoke_id, "theta_raw_mean")
    spoke_phase_sd <- .adaptive_link_phase_a_theta_map(out, spoke_id, "theta_raw_sd")
    hub_current <- .adaptive_link_theta_mean_map(out, hub_id)
    hub_current_sd <- .adaptive_link_theta_sd_map(out, hub_id)
    spoke_current <- .adaptive_link_theta_mean_map(out, spoke_id)
    spoke_current_sd <- .adaptive_link_theta_sd_map(out, spoke_id)

    if (identical(refit_mode, "joint_refit")) {
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
      shift_only_theta_treatment = theta_treatment
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
    fit <- if (isTRUE(transform_frozen)) {
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
    ppc_hub_theta <- fit$theta_hub_post %||% hub_theta
    ppc_spoke_theta <- fit$theta_spoke_post %||% spoke_theta
    cross_since_probe <- cross_since[
      as.character(cross_since$run_mode) == "link_probe" | cross_since$is_probe_step %in% TRUE,
      ,
      drop = FALSE
    ]
    cross_since_active <- cross_since[
      !(as.character(cross_since$run_mode) == "link_probe" | cross_since$is_probe_step %in% TRUE),
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

    panel_current <- .adaptive_link_probe_panel_for_spoke(out, spoke_id = spoke_id)
    probe_panel_id_current <- if (nrow(panel_current) > 0L) {
      as.character(panel_current$probe_panel_id[[1L]] %||% NA_character_)
    } else {
      NA_character_
    }
    hub_art <- out$linking$phase_a$artifacts[[as.character(hub_id)]] %||% list()
    spoke_art <- out$linking$phase_a$artifacts[[as.character(spoke_id)]] %||% list()
    epoch_signature <- paste(
      as.character(transform_state),
      as.character(refit_mode),
      as.character(lock_mode),
      as.integer(hub_art$refit_id %||% NA_integer_),
      as.integer(spoke_art$refit_id %||% NA_integer_),
      as.character(hub_art$fit_config_hash %||% NA_character_),
      as.character(spoke_art$fit_config_hash %||% NA_character_),
      as.character(probe_panel_id_current),
      sep = "|"
    )
    previous_signature <- as.character(epoch_signature_map[[key]] %||% NA_character_)
    link_epoch_id <- as.integer(epoch_id_map[[key]] %||% 1L)
    lag_domain_reset <- !is.na(previous_signature) && !identical(previous_signature, epoch_signature)
    if (isTRUE(lag_domain_reset)) {
      link_epoch_id <- as.integer(link_epoch_id + 1L)
      stop_counter_map[[key]] <- 0L
      escalation_counter_map[[key]] <- 0L
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
      identical(transform_state, "shift_scale") &&
      is.finite(log_alpha_change)) {
      log_alpha_change <= as.double(controller$log_alpha_change_max %||% 0.05)
    } else if (identical(transform_state, "shift_only")) {
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
    log_alpha_sd_pass <- if (identical(transform_state, "shift_scale")) {
      is.finite(fit$log_alpha_sd) && fit$log_alpha_sd <= as.double(controller$log_alpha_sd_max %||% 0.10)
    } else {
      TRUE
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
      log_alpha_change_pass <- NA
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
      var_mu_epsilon = as.double(controller$reliability_var_mu_epsilon %||% 1e-6),
      total_var_epsilon = as.double(controller$reliability_total_var_epsilon %||% 1e-6)
    )
    if (!is.finite(reliability_active)) {
      reliability_active <- as.double(reliability_stats$reliability %||% NA_real_)
    }
    theta_mean_transformed <- .adaptive_link_transform_theta_mean_for_spoke(
      state = out,
      theta_mean = .adaptive_btl_fit_theta_mean(out$btl_fit %||% list()),
      spoke_id = spoke_id,
      hub_id = hub_id,
      transform_mode = transform_state,
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
      transform_mode = transform_state,
      delta_mean = fit$delta_mean,
      log_alpha_mean = fit$log_alpha_mean,
      lag_row = lag_row
    )
    fit_diag <- fit$diagnostics %||% list()
    fit_contract <- fit$fit_contract %||% list()
    hub_anchored <- if (identical(refit_mode, "shift_only") || identical(lock_mode, "hard_lock")) {
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
    theta_global_rmse_lagged <- if (isTRUE(lag_eligible) && nrow(lag_row) > 0L) {
      .adaptive_link_theta_global_rmse_lagged(
        state = out,
        spoke_id = spoke_id,
        hub_id = hub_id,
        scope_ids = scope_ids,
        transform_mode = transform_state,
        delta_mean = fit$delta_mean,
        log_alpha_mean = fit$log_alpha_mean,
        lag_row = lag_row
      )
    } else {
      NA_real_
    }
    probe_edges_realized_tbl <- .adaptive_link_probe_edges_realized(
      state = out,
      spoke_id = spoke_id,
      epoch_id = link_epoch_id
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
    probe_pred_rmse_lagged <- if (isTRUE(lag_eligible) && nrow(lag_row) > 0L) {
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
    link_stop_pass_now <- isTRUE(link_stop_eligible) &&
      isTRUE(hub_anchored) &&
      is.finite(reliability_active) &&
      reliability_active >= as.double(controller$link_stop_reliability_min %||% 0.90) &&
      is.finite(probe_brier) &&
      probe_brier <= as.double(controller$probe_brier_max %||% 0.19) &&
      is.finite(probe_pred_rmse_lagged) &&
      probe_pred_rmse_lagged <= as.double(controller$probe_pred_rmse_max %||% 0.015) &&
      is.finite(theta_global_rmse_lagged) &&
      theta_global_rmse_lagged <= as.double(controller$theta_global_rmse_max %||% 0.04)
    stop_counter <- if (isTRUE(link_stop_pass_now)) {
      as.integer(stop_counter_map[[key]] %||% 0L) + 1L
    } else {
      0L
    }
    stop_counter_map[[key]] <- as.integer(stop_counter)

    cross_active_epoch <- cross_active_all[0, , drop = FALSE]
    scale_ready <- FALSE
    if (nrow(cross_all) > 0L) {
      spoke_item <- hub_item <- NULL
      cross_active_epoch <- cross_active_all[
        as.integer(cross_active_all$step_id) >= as.integer(epoch_start_step),
        ,
        drop = FALSE
      ]
      bins_used <- as.integer(coverage_bins_map[[key]] %||% 3L)
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
    escalation_counter <- as.integer(escalation_counter_map[[key]] %||% 0L)
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
      if (isTRUE(alt_fit$converged) &&
        is.finite(probe_brier_delta) &&
        probe_brier_delta >= as.double(controller$probe_brier_delta_min %||% 0.005) &&
        is.finite(alt_fit$log_alpha_sd) &&
        alt_fit$log_alpha_sd <= as.double(controller$logalpha_sd_guardrail %||% 0.10)) {
        escalation_counter <- escalation_counter + 1L
      } else {
        escalation_counter <- 0L
      }
      if (escalation_counter >= as.integer(controller$link_transform_escalation_refits_required %||% 2L)) {
        escalated_this_refit <- TRUE
        escalation_counter <- 0L
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
        epoch_signature <- paste(
          "shift_scale",
          as.character(refit_mode),
          as.character(lock_mode),
          as.integer(hub_art$refit_id %||% NA_integer_),
          as.integer(spoke_art$refit_id %||% NA_integer_),
          as.character(hub_art$fit_config_hash %||% NA_character_),
          as.character(spoke_art$fit_config_hash %||% NA_character_),
          as.character(probe_panel_id_current),
          sep = "|"
        )
        epoch_signature_map[[key]] <- as.character(epoch_signature)
        lag_domain_key <- as.character(epoch_signature)
        lag_domain_key_map[[key]] <- as.character(lag_domain_key)
        lag_domain_reset <- TRUE
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
        stop_counter <- 0L
        stop_counter_map[[key]] <- 0L
      }
    } else {
      escalation_counter <- 0L
    }
    escalation_counter_map[[key]] <- as.integer(escalation_counter)

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
      transform_frozen = as.logical(transform_frozen),
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
      reliability_link_global = as.double(reliability_active),
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
        is.finite(ts_btl_rank_active) &&
          ts_btl_rank_active >= as.double(controller$link_rank_corr_min %||% 0.90)
      ),
      lag_domain_key = as.character(lag_domain_key),
      lag_domain_reset = as.logical(lag_domain_reset),
      link_epoch_id = as.integer(link_epoch_id),
      lag_eligible = as.logical(lag_eligible),
      link_lag_eligible = as.logical(link_lag_eligible),
      link_min_refit_eligible = as.logical(link_min_refit_eligible),
      link_stop_gate_open = as.logical(link_stop_gate_open),
      rank_stability_lagged = as.double(rank_stability$rho_rank_lagged %||% NA_real_),
      rank_stability_pass = as.logical(rank_stability$rho_rank_lagged_pass %||% FALSE),
      link_identified = as.logical(link_identified),
      link_stop_eligible = as.logical(link_stop_eligible),
      stop_consecutive_pass_count = as.integer(stop_counter),
      link_stop_pass = as.logical(
        stop_counter >= as.integer(controller$stability_consecutive_k %||% 2L)
      ),
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
      probe_brier = as.double(probe_brier),
      probe_pred_rmse_lagged = as.double(probe_pred_rmse_lagged),
      theta_global_rmse_scope = as.character(controller$theta_global_rmse_scope %||% "direct_evidence_spoke"),
      theta_global_rmse_lagged = as.double(theta_global_rmse_lagged),
      escalated_this_refit = as.logical(escalated_this_refit),
      escalation_consecutive_pass_count = as.integer(escalation_counter),
      probe_brier_shift_only = as.double(probe_brier_shift_only),
      probe_brier_shift_scale = as.double(probe_brier_shift_scale),
      probe_brier_delta = as.double(probe_brier_delta),
      log_alpha_spoke_sd_alt = as.double(alt_fit$log_alpha_sd %||% NA_real_),
      alt_eval_active_edges = as.integer(nrow(cross_active_epoch)),
      alt_eval_converged = as.logical(alt_fit$converged %||% FALSE),
      alternative_fit_method = as.character(alt_fit$fit_method %||% "map_laplace_hessian"),
      alternative_uncertainty_approximation = as.character(
        alt_fit$uncertainty_approximation %||% "laplace_hessian"
      ),
      probe_brier_delta_min_used = as.double(controller$probe_brier_delta_min %||% 0.005),
      logalpha_sd_guardrail_used = as.double(controller$logalpha_sd_guardrail %||% 0.10),
      probe_edges_min_for_stop_used = as.integer(controller$probe_edges_min_for_stop %||% 30L),
      link_transform_escalation_refits_required_used = as.integer(
        controller$link_transform_escalation_refits_required %||% 2L
      ),
      n_probe_pairs_since_last_refit = as.integer(nrow(cross_since_probe)),
      n_cross_edges_active_since_last_refit = as.integer(nrow(cross_since_active)),
      n_cross_edges_probe_since_last_refit = as.integer(nrow(cross_since_probe)),
      n_cross_edges_total_since_last_refit = as.integer(nrow(cross_since)),
      coverage_bins_used = as.integer(coverage_bins_map[[key]] %||% NA_integer_),
      coverage_source = as.character(coverage_source_map[[key]] %||% NA_character_),
      active_item_count_hub = as.integer(length(active$active_hub)),
      active_item_count_spoke = as.integer(length(scope_ids)),
      active_item_count_total = as.integer(length(active$active_all)),
      var_mean_theta_global_active = as.double(reliability_stats$V_mu %||% NA_real_),
      mean_var_theta_global_active = as.double(reliability_stats$V_post %||% NA_real_),
      reliability_var_mu_epsilon_used = as.double(controller$reliability_var_mu_epsilon %||% 1e-6),
      reliability_total_var_epsilon_used = as.double(controller$reliability_total_var_epsilon %||% 1e-6),
      uncertainty = as.double(fit$delta_sd + if (is.finite(fit$log_alpha_sd)) fit$log_alpha_sd else 0)
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
  controller$link_transform_frozen_by_spoke <- frozen_map
  controller$link_transform_frozen_delta_by_spoke <- frozen_delta_map
  controller$link_transform_frozen_log_alpha_by_spoke <- frozen_log_alpha_map
  controller$linking_identified_by_spoke <- link_identified_map
  controller$link_stop_consecutive_pass_count_by_spoke <- stop_counter_map
  controller$link_escalation_consecutive_pass_count_by_spoke <- escalation_counter_map
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
  budget_map <- .adaptive_link_budget_map_for_refit(
    state = state,
    controller = controller,
    eligible_spoke_ids = spoke_ids
  )

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
    since_last_probe <- since_last[
      as.character(since_last$run_mode) == "link_probe" | since_last$is_probe_step %in% TRUE,
      ,
      drop = FALSE
    ]
    since_last_active <- since_last[
      !(as.character(since_last$run_mode) == "link_probe" | since_last$is_probe_step %in% TRUE),
      ,
      drop = FALSE
    ]
    n_pairs_since_probe <- as.integer(nrow(since_last_probe))
    n_pairs_since_active <- as.integer(nrow(since_last_active))
    n_pairs_since_total <- as.integer(nrow(since_last))
    budget_info <- budget_map[[key]] %||% list(
      B_spoke_refit_budget = .adaptive_link_refit_budget_default(as.integer(state$n_items), controller),
      B_spoke_refit_budget_source = "single_spoke_default"
    )
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
    stage_quotas[!is.finite(stage_quotas)] <- 0L
    committed_stage[!is.finite(committed_stage)] <- 0L
    realized_active_budget_floor <- as.integer(sum(committed_stage, na.rm = TRUE))
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

    transform_policy <- as.character(stats_row$link_transform_policy %||%
      controller$link_transform_policy %||% "auto")
    transform_state <- as.character(stats_row$link_transform_state %||%
      .adaptive_link_transform_state_for_spoke(controller, spoke_id))
    d_opt_key <- .adaptive_link_d_opt_state_key(refit_id = as.integer(refit_id), spoke_id = as.integer(spoke_id))
    d_opt_entry <- d_opt_map[[d_opt_key]] %||%
      .adaptive_link_d_opt_state_get(
        controller = controller,
        refit_id = as.integer(refit_id),
        spoke_id = as.integer(spoke_id),
        transform_mode = transform_state
      )
    d_opt_dim <- .adaptive_link_d_opt_matrix_dim(transform_state)
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
    transform_frozen <- isTRUE(stats_row$transform_frozen %||% FALSE) || isTRUE(link_stop_pass)
    probe_panel <- .adaptive_link_probe_panel_for_spoke(
      state,
      spoke_id = as.integer(spoke_id),
      epoch_id = .adaptive_link_probe_epoch_for_spoke(state, spoke_id = spoke_id)
    )
    probe_panel_id <- if (nrow(probe_panel) > 0L) {
      as.character(probe_panel$probe_panel_id[[1L]] %||% NA_character_)
    } else {
      NA_character_
    }
    probe_edges_planned <- as.integer(nrow(probe_panel))
    probe_edges_realized <- as.integer(sum(probe_panel$realized %in% TRUE, na.rm = TRUE))
    probe_panel_shortfall <- as.integer(max(0L, probe_edges_planned - probe_edges_realized))
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
      delta_change_pass = as.logical(stats_row$delta_change_pass %||% NA),
      log_alpha_change_pass = as.logical(stats_row$log_alpha_change_pass %||% NA),
      delta_sd_max_used = as.double(stats_row$delta_sd_max_used %||% NA_real_),
      delta_sd_pass = as.logical(stats_row$delta_sd_pass %||% NA),
      log_alpha_sd_pass = as.logical(stats_row$log_alpha_sd_pass %||% NA),
      reliability_EAP_link = as.double(stats_row$link_reliability %||% NA_real_),
      reliability_link_global = as.double(
        stats_row$reliability_link_global %||% stats_row$link_reliability %||% NA_real_
      ),
      reliability_stop_pass = as.logical(stats_row$link_reliability_stop_pass %||% NA),
      linking_identified = as.logical(linking_identified),
      lag_eligible = as.logical(stats_row$lag_eligible %||% FALSE),
      link_lag_eligible = as.logical(stats_row$link_lag_eligible %||% stats_row$lag_eligible %||% FALSE),
      link_min_refit_eligible = as.logical(stats_row$link_min_refit_eligible %||% FALSE),
      link_stop_gate_open = as.logical(stats_row$link_stop_gate_open %||% FALSE),
      rank_stability_lagged = as.double(stats_row$rank_stability_lagged %||% NA_real_),
      rank_stability_pass = as.logical(stats_row$rank_stability_pass %||% FALSE),
      link_stop_eligible = as.logical(link_stop_eligible),
      stop_consecutive_pass_count = as.integer(stats_row$stop_consecutive_pass_count %||% 0L),
      link_stop_pass = as.logical(link_stop_pass),
      transform_frozen = as.logical(transform_frozen),
      transform_frozen_refit_id = as.integer(controller$link_transform_frozen_refit_id_by_spoke[[key]] %||%
        if (isTRUE(transform_frozen)) refit_id else NA_integer_),
      link_epoch_id = as.integer(.adaptive_link_probe_epoch_for_spoke(state, spoke_id = spoke_id)),
      ts_btl_rank_spearman = as.double(stats_row$ts_btl_rank_spearman_active %||% NA_real_),
      ppc_brier_cross_active = as.double(stats_row$ppc_brier_cross_active %||% NA_real_),
      ppc_brier_cross_probe = as.double(stats_row$ppc_brier_cross_probe %||% NA_real_),
      ppc_brier_cross = as.double(stats_row$ppc_brier_cross %||% NA_real_),
      hub_anchored = as.logical(stats_row$hub_anchored %||% NA),
      scale_ready = as.logical(stats_row$scale_ready %||% NA),
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
      escalation_consecutive_pass_count = as.integer(stats_row$escalation_consecutive_pass_count %||% 0L),
      escalated_this_refit = as.logical(stats_row$escalated_this_refit %||% FALSE),
      probe_brier_shift_only = as.double(stats_row$probe_brier_shift_only %||% NA_real_),
      probe_brier_shift_scale = as.double(stats_row$probe_brier_shift_scale %||% NA_real_),
      probe_brier_delta = as.double(stats_row$probe_brier_delta %||% NA_real_),
      log_alpha_spoke_sd_alt = as.double(stats_row$log_alpha_spoke_sd_alt %||% NA_real_),
      n_pairs_cross_set_done = as.integer(n_pairs_done),
      n_unique_cross_pairs_seen = as.integer(n_unique),
      n_probe_pairs_since_last_refit = as.integer(stats_row$n_probe_pairs_since_last_refit %||% n_pairs_since_probe),
      n_cross_edges_active_since_last_refit = as.integer(
        stats_row$n_cross_edges_active_since_last_refit %||% n_pairs_since_active
      ),
      n_cross_edges_probe_since_last_refit = as.integer(
        stats_row$n_cross_edges_probe_since_last_refit %||% n_pairs_since_probe
      ),
      n_cross_edges_total_since_last_refit = as.integer(
        stats_row$n_cross_edges_total_since_last_refit %||% n_pairs_since_total
      ),
      B_spoke_refit_budget = as.integer(sum(stage_quotas)),
      B_spoke_refit_budget_source = as.character(
        budget_info$B_spoke_refit_budget_source %||% "single_spoke_default"
      ),
      stage_target_anchor_link = as.integer(stage_quotas[["anchor_link"]] %||% NA_integer_),
      stage_target_long_link = as.integer(stage_quotas[["long_link"]] %||% NA_integer_),
      stage_target_mid_link = as.integer(stage_quotas[["mid_link"]] %||% NA_integer_),
      stage_target_local_link = as.integer(stage_quotas[["local_link"]] %||% NA_integer_),
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
      probe_edges_realized = as.integer(probe_edges_realized),
      probe_panel_shortfall = as.integer(probe_panel_shortfall),
      probe_panel_reallocation_used = as.logical(probe_panel_reallocation_used),
      probe_pred_cache_used = as.logical(probe_pred_cache_used),
      probe_brier = as.double(stats_row$probe_brier %||% NA_real_),
      probe_pred_rmse_lagged = as.double(stats_row$probe_pred_rmse_lagged %||% NA_real_),
      theta_global_rmse_scope = as.character(
        stats_row$theta_global_rmse_scope %||% controller$theta_global_rmse_scope %||% "direct_evidence_spoke"
      ),
      theta_global_rmse_lagged = as.double(stats_row$theta_global_rmse_lagged %||% NA_real_),
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
      link_transform_escalation_refits_required_used = as.integer(
        stats_row$link_transform_escalation_refits_required_used %||%
          controller$link_transform_escalation_refits_required %||%
          2L
      ),
      probe_edges_count_toward_active_constraints_used = as.logical(
        controller$probe_edges_count_toward_active_constraints %||% FALSE
      ),
      lag_domain_key = as.character(stats_row$lag_domain_key %||% NA_character_),
      lag_domain_reset = as.logical(stats_row$lag_domain_reset %||% NA)
    )
  }

  rows_tbl <- dplyr::bind_rows(rows)
  .adaptive_assert_link_stage_budget_invariants(rows_tbl)
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
    "refit_id", "spoke_id", "hub_id", "link_transform_policy", "link_transform_state", "link_refit_mode",
    "hub_lock_mode", "reliability_EAP_link", "linking_identified", "link_stop_eligible", "link_stop_pass",
    "transform_frozen",
    "n_pairs_cross_set_done", "n_unique_cross_pairs_seen", "n_cross_edges_active_since_last_refit",
    "n_cross_edges_probe_since_last_refit", "n_cross_edges_total_since_last_refit", "coverage_bins_used",
    "B_spoke_refit_budget", "B_spoke_refit_budget_source",
    "stage_target_anchor_link", "stage_target_long_link", "stage_target_mid_link", "stage_target_local_link",
    "stage_realized_anchor_link", "stage_realized_long_link", "stage_realized_mid_link", "stage_realized_local_link",
    "stage_shortfall_anchor_link", "stage_shortfall_long_link", "stage_shortfall_mid_link",
    "stage_shortfall_local_link", "stage_reallocation_used", "stage_reallocation_rule_used",
    "stage_budget_unfilled"
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
    is.na(rows$link_transform_policy) | is.na(rows$link_transform_state) |
      is.na(rows$link_refit_mode) | is.na(rows$hub_lock_mode),
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
  if (any(is.na(rows$transform_frozen))) {
    rlang::abort("link_stage_log append completeness failure: `transform_frozen` must be populated.")
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
  controller <- .adaptive_controller_resolve(state)
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
    if (.adaptive_link_mode_active(controller) &&
      identical(as.character(phase_ctx$phase %||% "phase_a"), "phase_b") &&
      isTRUE(latest_starved) &&
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

  history <- state$refit_meta$theta_mean_history %||% list()
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
