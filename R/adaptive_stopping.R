# -------------------------------------------------------------------------
# Stopping checks for adaptive refinement (diagnostics + reliability +
# lagged theta/rank stability with refit-only stop passes).
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
near_stop_from_state <- function(state) {
  validate_state(state)
  identical(state$phase, "phase3")
}

#' @keywords internal
#' @noRd
compute_stop_metrics <- function(state, fit, candidates_with_utility, config) {
  validate_state(state)
  if (!is.list(fit) || is.null(fit$theta_draws)) {
    rlang::abort("`fit` must include `theta_draws`.")
  }
  validate_v3_fit_contract(fit, ids = state$ids)
  theta_draws <- fit$theta_draws
  if (!is.matrix(theta_draws) || !is.numeric(theta_draws)) {
    rlang::abort("`fit$theta_draws` must be a numeric matrix.")
  }
  if (nrow(theta_draws) < 2L) {
    rlang::abort("`fit$theta_draws` must have at least two draws.")
  }

  theta_summary <- .adaptive_theta_summary_from_fit(fit, state)
  theta_mean <- stats::setNames(theta_summary$theta_mean, theta_summary$item_id)
  theta_mean <- as.double(theta_mean[state$ids])
  if (any(!is.finite(theta_mean))) {
    rlang::abort("`fit$theta_mean` must be finite.")
  }
  theta_sd_eap <- stats::sd(theta_mean)
  reliability_EAP <- compute_reliability_EAP(theta_draws)

  utilities_tbl <- candidates_with_utility %||% tibble::tibble()
  if (!is.data.frame(utilities_tbl)) {
    rlang::abort("`candidates_with_utility` must be a data frame or tibble.")
  }
  utilities_tbl <- tibble::as_tibble(utilities_tbl)

  diagnostics_pass <- state$posterior$diagnostics_pass %||% NA
  if (!is.na(diagnostics_pass)) {
    diagnostics_pass <- as.logical(diagnostics_pass)
    if (length(diagnostics_pass) != 1L || is.na(diagnostics_pass)) {
      rlang::abort("`state$posterior$diagnostics_pass` must be TRUE, FALSE, or NA.")
    }
  }

  hard_cap_frac <- config$hard_cap_frac
  if (!is.numeric(hard_cap_frac) ||
    length(hard_cap_frac) != 1L ||
    !is.finite(hard_cap_frac) ||
    hard_cap_frac <= 0 ||
    hard_cap_frac > 1) {
    rlang::abort("`config$hard_cap_frac` must be a finite numeric scalar in (0, 1].")
  }
  total_pairs <- state$N * (state$N - 1L) / 2
  hard_cap_threshold <- ceiling(hard_cap_frac * total_pairs)
  n_unique_pairs_seen <- sum(state$pair_count >= 1L)
  hard_cap_reached <- n_unique_pairs_seen >= hard_cap_threshold

  history <- state$posterior$theta_mean_history %||% list()
  if (!is.list(history)) {
    rlang::abort("`state$posterior$theta_mean_history` must be a list when set.")
  }
  current_refit <- length(history) + 1L
  stability_lag <- as.integer(config$stability_lag)
  if (is.na(stability_lag) || stability_lag < 1L) {
    rlang::abort("`config$stability_lag` must be a positive integer.")
  }

  lag_eligible <- current_refit > stability_lag

  rho_theta_lag <- NA_real_
  delta_sd_theta_lag <- NA_real_
  rho_rank_lag <- NA_real_
  theta_corr_pass <- NA
  delta_sd_theta_pass <- NA
  rho_rank_pass <- NA
  rank_stability_pass <- NA
  if (isTRUE(lag_eligible)) {
    lag_idx <- length(history) - stability_lag + 1L
    lag_theta <- history[[lag_idx]]
    if (!is.numeric(lag_theta) || length(lag_theta) != length(state$ids)) {
      rlang::abort("Lagged theta history must be a numeric vector over `state$ids`.")
    }
    if (is.null(names(lag_theta))) {
      if (length(lag_theta) == length(state$ids)) {
        names(lag_theta) <- state$ids
      } else {
        rlang::abort("Lagged theta history must be named over `state$ids`.")
      }
    }
    if (!setequal(names(lag_theta), state$ids)) {
      rlang::abort("Lagged theta history must be named over `state$ids`.")
    }
    lag_theta <- as.double(lag_theta[state$ids])
    if (any(!is.finite(lag_theta))) {
      rlang::abort("Lagged theta history must be finite.")
    }

    sd_current <- stats::sd(theta_mean)
    sd_lag <- stats::sd(lag_theta)
    if (is.finite(sd_current) && is.finite(sd_lag) && sd_current > 0 && sd_lag > 0) {
      rho_theta_lag <- stats::cor(theta_mean, lag_theta,
        method = "pearson",
        use = "complete.obs"
      )
    }
    if (is.finite(sd_lag) && sd_lag > 0) {
      delta_sd_theta_lag <- abs(sd_current - sd_lag) / sd_lag
    }
    if (is.finite(sd_current) && is.finite(sd_lag) && sd_current > 0 && sd_lag > 0) {
      rho_rank_lag <- stats::cor(theta_mean, lag_theta,
        method = "spearman",
        use = "complete.obs"
      )
    }
    theta_corr_min <- as.double(config$theta_corr_min)
    theta_sd_rel_change_max <- as.double(config$theta_sd_rel_change_max)
    rank_spearman_min <- as.double(config$rank_spearman_min)
    if (!is.finite(theta_corr_min) || length(theta_corr_min) != 1L) {
      rlang::abort("`config$theta_corr_min` must be a finite numeric scalar.")
    }
    if (!is.finite(theta_sd_rel_change_max) || length(theta_sd_rel_change_max) != 1L) {
      rlang::abort("`config$theta_sd_rel_change_max` must be a finite numeric scalar.")
    }
    if (!is.finite(rank_spearman_min) || length(rank_spearman_min) != 1L) {
      rlang::abort("`config$rank_spearman_min` must be a finite numeric scalar.")
    }
    theta_corr_pass <- is.finite(rho_theta_lag) && rho_theta_lag >= theta_corr_min
    delta_sd_theta_pass <- is.finite(delta_sd_theta_lag) && delta_sd_theta_lag <= theta_sd_rel_change_max
    rho_rank_pass <- is.finite(rho_rank_lag) && rho_rank_lag >= rank_spearman_min
    rank_stability_pass <- rho_rank_pass
  }

  metrics <- .adaptive_stop_metrics_defaults()
  diagnostics <- fit$diagnostics %||% list()

  metrics$hard_cap_reached <- hard_cap_reached
  metrics$hard_cap_threshold <- hard_cap_threshold
  metrics$n_unique_pairs_seen <- n_unique_pairs_seen
  metrics$scheduled_pairs <- as.integer(state$comparisons_scheduled)
  metrics$proposed_pairs <- as.integer(nrow(utilities_tbl))
  metrics$completed_pairs <- as.integer(state$comparisons_observed)
  metrics$diagnostics_pass <- diagnostics_pass
  metrics$divergences <- as.integer(diagnostics$divergences %||% NA_integer_)
  metrics$min_ess_bulk <- as.double(diagnostics$min_ess_bulk %||% NA_real_)
  metrics$max_rhat <- as.double(diagnostics$max_rhat %||% NA_real_)
  metrics$reliability_EAP <- as.double(reliability_EAP)
  metrics$eap_pass <- NA
  metrics$theta_sd_eap <- as.double(theta_sd_eap)
  metrics$rho_theta_lag <- as.double(rho_theta_lag)
  metrics$theta_corr_pass <- theta_corr_pass
  metrics$delta_sd_theta_lag <- as.double(delta_sd_theta_lag)
  metrics$delta_sd_theta_pass <- delta_sd_theta_pass
  metrics$rho_rank_lag <- as.double(rho_rank_lag)
  metrics$rho_rank_pass <- rho_rank_pass
  metrics$rank_stability_pass <- rank_stability_pass
  metrics$candidate_starved <- as.logical(state$posterior$candidate_starved %||% NA)
  metrics$stop_eligible <- isTRUE(lag_eligible)

  if (isTRUE(diagnostics_pass)) {
    eap_min <- as.double(config$eap_reliability_min)
    if (!is.finite(eap_min) || length(eap_min) != 1L) {
      rlang::abort("`config$eap_reliability_min` must be a finite numeric scalar.")
    }
    metrics$eap_pass <- is.finite(metrics$reliability_EAP) && metrics$reliability_EAP >= eap_min
  }

  metrics
}

#' @keywords internal
#' @noRd
.adaptive_update_theta_history <- function(state, fit) {
  validate_state(state)
  if (!is.list(fit) || is.null(fit$theta_mean) || is.null(fit$theta_sd)) {
    rlang::abort("`fit` must include `theta_mean` and `theta_sd`.")
  }
  theta_summary <- .adaptive_theta_summary_from_fit(fit, state)
  theta_mean <- stats::setNames(theta_summary$theta_mean, theta_summary$item_id)
  theta_mean <- as.double(theta_mean[state$ids])
  history <- state$posterior$theta_mean_history %||% list()
  if (!is.list(history)) {
    rlang::abort("`state$posterior$theta_mean_history` must be a list when set.")
  }
  state$posterior$theta_mean_history <- c(history, list(theta_mean))
  state
}

#' @keywords internal
#' @noRd
should_stop <- function(metrics, state, config) {
  validate_state(state)
  if (!is.list(metrics)) {
    rlang::abort("`metrics` must be a list.")
  }
  if (is.null(config) || !is.list(config)) {
    rlang::abort("`config` must be a list.")
  }

  eap_min <- as.double(config$eap_reliability_min)
  theta_corr_min <- as.double(config$theta_corr_min)
  theta_sd_rel_change_max <- as.double(config$theta_sd_rel_change_max)
  rank_spearman_min <- as.double(config$rank_spearman_min)
  if (!is.finite(eap_min) || length(eap_min) != 1L) {
    rlang::abort("`config$eap_reliability_min` must be a finite numeric scalar.")
  }
  if (!is.finite(theta_corr_min) || length(theta_corr_min) != 1L) {
    rlang::abort("`config$theta_corr_min` must be a finite numeric scalar.")
  }
  if (!is.finite(theta_sd_rel_change_max) || length(theta_sd_rel_change_max) != 1L) {
    rlang::abort("`config$theta_sd_rel_change_max` must be a finite numeric scalar.")
  }
  if (!is.finite(rank_spearman_min) || length(rank_spearman_min) != 1L) {
    rlang::abort("`config$rank_spearman_min` must be a finite numeric scalar.")
  }

  if (isTRUE(metrics$hard_cap_reached)) {
    state$mode <- "stopped"
    state$stop_reason <- "hard_cap_40pct"
    return(list(
      state = state,
      stop_decision = TRUE,
      stop_reason = state$stop_reason
    ))
  }

  if (identical(state$mode, "stopped")) {
    return(list(
      state = state,
      stop_decision = TRUE,
      stop_reason = state$stop_reason %||% NA_character_
    ))
  }

  min_comparisons <- state$M1_target %||% NA_integer_
  if (!is.integer(min_comparisons) || length(min_comparisons) != 1L) {
    rlang::abort("`state$M1_target` must be a length-1 integer.")
  }
  if (!is.na(min_comparisons) && state$comparisons_observed < min_comparisons) {
    return(list(
      state = state,
      stop_decision = FALSE,
      stop_reason = state$stop_reason %||% NA_character_
    ))
  }

  refit_performed <- isTRUE(metrics$refit_performed)
  if (!isTRUE(refit_performed)) {
    return(list(
      state = state,
      stop_decision = FALSE,
      stop_reason = state$stop_reason %||% NA_character_
    ))
  }

  diagnostics_pass <- isTRUE(metrics$diagnostics_pass)
  history <- state$posterior$theta_mean_history %||% list()
  if (!is.list(history)) {
    rlang::abort("`state$posterior$theta_mean_history` must be a list when set.")
  }
  current_refit <- length(history) + 1L
  stability_lag <- as.integer(config$stability_lag)
  if (is.na(stability_lag) || stability_lag < 1L) {
    rlang::abort("`config$stability_lag` must be a positive integer.")
  }
  lag_eligible <- current_refit > stability_lag
  eap_pass <- metrics$eap_pass
  if (!isTRUE(eap_pass) && !isFALSE(eap_pass)) {
    eap_pass <- is.finite(metrics$reliability_EAP) && metrics$reliability_EAP >= eap_min
  }
  if (isTRUE(lag_eligible)) {
    theta_corr_pass <- metrics$theta_corr_pass
    if (!isTRUE(theta_corr_pass) && !isFALSE(theta_corr_pass)) {
      theta_corr_pass <- is.finite(metrics$rho_theta_lag) && metrics$rho_theta_lag >= theta_corr_min
    }
    delta_sd_theta_pass <- metrics$delta_sd_theta_pass
    if (!isTRUE(delta_sd_theta_pass) && !isFALSE(delta_sd_theta_pass)) {
      delta_sd_theta_pass <- is.finite(metrics$delta_sd_theta_lag) &&
        metrics$delta_sd_theta_lag <= theta_sd_rel_change_max
    }
    rho_rank_pass <- metrics$rho_rank_pass
    if (!isTRUE(rho_rank_pass) && !isFALSE(rho_rank_pass)) {
      if (isTRUE(metrics$rank_stability_pass) || isFALSE(metrics$rank_stability_pass)) {
        rho_rank_pass <- isTRUE(metrics$rank_stability_pass)
      } else {
        rho_rank_pass <- is.finite(metrics$rho_rank_lag) && metrics$rho_rank_lag >= rank_spearman_min
      }
    }
  } else {
    theta_corr_pass <- TRUE
    delta_sd_theta_pass <- TRUE
    rho_rank_pass <- TRUE
  }

  state <- .adaptive_update_theta_history(state, state$fit)

  stop_decision <- isTRUE(diagnostics_pass) &&
    eap_pass &&
    theta_corr_pass &&
    delta_sd_theta_pass &&
    rho_rank_pass
  if (stop_decision) {
    state$mode <- "stopped"
    state$stop_reason <- "v3_converged"
  }

  list(
    state = state,
    stop_decision = stop_decision,
    stop_reason = state$stop_reason %||% NA_character_
  )
}
