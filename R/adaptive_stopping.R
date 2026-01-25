# -------------------------------------------------------------------------
# Stopping checks for adaptive refinement (diagnostics + reliability +
# lagged theta/rank stability with refit-only stop passes).
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
near_stop_from_state <- function(state) {
  validate_state(state)
  isTRUE(state$stop_candidate) || state$checks_passed_in_row > 0L
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
  min_refits_for_stability <- as.integer(config$min_refits_for_stability)
  stability_lag <- as.integer(config$stability_lag)
  if (is.na(min_refits_for_stability) || min_refits_for_stability < 1L) {
    rlang::abort("`config$min_refits_for_stability` must be a positive integer.")
  }
  if (is.na(stability_lag) || stability_lag < 1L) {
    rlang::abort("`config$stability_lag` must be a positive integer.")
  }

  rho_theta_lag <- NA_real_
  delta_sd_theta_lag <- NA_real_
  rho_rank_lag <- NA_real_
  rank_stability_pass <- NA
  eligible <- current_refit >= min_refits_for_stability && length(history) >= stability_lag
  if (isTRUE(eligible)) {
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

    rho_theta_lag <- stats::cor(theta_mean, lag_theta, use = "complete.obs")
    sd_current <- stats::sd(theta_mean)
    sd_lag <- stats::sd(lag_theta)
    if (is.finite(sd_lag) && sd_lag > 0) {
      delta_sd_theta_lag <- abs(sd_current - sd_lag) / sd_lag
    }
    rank_current <- rank(-theta_mean, ties.method = "average")
    rank_lag <- rank(-lag_theta, ties.method = "average")
    rho_rank_lag <- stats::cor(rank_current, rank_lag, use = "complete.obs")
    rank_spearman_min <- as.double(config$rank_spearman_min)
    if (!is.finite(rank_spearman_min) || length(rank_spearman_min) != 1L) {
      rlang::abort("`config$rank_spearman_min` must be a finite numeric scalar.")
    }
    rank_stability_pass <- is.finite(rho_rank_lag) && rho_rank_lag >= rank_spearman_min
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
  metrics$theta_sd_eap <- as.double(theta_sd_eap)
  metrics$rho_theta_lag <- as.double(rho_theta_lag)
  metrics$delta_sd_theta_lag <- as.double(delta_sd_theta_lag)
  metrics$rho_rank_lag <- as.double(rho_rank_lag)
  metrics$rank_stability_pass <- rank_stability_pass
  metrics$candidate_starved <- as.logical(state$posterior$candidate_starved %||% NA)

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
    state$checks_passed_in_row <- 0L
    state$stop_candidate <- FALSE
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
  min_refits_for_stability <- as.integer(config$min_refits_for_stability)
  if (is.na(min_refits_for_stability) || min_refits_for_stability < 1L) {
    rlang::abort("`config$min_refits_for_stability` must be a positive integer.")
  }
  stop_eligible <- current_refit >= min_refits_for_stability

  if (!isTRUE(diagnostics_pass)) {
    state$checks_passed_in_row <- 0L
    state$stop_candidate <- FALSE
  } else {
    eap_min <- as.double(config$eap_reliability_min)
    if (!is.finite(eap_min) || length(eap_min) != 1L) {
      rlang::abort("`config$eap_reliability_min` must be a finite numeric scalar.")
    }
    eap_pass <- is.finite(metrics$reliability_EAP) && metrics$reliability_EAP >= eap_min

    theta_corr_min <- as.double(config$theta_corr_min)
    theta_sd_rel_change_max <- as.double(config$theta_sd_rel_change_max)
    if (!is.finite(theta_corr_min) || length(theta_corr_min) != 1L) {
      rlang::abort("`config$theta_corr_min` must be a finite numeric scalar.")
    }
    if (!is.finite(theta_sd_rel_change_max) || length(theta_sd_rel_change_max) != 1L) {
      rlang::abort("`config$theta_sd_rel_change_max` must be a finite numeric scalar.")
    }
    theta_stability_pass <- stop_eligible &&
      is.finite(metrics$rho_theta_lag) &&
      metrics$rho_theta_lag >= theta_corr_min &&
      is.finite(metrics$delta_sd_theta_lag) &&
      metrics$delta_sd_theta_lag <= theta_sd_rel_change_max

    rank_stability_pass <- stop_eligible && isTRUE(metrics$rank_stability_pass)

    if (stop_eligible && diagnostics_pass && eap_pass && theta_stability_pass && rank_stability_pass) {
      state$checks_passed_in_row <- as.integer(state$checks_passed_in_row + 1L)
    } else {
      state$checks_passed_in_row <- 0L
    }
    state$stop_candidate <- isTRUE(stop_eligible)
  }

  state <- .adaptive_update_theta_history(state, state$fit)

  stop_decision <- isTRUE(state$checks_passed_in_row >= config$stability_consecutive)
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
