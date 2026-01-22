# -------------------------------------------------------------------------
# Stopping checks for adaptive refinement.
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
  if (!is.null(fit$epsilon_mean) && !is.null(fit$diagnostics)) {
    validate_v3_fit_contract(fit, ids = state$ids)
  }
  theta_draws <- fit$theta_draws
  if (!is.matrix(theta_draws) || !is.numeric(theta_draws)) {
    rlang::abort("`fit$theta_draws` must be a numeric matrix.")
  }
  if (nrow(theta_draws) < 2L) {
    rlang::abort("`fit$theta_draws` must have at least two draws.")
  }

  theta_summary <- .adaptive_theta_summary_from_fit(fit, state)
  theta_mean <- stats::setNames(theta_summary$theta_mean, theta_summary$item_id)
  ranking_ids <- compute_ranking_from_theta_mean(theta_mean, state)
  S_subset <- as.integer(config$S_subset)
  if (is.na(S_subset) || S_subset < 1L) {
    rlang::abort("`config$S_subset` must be a positive integer.")
  }
  S_subset <- min(S_subset, nrow(theta_summary))

  if (S_subset >= nrow(theta_summary)) {
    theta_subset <- theta_summary
  } else {
    n_top <- floor(S_subset / 3)
    n_bottom <- floor(S_subset / 3)

    top_tbl <- dplyr::arrange(
      theta_summary,
      dplyr::desc(.data$theta_mean),
      .data$item_id
    )
    bottom_tbl <- dplyr::arrange(
      theta_summary,
      .data$theta_mean,
      .data$item_id
    )

    top_ids <- if (n_top > 0L) {
      dplyr::slice_head(top_tbl, n = n_top)$item_id
    } else {
      character()
    }
    bottom_ids <- if (n_bottom > 0L) {
      dplyr::slice_head(bottom_tbl, n = n_bottom)$item_id
    } else {
      character()
    }
    used_ids <- unique(c(as.character(top_ids), as.character(bottom_ids)))

    remaining_n <- as.integer(S_subset - length(used_ids))
    if (remaining_n < 0L) {
      rlang::abort("Stopping subset size exceeded available ids.")
    }

    remaining_tbl <- theta_summary[!theta_summary$item_id %in% used_ids, , drop = FALSE]
    remaining_tbl <- dplyr::arrange(
      remaining_tbl,
      dplyr::desc(.data$theta_sd),
      .data$item_id
    )
    uncertainty_ids <- if (remaining_n > 0L) {
      dplyr::slice_head(remaining_tbl, n = remaining_n)$item_id
    } else {
      character()
    }

    subset_ids <- c(as.character(top_ids), as.character(bottom_ids), as.character(uncertainty_ids))
    subset_ids <- subset_ids[!is.na(subset_ids) & subset_ids != ""]
    subset_ids <- unique(subset_ids)
    if (length(subset_ids) != S_subset) {
      rlang::abort("Stopping subset selection failed to reach `S_subset`.")
    }
    theta_subset <- theta_summary[theta_summary$item_id %in% subset_ids, , drop = FALSE]
  }
  theta_sd_median_S <- stats::median(theta_subset$theta_sd)

  tau <- config$tau_fn(state$N)
  if (!is.numeric(tau) || length(tau) != 1L || !is.finite(tau)) {
    rlang::abort("`config$tau_fn` must return a finite numeric scalar.")
  }
  theta_sd_pass <- is.finite(theta_sd_median_S) && theta_sd_median_S <= tau

  utilities_tbl <- candidates_with_utility %||% tibble::tibble()
  if (!is.data.frame(utilities_tbl)) {
    rlang::abort("`candidates_with_utility` must be a data frame or tibble.")
  }
  utilities_tbl <- tibble::as_tibble(utilities_tbl)

  utility_vals <- numeric()
  if (nrow(utilities_tbl) > 0L) {
    util_col <- NULL
    if ("utility" %in% names(utilities_tbl)) {
      util_col <- "utility"
    } else if ("utility_raw" %in% names(utilities_tbl)) {
      util_col <- "utility_raw"
    }
    if (is.null(util_col)) {
      rlang::abort("`candidates_with_utility` must include `utility` or `utility_raw`.")
    }
    utility_vals <- as.double(utilities_tbl[[util_col]])
    utility_vals <- utility_vals[is.finite(utility_vals)]
  }

  U_top_median <- NA_real_
  if (length(utility_vals) > 0L) {
    K_top <- as.integer(config$K_top)
    if (is.na(K_top) || K_top < 1L) {
      rlang::abort("`config$K_top` must be a positive integer.")
    }
    K_top <- min(K_top, length(utility_vals))
    utility_vals <- sort(utility_vals, decreasing = TRUE)
    U_top_median <- stats::median(utility_vals[seq_len(K_top)])
  }

  U0 <- state$U0
  if (!is.finite(U0) && is.finite(U_top_median)) {
    U0 <- as.double(U_top_median)
  }
  U_abs <- as.double(config$U_abs)
  U_pass <- is.finite(U_top_median) && is.finite(U_abs) && U_top_median <= U_abs

  diagnostics_pass <- state$posterior$diagnostics_pass %||% NA
  if (!is.na(diagnostics_pass)) {
    diagnostics_pass <- as.logical(diagnostics_pass)
    if (length(diagnostics_pass) != 1L || is.na(diagnostics_pass)) {
      rlang::abort("`state$posterior$diagnostics_pass` must be TRUE, FALSE, or NA.")
    }
  }

  total_pairs <- state$N * (state$N - 1L) / 2
  hard_cap_threshold <- ceiling(0.40 * total_pairs)
  n_unique_pairs_seen <- sum(state$pair_count >= 1L)
  hard_cap_reached <- n_unique_pairs_seen >= hard_cap_threshold

  weak_adj_threshold <- as.double(config$rank_weak_adj_threshold)
  weak_adj_frac_max <- as.double(config$rank_weak_adj_frac_max)
  min_adj_prob_threshold <- as.double(config$rank_min_adj_prob)
  min_new_pairs_for_check <- as.integer(config$min_new_pairs_for_check)
  if (is.na(min_new_pairs_for_check) || min_new_pairs_for_check < 1L) {
    rlang::abort("`config$min_new_pairs_for_check` must be a positive integer.")
  }

  adj_probs <- numeric()
  if (length(ranking_ids) > 1L) {
    adj_probs <- vapply(seq_len(length(ranking_ids) - 1L), function(idx) {
      left_id <- ranking_ids[[idx]]
      right_id <- ranking_ids[[idx + 1L]]
      mean(theta_draws[, left_id] > theta_draws[, right_id])
    }, numeric(1L))
  }

  frac_weak_adj <- NA_real_
  min_adj_prob <- NA_real_
  rank_stability_pass <- NA
  if (length(adj_probs) > 0L) {
    weak_adj <- adj_probs < weak_adj_threshold
    frac_weak_adj <- mean(weak_adj)
    min_adj_prob <- min(adj_probs)
    rank_stability_pass <- is.finite(frac_weak_adj) &&
      is.finite(min_adj_prob) &&
      frac_weak_adj <= weak_adj_frac_max &&
      min_adj_prob >= min_adj_prob_threshold
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
  metrics$theta_sd_median_S <- theta_sd_median_S
  metrics$theta_sd_pass <- theta_sd_pass
  metrics$tau <- tau
  metrics$U0 <- U0
  metrics$U_top_median <- U_top_median
  metrics$U_abs <- U_abs
  metrics$U_pass <- U_pass
  metrics$rank_stability_pass <- rank_stability_pass
  metrics$frac_weak_adj <- frac_weak_adj
  metrics$min_adj_prob <- min_adj_prob
  metrics$weak_adj_threshold <- weak_adj_threshold
  metrics$weak_adj_frac_max <- weak_adj_frac_max
  metrics$min_adj_prob_threshold <- min_adj_prob_threshold
  metrics$min_new_pairs_for_check <- min_new_pairs_for_check
  metrics$candidate_starved <- as.logical(state$posterior$candidate_starved %||% NA)

  metrics
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

  if (!is.finite(state$U0) && is.finite(metrics$U0)) {
    state$U0 <- as.double(metrics$U0)
  }

  min_comparisons <- state$M1_target %||% NA_integer_
  if (!is.integer(min_comparisons) || length(min_comparisons) != 1L) {
    rlang::abort("`state$M1_target` must be a length-1 integer.")
  }

  min_new_pairs_for_check <- as.integer(config$min_new_pairs_for_check)
  if (is.na(min_new_pairs_for_check) || min_new_pairs_for_check < 1L) {
    rlang::abort("`config$min_new_pairs_for_check` must be a positive integer.")
  }
  comparisons_since_check <- state$comparisons_observed - state$last_check_at
  refit_performed <- isTRUE(metrics$refit_performed)
  check_eligible <- refit_performed || comparisons_since_check >= min_new_pairs_for_check
  if (!isTRUE(check_eligible)) {
    return(list(
      state = state,
      stop_decision = FALSE,
      stop_reason = state$stop_reason %||% NA_character_
    ))
  }
  state$last_check_at <- as.integer(state$comparisons_observed)

  if (!is.na(min_comparisons) && state$comparisons_observed < min_comparisons) {
    state$checks_passed_in_row <- 0L
    return(list(
      state = state,
      stop_decision = FALSE,
      stop_reason = state$stop_reason %||% NA_character_
    ))
  }

  diagnostics_pass <- isTRUE(metrics$diagnostics_pass)
  theta_sd_pass <- isTRUE(metrics$theta_sd_pass)
  U_pass <- isTRUE(metrics$U_pass)
  rank_stability_pass <- isTRUE(metrics$rank_stability_pass)

  if (diagnostics_pass && theta_sd_pass && U_pass && rank_stability_pass) {
    state$checks_passed_in_row <- as.integer(state$checks_passed_in_row + 1L)
  } else {
    state$checks_passed_in_row <- 0L
  }

  stop_decision <- isTRUE(state$checks_passed_in_row >= config$checks_passed_target)
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
